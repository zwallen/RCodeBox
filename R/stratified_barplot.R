#' Plot a stratified barplot with or without pairwise statistical testing
#'
#' @description
#' This function creates a bar plot reporting frequencies of a categorical
#' variable grouped by a specified grouping variable. It can also test for
#' significant differences between groups using Fisher's exact test or Chi-squared
#' test, annotating significant comparisons in the plot.
#'
#' @param df
#' Dataframe containing the grouping variable and categorical variable of interest.
#' @param groups
#' Name of the categorical variable in `df` to group by.
#' @param column
#' Name of the categorical variable in `df` to be plotted.
#' @param ylab
#' Title for the y-axis. (default is to use `Frequency (%)`)
#' @param xlab
#' Title for the x-axis. (default is to use name given to `groups`)
#' @param legendlab
#' Title for the legend. (default is to use name given to `column`)
#' @param colors
#' Vector of R recognized color strings the length of the number of groups in
#' the variable provided to `column`.
#' @param test
#' Which test to use for group comparison: `fisher.test`, `chisq.test`, or `NULL`
#' (no testing performed). (default: `fisher.test`)
#' @param multi_test_correct
#' Which method to use for multiple testing correction. Can specify any methods
#' that are accepted by the `p.adjust` function. (default is to perform no
#' correction)
#' @param alpha
#' P-value threshold for significance. (default: 0.05)
#' @param save
#' Whether to save the image to file. (default: FALSE)
#' @param figwidth
#' Width of the output image file in pixels (`px`).
#' @param figheight
#' Height of the output image file in pixels (`px`).
#' @param out_path
#' File path for the output image file. Existing files at this path are overwritten.
#' Whatever extension included in the image file name (e.g., jpg, png, pdf) will
#' be the format that is outputted.
#'
#' @return
#' A `ggplot2` figure object. If export is `TRUE`, also exports the image to file.
#'
#' @import ggplot2
#' @importFrom stringr str_split str_to_title
#' @importFrom ggsignif geom_signif
#' @importFrom stats fisher.test chisq.test
#' @importFrom utils combn
#' @export
#'
stratified_barplot = function(
  df,
  groups,
  column,
  ylab = NULL,
  xlab = NULL,
  legendlab = NULL,
  colors = NULL,
  test = "fisher.test",
  multi_test_correct = NULL,
  alpha = 0.05,
  save = FALSE,
  figwidth = 5,
  figheight = 5,
  out_path = "stratified_barplot"
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package 'stringr' is required.")
  }
  if (!requireNamespace("ggsignif", quietly = TRUE)) {
    stop("Package 'ggsignif' is required.")
  }

  # Perform a few data checks
  if (!(groups %in% colnames(df))) {
    stop("ERROR: groups variable name was not found in column names of df")
  }
  if (!(column %in% colnames(df))) {
    stop("ERROR: column variable name was not found in df")
  }
  if (!(is.factor(df[[groups]]) | is.character(df[[groups]]))) {
    stop("ERROR: groups variable is not a factor or character variable")
  }
  if (!(is.factor(df[[column]]) | is.character(df[[column]]))) {
    stop("ERROR: column variable is not a factor or character variable")
  }

  #### Not implemented yet, but should put a check in for how many bars in plot
  #### and give error if there are too many
  #n_bars = length(unique(na.omit(df[[column]]))) *
  #  (length(unique(na.omit(df[[groups]]))) + 1)
  #if (n_bars > ...) {
  #  stop(paste0(
  #    "ERROR: number of columns in plot will be ",
  #    n_bars,
  #    ", which is too many to include in this type of plot"
  #  ))
  #}

  # Define function for pairwise statistical testing
  if (test == "fisher.test") {
    pair.test = function(x) fisher.test(x)
  }
  if (test == "chisq.test") {
    pair.test = function(x) chisq.test(x)
  }

  # Create a small function to capitalize categories more aesthetically
  format_string = function(x) {
    nocaps = "^and$|^or$|^at$|^in$|^of$|^the$|^for$|^by$|^to$|^with$|^Mean\u00B1SD$"
    alwayscaps = "^II$|^III$|^IV$|^V$|^VI$|^VII$|^VIII$|^VIIII$|^X$"
    paste(
      sapply(unlist(stringr::str_split(x, " ")), function(y) {
        ifelse(
          grepl(nocaps, y, ignore.case = TRUE),
          y,
          ifelse(
            grepl(alwayscaps, y, ignore.case = TRUE),
            toupper(y),
            stringr::str_to_title(y)
          )
        )
      }),
      collapse = " "
    )
  }

  # Calculate counts and frequencies for groups
  n = table(df[[column]], df[[groups]])
  perc = apply(n, 2, function(x) round(x / sum(x) * 100, 1))
  n_perc = data.frame(matrix(
    paste0(n, " (", perc, "%)"),
    nrow = nrow(n),
    ncol = ncol(n)
  ))
  plot_df = data.frame(
    data.frame(n),
    Perc = as.vector(perc),
    Label = unlist(as.vector(n_perc))
  )

  # Perform pairwise statistical testing
  plot_annot = data.frame()
  for (group1 in colnames(n)) {
    for (group2 in colnames(n)) {
      if (
        group1 != group2 &
          !(paste(group1, group2) %in%
            paste(plot_annot[["Group1"]], plot_annot[["Group2"]])) &
          !(paste(group1, group2) %in%
            paste(plot_annot[["Group2"]], plot_annot[["Group1"]]))
      ) {
        res = pair.test(n[, c(group1, group2)])
        plot_annot = rbind(
          plot_annot,
          data.frame(
            Group1 = group1,
            Group2 = group2,
            p = res$p.value
          )
        )
      }
    }
  }

  # Perform multiple testing correction if specified
  if (!is.null(multi_test_correct)) {
    plot_annot[["p"]] = p.adjust(
      plot_annot[["p"]],
      method = multi_test_correct
    )
  }

  # Calculate counts and frequencies for all cases and add to plot data
  n = table(df[[column]])
  perc = round(n / sum(n) * 100, 1)
  n_perc = paste0(n, " (", perc, "%)")
  plot_df = rbind(
    data.frame(
      Var1 = names(n),
      Var2 = "All Cases",
      Freq = as.vector(n),
      Perc = as.vector(perc),
      Label = n_perc
    ),
    plot_df
  )

  # Make sure levels of grouping variable and column are the same as input
  plot_df[["Var1"]] = factor(
    plot_df[["Var1"]],
    levels = names(table(df[[column]]))
  )
  plot_df[["Var2"]] = factor(
    plot_df[["Var2"]],
    levels = c("All Cases", names(table(df[[groups]])))
  )

  # Create color vector for plotting if one was not provided
  if (is.null(colors)) {
    colors = RColorBrewer::brewer.pal(
      length(unique(plot_df[["Var1"]])),
      "Set2"
    )
  }

  # Perform plotting
  ymax = max(plot_df[["Perc"]] + nchar(plot_df[["Label"]]))
  g = ggplot2::ggplot(plot_df, ggplot2::aes(y = Perc, x = Var2, fill = Var1)) +
    ggplot2::geom_col(
      stat = "identity",
      position = "dodge",
      color = "black"
    ) +
    ggplot2::geom_label(
      ggplot2::aes(label = Label, group = Var1),
      position = ggplot2::position_dodge(0.9),
      hjust = -0.1,
      angle = 90,
      fill = "white",
      border.color = "white",
      alpha = 0.75
    ) +
    ggplot2::scale_y_continuous(breaks = seq(0, 100, 10)) +
    ggplot2::scale_x_discrete(
      labels = stringr::str_wrap(
        sapply(unique(plot_df[["Var2"]]), function(x) format_string(x)),
        width = 15
      )
    ) +
    ggplot2::scale_fill_manual(
      labels = stringr::str_wrap(
        sapply(unique(plot_df[["Var1"]]), function(x) {
          format_string(x)
        }),
        width = 20
      ),
      values = colors
    ) +
    ggplot2::coord_cartesian(ylim = c(0, ymax), clip = "off") +
    ggplot2::labs(
      x = ifelse(is.null(xlab), format_string(groups), xlab),
      y = ifelse(is.null(ylab), "Frequency (%)", ylab),
      fill = stringr::str_wrap(
        ifelse(is.null(legendlab), format_string(column), legendlab),
        width = 15
      )
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(color = "black", linewidth = 0.5)
    )

  # Add statistical testing annotations if requested
  if (!is.null(test) & nrow(plot_annot) > 0) {
    plot_annot = plot_annot[plot_annot[["p"]] < alpha, ]
    if (nrow(plot_annot) > 0) {
      y_position = sapply(1:nrow(plot_annot), function(x) {
        ymax + (x * (ymax / 10))
      })
      g = g +
        ggsignif::geom_signif(
          inherit.aes = FALSE,
          data = plot_annot,
          ggplot2::aes(
            xmin = Group1,
            xmax = Group2,
            y_position = y_position,
            annotations = paste0("p=", formatC(p, digits = 1))
          ),
          manual = TRUE
        ) +
        ggplot2::coord_cartesian(ylim = c(0, max(y_position)), clip = "off")
      g$layers = g$layers[c("geom_col", "geom_signif", "geom_label")]
    }
  }

  # Save plot to file if requested
  if (save) {
    ggplot2::ggsave(
      out_path,
      g,
      width = figwidth,
      height = figheight,
      units = "px"
    )
  }

  return(g)
}
