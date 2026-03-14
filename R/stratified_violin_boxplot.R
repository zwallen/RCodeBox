#' Plot a stratified violin-boxplot with or without pairwise statistical testing
#'
#' @description
#' This function creates a violin-box plot with data points and means/stds for a
#' numeric variable grouped by a specified groups variable. It also can test for
#' significant differences between groups using a t-test or Wilcoxon rank-sum
#' test, annotating significant comparisons in the plot.
#'
#' @param df
#' Dataframe containing the grouping variable and numeric variable of interest.
#' @param column
#' Name of the numeric variable in `df` to be plotted.
#' @param groups
#' Name of the categorical variable in `df` to group by. If not supplied, then
#' a non-stratified boxplot will be plotted.
#' @param ylab
#' Title for the y-axis. (default is to use name given to `column`)
#' @param xlab
#' Title for the x-axis. (default is to use name given to `groups`)
#' @param test
#' Which test to use for group comparison: `t.test`, `wilcox.test`, or `NULL`
#' (no testing performed). (default: `wilcox.test`)
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
#' A `ggplot2` figure object. If save is `TRUE`, also exports the image to file.
#'
#' @import ggplot2
#' @importFrom stringr str_split str_to_title
#' @importFrom ggsignif geom_signif
#' @importFrom stats t.test wilcox.test p.adjust
#' @export
#'
stratified_violin_boxplot = function(
  df,
  column,
  groups = NULL,
  ylab = NULL,
  xlab = NULL,
  test = "fisher.test",
  multi_test_correct = NULL,
  alpha = 0.05,
  save = FALSE,
  figwidth = 1000,
  figheight = 1000,
  out_path = "stratified_violin_boxplot.jpg"
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
  if (!(column %in% colnames(df))) {
    stop("ERROR: column variable name was not found in df")
  }
  if (!is.numeric(df[[column]])) {
    stop("ERROR: column variable is not a numeric variable")
  }
  if (!is.null(groups)) {
    if (!(groups %in% colnames(df))) {
      stop("ERROR: groups variable name was not found in column names of df")
    }
    if (!(is.factor(df[[groups]]) | is.character(df[[groups]]))) {
      stop("ERROR: groups variable is not a factor or character variable")
    }
  }

  #### Not implemented yet, but should put a check in for how many columns in plot
  #### and give error if there are too many
  #n_cols = length(unique(na.omit(df[[groups]]))) + 1
  #if (n_cols > ...) {
  #  stop(paste0(
  #    "ERROR: number of columns in plot will be ",
  #    n_cols,
  #    ", which is too many to include in this type of plot"
  #  ))
  #}

  # Define function for pairwise statistical testing
  if (test == "t.test") {
    pair.test = function(x, y) t.test(x, y, var.equal = FALSE)
  }
  if (test == "wilcox.test") {
    pair.test = function(x, y) wilcox.test(x, y)
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

  if (!is.null(groups)) {
    # Calculate average and standard deviations for groups
    n = sapply(names(table(df[[groups]])), function(x) {
      length(na.omit(df[[column]][df[[groups]] == x]))
    })
    avg = sapply(names(table(df[[groups]])), function(x) {
      round(mean(df[[column]][df[[groups]] == x], na.rm = TRUE), 1)
    })
    std = sapply(names(table(df[[groups]])), function(x) {
      round(sd(df[[column]][df[[groups]] == x], na.rm = TRUE), 1)
    })
    avg_std = paste0(avg, "\u00B1", std)
    plot_df = data.frame(
      group = names(avg),
      n = n,
      avg = avg,
      std_start = avg - std,
      std_end = avg + std,
      label = avg_std
    )

    # Perform pairwise statistical testing
    plot_annot = data.frame()
    for (group1 in names(avg)) {
      for (group2 in names(avg)) {
        if (
          group1 != group2 &
            !(paste(group1, group2) %in%
              paste(plot_annot[["Group1"]], plot_annot[["Group2"]])) &
            !(paste(group1, group2) %in%
              paste(plot_annot[["Group2"]], plot_annot[["Group1"]]))
        ) {
          res = pair.test(
            df[[column]][df[[groups]] == group1],
            df[[column]][df[[groups]] == group2]
          )
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
  } else {
    plot_df = data.frame()
    plot_annot = data.frame()
  }

  # Calculate counts and frequencies for all cases and add to plot data
  n = length(na.omit(df[[column]]))
  avg = round(mean(df[[column]], na.rm = TRUE), 1)
  std = round(sd(df[[column]], na.rm = TRUE), 1)
  avg_std = paste0(avg, "\u00B1", std)
  plot_df = rbind(
    data.frame(
      group = "All Cases",
      n = n,
      avg = avg,
      std_start = avg - std,
      std_end = avg + std,
      label = avg_std
    ),
    plot_df
  )

  # Make sure levels of grouping variable are the same as input
  if (!is.null(groups)) {
    plot_df[["group"]] = factor(
      plot_df[["group"]],
      levels = c("All Cases", names(table(df[[groups]])))
    )
  }

  # Need to double the data to get an all case group
  if (!is.null(groups)) {
    group_vec = factor(
      c(rep("All Cases", nrow(df)), as.character(df[[groups]])),
      levels = c("All Cases", names(table(as.character(df[[groups]]))))
    )
    y_vec = c(df[[column]], df[[column]])
  } else {
    group_vec = factor(rep("All Cases", nrow(df)), levels = "All Cases")
    y_vec = df[[column]]
  }

  # Perform plotting
  ymin = min(df[[column]], na.rm = TRUE)
  ymax = max(df[[column]], na.rm = TRUE)
  set.seed(1234)
  g = ggplot2::ggplot(
    data.frame(group = group_vec, y = y_vec),
    ggplot2::aes(y = .data[["y"]], x = .data[["group"]])
  ) +
    ggplot2::geom_violin(na.rm = TRUE, color = "black", fill = "grey") +
    ggplot2::geom_boxplot(
      outliers = FALSE,
      na.rm = TRUE,
      color = "black",
      alpha = 0.75,
      width = 0.75
    ) +
    ggplot2::geom_jitter(na.rm = TRUE, width = 0.1, size = 2) +
    ggplot2::geom_errorbar(
      inherit.aes = FALSE,
      data = plot_df,
      ggplot2::aes(
        ymin = .data[["std_start"]],
        ymax = .data[["std_end"]],
        x = .data[["group"]]
      ),
      color = "red",
      width = 0.1,
      linewidth = 1
    ) +
    ggplot2::geom_label(
      inherit.aes = FALSE,
      data = plot_df,
      ggplot2::aes(
        label = .data[["label"]],
        y = .data[["avg"]],
        x = .data[["group"]]
      ),
      position = ggplot2::position_dodge(0.9),
      fill = "white",
      border.color = "red",
      linewidth = 1
    ) +
    ggplot2::scale_x_discrete(
      labels = paste0(
        stringr::str_wrap(
          sapply(levels(plot_df[["group"]]), function(x) format_string(x)),
          width = 15
        ),
        "\n(N=",
        plot_df[["n"]],
        ")"
      )
    ) +
    ggplot2::coord_cartesian(ylim = c(ymin, ymax), clip = "off") +
    ggplot2::labs(
      x = ifelse(is.null(xlab), format_string(groups), xlab),
      y = ifelse(is.null(ylab), format_string(column), ylab)
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
        ymax + (x * (ymax / 20))
      })
      g = g +
        ggsignif::geom_signif(
          inherit.aes = FALSE,
          data = plot_annot,
          ggplot2::aes(
            xmin = .data[["Group1"]],
            xmax = .data[["Group2"]],
            y_position = y_position,
            annotations = paste0("p=", formatC(.data[["p"]], digits = 1))
          ),
          manual = TRUE,
          tip_length = 0
        ) +
        ggplot2::coord_cartesian(ylim = c(ymin, max(y_position)), clip = "off")
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
