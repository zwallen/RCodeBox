#' Plot a stratified barplot with or without pairwise statistical testing
#'
#' @description
#' This function creates a bar plot reporting frequencies of a categorical
#' variable grouped by a specified grouping variable. It can also test for
#' significant differences between groups using Fisher's exact test, annotating
#' significant comparisons in the plot.
#'
#' @param df
#' Dataframe containing the grouping variable and categorical variable of interest.
#' @param column
#' Name of the categorical variable in `df` to be plotted.
#' @param groups
#' Name of the categorical variable in `df` to group by. If not supplied, then
#' a non-stratified boxplot will be plotted.
#' @param ylab
#' Title for the y-axis. (default is to use `Frequency (%)`)
#' @param xlab
#' Title for the x-axis. (default is to use name given to `groups`)
#' @param legendlab
#' Title for the legend. (default is to use name given to `column`)
#' @param color_list
#' Vector of R recognized color strings the length of the number of groups in
#' the variable provided to `column`.
#' @param test
#' Whether to perform pairwise statistical testing. (default: `TRUE`)
#' @param multi_test_correct
#' Which method to use for multiple testing correction. Can specify any methods
#' that are accepted by the `p.adjust` function. (default is to perform no
#' correction)
#' @param alpha
#' P-value threshold for significance. (default: 0.05)
#' @param drop_all_cases
#' Whether or not to drop the "All Cases" group for times when having this group
#' included in the plot does not make sense (i.e., when not plotting individual
#' or case-level data). (default: FALSE)
#' @param drop_level
#' A level in the variable specified to `column` to drop from the plot. This is
#' ignored if pairwise testing is performed (would be misleading to drop levels
#' that were tested).
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
#' @importFrom ggsignif geom_signif
#' @importFrom stats fisher.test p.adjust
#' @importFrom grid unit
#' @export
#'
stratified_barplot = function(
  df,
  column,
  groups = NULL,
  ylab = NULL,
  xlab = NULL,
  legendlab = NULL,
  color_list = NULL,
  test = TRUE,
  multi_test_correct = NULL,
  alpha = 0.05,
  drop_all_cases = FALSE,
  drop_level = NULL,
  save = FALSE,
  figwidth = 1000,
  figheight = 1000,
  out_path = "stratified_barplot.jpg"
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }
  if (!requireNamespace("ggsignif", quietly = TRUE)) {
    stop("Package 'ggsignif' is required.")
  }

  # Perform a few data checks
  if (!(column %in% colnames(df))) {
    stop("ERROR: column variable name was not found in df")
  }
  if (!(is.factor(df[[column]]) | is.character(df[[column]]))) {
    stop("ERROR: column variable is not a factor or character variable")
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
  #n_cols = length(unique(na.omit(df[[column]]))) *
  #  (length(unique(na.omit(df[[groups]]))) + 1)
  #if (n_cols > ...) {
  #  stop(paste0(
  #    "ERROR: number of columns in plot will be ",
  #    n_cols,
  #    ", which is too many to include in this type of plot"
  #  ))
  #}

  if (!is.null(groups)) {
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
      perc = as.vector(perc),
      label = unlist(as.vector(n_perc))
    )

    # Remove levels with frequency of 0
    plot_df = plot_df[plot_df[["perc"]] > 0, ]

    # Perform pairwise statistical testing
    if (test) {
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
            res = tryCatch(
              fisher.test(n[, c(group1, group2)]),
              error = function(e) {
                fisher.test(n[, c(group1, group2)], simulate.p.value = TRUE)
              }
            )
            if (is.na(res$p.value)) {
              res = fisher.test(n[, c(group1, group2)], simulate.p.value = TRUE)
            }
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
      plot_annot = data.frame()
    }
  } else {
    plot_df = data.frame()
    plot_annot = data.frame()
  }

  # Calculate counts and frequencies for all cases and add to plot data
  if (is.null(groups) | !drop_all_cases) {
    n = table(df[[column]])
    perc = round(n / sum(n) * 100, 1)
    n_perc = paste0(n, " (", perc, "%)")
    plot_df = rbind(
      data.frame(
        Var1 = names(n),
        Var2 = "All Cases",
        Freq = as.vector(n),
        perc = as.vector(perc),
        label = n_perc
      ),
      plot_df
    )
  }

  # Make sure levels of grouping variable and column are the same as input
  plot_df[["Var1"]] = factor(
    plot_df[["Var1"]],
    levels = names(table(df[[column]]))
  )
  if (!is.null(groups)) {
    if (!drop_all_cases) {
      plot_df[["Var2"]] = factor(
        plot_df[["Var2"]],
        levels = c("All Cases", names(table(df[[groups]])))
      )
    } else {
      plot_df[["Var2"]] = factor(
        plot_df[["Var2"]],
        levels = names(table(df[[groups]]))
      )
    }
  }

  # Create color vector for plotting if one was not provided
  if (is.null(color_list)) {
    color_list = RColorBrewer::brewer.pal(
      length(levels(plot_df[["Var1"]])),
      "Set2"
    )
  }

  # Drop any specified levels
  if (!is.null(drop_level) & !test) {
    plot_df = plot_df[!(plot_df[["Var1"]] %in% drop_level), ]
    plot_df[["Var1"]] = droplevels(plot_df[["Var1"]])
  }

  # Perform plotting
  ymax = max(
    plot_df[["perc"]] + (nchar(plot_df[["label"]]) * 1.5),
    na.rm = TRUE
  )
  x_variable = ifelse(!is.null(groups), "Var2", "Var1")
  g = ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      y = .data[["perc"]],
      x = .data[[x_variable]],
      fill = .data[["Var1"]]
    )
  ) +
    ggplot2::geom_col(
      stat = "identity",
      position = "dodge",
      color = "black"
    ) +
    ggplot2::geom_label(
      ggplot2::aes(label = .data[["label"]], group = .data[["Var1"]]),
      position = ggplot2::position_dodge(0.9),
      hjust = -0.05,
      angle = 90,
      fill = "white",
      border.color = "white",
      alpha = 0.75
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "%"),
      breaks = seq(0, 100, 10),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::scale_x_discrete(
      labels = paste0(
        stringr::str_wrap(
          sapply(levels(plot_df[[x_variable]]), function(x) x),
          width = 15
        ),
        "\n(N=",
        sapply(levels(plot_df[[x_variable]]), function(x) {
          sum(plot_df[["Freq"]][plot_df[[x_variable]] == x])
        }),
        ")"
      )
    ) +
    ggplot2::scale_fill_manual(
      labels = stringr::str_wrap(
        sapply(levels(plot_df[["Var1"]]), function(x) x),
        width = 20
      ),
      values = color_list
    ) +
    ggplot2::coord_cartesian(ylim = c(0, ymax), clip = "off") +
    ggplot2::labs(
      x = ifelse(is.null(xlab), groups, xlab),
      y = ifelse(is.null(ylab), "Frequency (%)", ylab),
      fill = stringr::str_wrap(
        ifelse(is.null(legendlab), column, legendlab),
        width = 15
      )
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(color = "black", linewidth = 0.5),
      legend.key.spacing.y = grid::unit(0.2, "lines")
    )
  if (is.null(groups) | (!is.null(drop_level) & !test)) {
    g = g +
      ggplot2::scale_x_discrete(
        labels = stringr::str_wrap(
          sapply(levels(plot_df[["Var1"]]), function(x) x),
          width = 15
        )
      ) +
      ggplot2::guides(fill = "none")
  }

  # Add statistical testing annotations if requested
  if (!is.null(groups) & test & nrow(plot_annot) > 0) {
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
            xmin = .data[["Group1"]],
            xmax = .data[["Group2"]],
            y_position = y_position,
            annotations = paste0("p=", formatC(.data[["p"]], digits = 1))
          ),
          manual = TRUE,
          tip_length = 0
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
