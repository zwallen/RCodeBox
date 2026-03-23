#' Plot a compositional (normalized stacked) barplot
#'
#' @description
#' This function creates a normalized stacked bar plot reporting frequencies of
#' a categorical variable grouped by a specified grouping variable.
#'
#' @param df
#' Dataframe containing the grouping variable and categorical variable of interest.
#' Should be in a "long" format where each row is an observation (e.g., what
#' you get when reshaping a data.frame using `tidyr::pivot_longer()`,
#' `reshape2::melt()`, `reshape()`, etc.).
#' @param groups
#' Name of the categorical variable in `df` to group by (i.e., what each bar of
#' the plot will represent).
#' @param column
#' Name of the categorical variable in `df` to be plotted (i.e., how each bar of
#' the plot will be broken up).
#' @param subrows
#' Name of a categorical variable to optionally stratify the plot by.
#' @param subcolumns
#' Name of a categorical variable to optionally group bars by.
#' @param sort_groups
#' Whether or not to sort groups (i.e., bars) by the most prevalent category in
#' the specified column. (default: `TRUE`)
#' @param add_labels
#' Whether or not to add `N (%)` labels to each groups within the bars. Put it
#' as `FALSE` if plotting a lot of bars. (default: `TRUE`)
#' @param ylab
#' Title for the y-axis. (default is to use `Frequency (%)`)
#' @param xlab
#' Title for the x-axis. (default is to use name given to `groups`)
#' @param legendlab
#' Title for the legend. (default is to use name given to `column`)
#' @param color_list
#' Vector of R recognized color strings the length of the number of groups in
#' the variable provided to `column`.
#' @param remove_xaxis_text
#' Whether to remove the x-axis text and tick marks. Useful for when there are
#' many bars being plotted and showing the x-axis text is not feasible.
#' (default: FALSE)
#' @param flip_plot
#' Whether to flip the plot so bars are now horizontal. (default: FALSE)
#' @param keep_caps
#' Vector of character strings to make sure to keep capitalized. The function
#' automatically tries to keep roman numerals capitalized, but any other string
#' needs to be provided here.
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
#' @importFrom grid unit
#' @export
#'
composition_barplot = function(
  df,
  groups,
  column,
  subrows = NULL,
  subcolumns = NULL,
  sort_groups = TRUE,
  add_labels = TRUE,
  ylab = NULL,
  xlab = NULL,
  legendlab = NULL,
  color_list = NULL,
  remove_xaxis_text = FALSE,
  flip_plot = FALSE,
  keep_caps = NULL,
  save = FALSE,
  figwidth = 1000,
  figheight = 1000,
  out_path = "composition_barplot.jpg"
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
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

  # Remove any missing observations
  if (!is.null(subrows) & !is.null(subcolumns)) {
    plot_df = df[
      rowSums(is.na(df[, c(groups, column, subrows, subcolumns)])) == 0,
    ]
  } else if (!is.null(subrows) & is.null(subcolumns)) {
    plot_df = df[rowSums(is.na(df[, c(groups, column, subrows)])) == 0, ]
  } else if (is.null(subrows) & !is.null(subcolumns)) {
    plot_df = df[rowSums(is.na(df[, c(groups, column, subcolumns)])) == 0, ]
  } else {
    plot_df = df[rowSums(is.na(df[, c(groups, column)])) == 0, ]
  }

  # Detect most prevalent category to plot and sort on its prevalence
  if (sort_groups) {
    top_cat = names(sort(table(plot_df[[column]]), decreasing = TRUE))[1]
    cat_order = names(sort(
      table(plot_df[[groups]], plot_df[[column]])[,
        top_cat
      ],
      decreasing = flip_plot
    ))
    plot_df[[groups]] = factor(plot_df[[groups]], levels = cat_order)
  } else {
    plot_df[[groups]] = factor(
      plot_df[[groups]],
      levels = names(table(df[[groups]]))
    )
  }

  # Make sure levels of column are the same as input
  plot_df[[column]] = factor(
    plot_df[[column]],
    levels = names(table(df[[column]]))
  )

  # Create color vector for plotting if one was not provided
  if (is.null(color_list)) {
    color_list = RColorBrewer::brewer.pal(
      length(levels(plot_df[[column]])),
      "Set2"
    )
  }

  # Perform plotting
  g = ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data[[groups]],
      fill = .data[[column]],
      color = .data[[column]]
    )
  ) +
    ggplot2::geom_bar(position = "fill") +
    ggplot2::scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
    ggplot2::scale_fill_manual(
      labels = stringr::str_wrap(
        sapply(levels(plot_df[[column]]), function(x) {
          format_string(x, keep_caps)
        }),
        width = 20
      ),
      values = color_list
    ) +
    ggplot2::scale_color_manual(
      labels = stringr::str_wrap(
        sapply(levels(plot_df[[column]]), function(x) {
          format_string(x, keep_caps)
        }),
        width = 20
      ),
      values = color_list
    ) +
    ggplot2::labs(
      x = ifelse(is.null(xlab), format_string(groups, keep_caps), xlab),
      y = ifelse(is.null(ylab), "Frequency (%)", ylab),
      fill = stringr::str_wrap(
        ifelse(is.null(legendlab), format_string(column, keep_caps), legendlab),
        width = 15
      )
    ) +
    ggplot2::guides(color = "none") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(color = "black", linewidth = 0.5),
      legend.key.spacing.y = grid::unit(0.2, "lines")
    )
  if (!add_labels) {
    g = g +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
      )
  }

  # Add lables if specified
  if (add_labels) {
    g = g +
      ggplot2::geom_text(
        ggplot2::aes(
          label = paste0(
            ggplot2::after_stat(count),
            " (",
            scales::percent(
              ggplot2::after_stat(count) / tapply(count, x, sum)[x]
            ),
            ")"
          )
        ),
        stat = "count",
        position = ggplot2::position_fill(vjust = 0.5),
        color = "black"
      )
    g$layers[[1]]$aes_params$colour = "black"
  }

  # Add bar groupings if specified
  if (!is.null(subrows) & !is.null(subcolumns)) {
    g = g +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data[[subrows]]),
        cols = ggplot2::vars(.data[[subcolumns]]),
        scales = "free",
        space = "free"
      )
  }
  if (!is.null(subrows) & is.null(subcolumns)) {
    g = g +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data[[subrows]]),
        scales = "free",
        space = "free"
      )
  }
  if (is.null(subrows) & !is.null(subcolumns)) {
    g = g +
      ggplot2::facet_grid(
        cols = ggplot2::vars(.data[[subcolumns]]),
        scales = "free",
        space = "free"
      )
  }

  # Remove x-axis text and tick marks if specified
  if (remove_xaxis_text) {
    g = g +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      )
  }

  # Flip plot if specified
  if (flip_plot) {
    g = g +
      ggplot2::coord_flip() +
      ggplot2::theme(panel.spacing = grid::unit(1, "lines"))
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
