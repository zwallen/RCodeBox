#' Create a pie chart with automatic labeling
#'
#' @description
#' This function creates a pie chart for a categorical variable, automatically
#' calculating counts and percentages. Labels are placed directly on slices that
#' represent at least 5% of the total, while smaller slices are labeled with
#' repelled labels to avoid overlap.
#'
#' @param df
#' Dataframe containing the categorical variable and count column.
#' @param column
#' Name of the categorical variable in `df` to create the pie chart for.
#' @param count_column
#' Name of the column in `df` to use for counting. Each unique value should
#' represent one observation/case.
#' @param repel_label_threshold
#' Frequency threshold for pushing labels outside of the pie chart. Any slices
#' of the pie chart less than this will have its labels repelled outside of the
#' pie chart area.
#' @param legendlab
#' Title for the legend. (default is to use name given to `column`)
#' @param color_list
#' Vector of R recognized color strings the length of the number of categories in
#' the variable provided to `column`. If NULL, uses RColorBrewer Set3 palette.
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
#' @importFrom stats aggregate as.formula
#' @export
#'
pie_chart <- function(
  df,
  column,
  count_column,
  repel_label_threshold = 0.05,
  legendlab = NULL,
  color_list = NULL,
  save = FALSE,
  figwidth = 1000,
  figheight = 1000,
  out_path = "pie_chart.jpg"
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    stop("Package 'ggrepel' is required.")
  }

  # Perform a few data checks
  if (!(column %in% colnames(df))) {
    stop("ERROR: column variable name was not found in df")
  }
  if (!(count_column %in% colnames(df))) {
    stop("ERROR: count_column variable name was not found in df")
  }
  if (!(is.factor(df[[column]]) || is.character(df[[column]]))) {
    stop("ERROR: column variable is not a factor or character variable")
  }

  # Prepare data for plotting
  formula_str <- as.formula(paste(count_column, "~", column))
  plot_df <- aggregate(formula_str, data = df, FUN = length)
  colnames(plot_df)[ncol(plot_df)] <- "count"

  plot_df[["freq"]] <- plot_df[["count"]] / sum(plot_df[["count"]])
  plot_df[["label"]] <- paste0(
    plot_df[["count"]],
    " (",
    round(plot_df[["freq"]] * 100, 1),
    "%)"
  )
  plot_df[["label_y"]] <- sum(plot_df[["count"]]) -
    (cumsum(plot_df[["count"]]) - plot_df[["count"]] / 2)

  # Create color vector for plotting if one was not provided
  if (is.null(color_list)) {
    color_list <- generate_color_palette(length(unique(plot_df[[column]])))
  }

  # Perform plotting
  g <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = 1, y = .data[["count"]], fill = .data[[column]])
  ) +
    ggplot2::geom_col(width = 1, color = "black") +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::scale_fill_manual(
      name = stringr::str_wrap(
        ifelse(is.null(legendlab), column, legendlab),
        width = 15
      ),
      labels = stringr::str_wrap(levels(plot_df[[column]]), width = 15),
      values = color_list
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = c(0, 0.5),
      legend.key.spacing.y = grid::unit(0.2, "lines"),
      plot.margin = ggplot2::margin(l = 75, unit = "pt")
    )

  # Add labels for slices >= defined frequency threshold
  if (any(plot_df[["freq"]] >= repel_label_threshold)) {
    g <- g +
      ggplot2::geom_label(
        data = plot_df[plot_df[["freq"]] >= repel_label_threshold, ],
        inherit.aes = FALSE,
        ggplot2::aes(x = 1, y = .data[["label_y"]], label = .data[["label"]]),
        fill = "white",
        size = 5,
        alpha = 0.8
      )
  }

  # Add repelled labels for slices < defined threshold
  if (any(plot_df[["freq"]] < repel_label_threshold)) {
    g <- g +
      ggrepel::geom_label_repel(
        data = plot_df[plot_df[["freq"]] < repel_label_threshold, ],
        inherit.aes = FALSE,
        ggplot2::aes(x = 1.5, y = .data[["label_y"]], label = .data[["label"]]),
        min.segment.length = 0,
        nudge_x = 0.2,
        size = 5,
        alpha = 0.8,
        seed = 1234
      )
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

  g
}
