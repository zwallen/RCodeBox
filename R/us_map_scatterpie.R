#' Create a US map with scatter pie charts by state
#'
#' @description
#' This function creates a US map with scatter pie charts positioned at state
#' centroids, showing the composition of a categorical variable for each state.
#' Alaska and Hawaii are repositioned for better visualization.
#'
#' @param df
#' Dataframe containing the categorical variable, state column, and count column.
#' @param column
#' Name of the categorical variable in `df` to visualize in the pie charts.
#' @param state_column
#' Name of the column in `df` containing US state abbreviations (e.g., "CA", "NY").
#' @param count_column
#' Name of the column in `df` to use for counting. Each unique value should
#' represent one observation/case.
#' @param legendlab
#' Title for the legend. (default is to use name given to `column`)
#' @param color_list
#' Vector of R recognized color strings the length of the number of categories in
#' the variable provided to `column`. If NULL, uses RColorBrewer Set3 palette.
#' @param pie_scale
#' Scaling factor for the pie chart sizes. Larger values create bigger pies.
#' (default: 1.75)
#' @param pie_alpha
#' Transparency level for the pie charts, from 0 (fully transparent) to 1
#' (fully opaque). (default: 0.6)
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
us_map_scatterpie <- function(
  df,
  column,
  state_column,
  count_column,
  legendlab = NULL,
  color_list = NULL,
  pie_scale = 1.75,
  pie_alpha = 0.6,
  save = FALSE,
  figwidth = 3000,
  figheight = 3000,
  out_path = "us_map_scatterpie.jpg"
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required.")
  }
  if (!requireNamespace("USAboundaries", quietly = TRUE)) {
    stop("Package 'USAboundaries' is required.")
  }
  if (!requireNamespace("scatterpie", quietly = TRUE)) {
    stop("Package 'scatterpie' is required.")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required.")
  }

  # Perform data checks
  if (!(column %in% colnames(df))) {
    stop("ERROR: column variable name was not found in df")
  }
  if (!(state_column %in% colnames(df))) {
    stop("ERROR: state_column variable name was not found in df")
  }
  if (!(count_column %in% colnames(df))) {
    stop("ERROR: count_column variable name was not found in df")
  }
  if (!(is.factor(df[[column]]) || is.character(df[[column]]))) {
    stop("ERROR: column variable is not a factor or character variable")
  }
  if (!(is.factor(df[[state_column]]) || is.character(df[[state_column]]))) {
    stop("ERROR: state_column variable is not a factor or character variable")
  }

  # Calculate per-state frequencies
  formula_str <- as.formula(paste(count_column, "~", column, "+", state_column))
  plot_df <- aggregate(formula_str, data = df, FUN = length)
  colnames(plot_df)[ncol(plot_df)] <- "count"

  # Calculate frequencies within each state
  plot_df[["freq"]] <- unlist(sapply(
    unique(plot_df[[state_column]]),
    function(x) {
      plot_df[["count"]][plot_df[[state_column]] == x] /
        sum(plot_df[["count"]][plot_df[[state_column]] == x])
    }
  ))

  # Pivot to wide format for scatterpie
  plot_df <- tidyr::pivot_wider(
    plot_df[, colnames(plot_df) != "count"],
    names_from = column,
    values_from = "freq",
    values_fill = 0
  )

  # Get US map geometry and reproject to US Albers Equal Area
  states <- sf::st_transform(USAboundaries::us_states(resolution = "low"), 5070)
  states <- states[
    !(states[["stusps"]] %in% c("PR", "VI", "GU", "MP", "AS")),
    c("stusps", "geometry")
  ]

  # Modify Alaska and Hawaii geometry to position them closer
  sf::st_geometry(states)[states[["stusps"]] == "AK"] <-
    sf::st_geometry(states)[states[["stusps"]] == "AK"] * 0.75 + c(1e6, -4e6)
  sf::st_geometry(states)[states[["stusps"]] == "HI"] <-
    sf::st_geometry(states)[states[["stusps"]] == "HI"] * 1.5 + c(1e7, -2.8e6)

  # Compute state centroids
  state_centroids <- data.frame(
    temp_state = states[["stusps"]],
    sf::st_coordinates(sf::st_centroid(states))
  )
  colnames(state_centroids)[1] <- state_column
  plot_df <- merge(plot_df, state_centroids, by = state_column, all.x = TRUE)

  # Get category names for scatterpie
  category_cols <- names(table(df[[column]]))

  # Create color vector for plotting if one was not provided
  if (is.null(color_list)) {
    color_list <- generate_color_palette(length(category_cols))
  }

  # Perform plotting
  g <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = states,
      fill = "grey",
      color = "white",
      linewidth = 0.2
    ) +
    scatterpie::geom_scatterpie(
      data = plot_df,
      ggplot2::aes(
        x = .data[["X"]],
        y = .data[["Y"]],
        group = .data[[state_column]]
      ),
      cols = category_cols,
      pie_scale = pie_scale,
      alpha = pie_alpha
    ) +
    ggplot2::coord_sf(crs = sf::st_crs(states)) +
    ggplot2::scale_fill_manual(
      name = stringr::str_wrap(
        ifelse(is.null(legendlab), column, legendlab),
        width = 15
      ),
      labels = stringr::str_wrap(category_cols, width = 15),
      values = color_list
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = c(0.1, 0.6),
      legend.title = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 14)
    )

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
