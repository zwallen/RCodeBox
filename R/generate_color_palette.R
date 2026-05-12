#' Generate a color palette for plotting
#'
#' @description
#' Creates a color palette from RColorBrewer. For palettes with 12 or fewer
#' colors, returns the exact palette colors. For larger palettes, interpolates
#' additional colors using a color ramp.
#'
#' @param n
#' Number of colors needed in the palette.
#' @param palette
#' Name of an RColorBrewer palette. (default: "Set3")
#'
#' @return
#' A character vector of hex color codes of length `n`.
#'
#' @export
#'
generate_color_palette <- function(n, palette = "Set3") {
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    stop("Package 'RColorBrewer' is required.")
  }

  # Validate input
  if (!is.numeric(n) || n < 1) {
    stop("ERROR: n must be a positive integer")
  }

  # Get palette information
  palette_info <- RColorBrewer::brewer.pal.info[palette, ]
  if (is.null(palette_info)) {
    stop(paste0("ERROR: palette '", palette, "' not found in RColorBrewer"))
  }
  max_colors <- palette_info[["maxcolors"]]

  # Generate color palette
  if (n <= max_colors) {
    # Use colors directly from the palette
    colors <- RColorBrewer::brewer.pal(max(3, n), palette)[seq_len(n)]
  } else {
    # Interpolate additional colors using color ramp
    colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(
      max_colors,
      palette
    ))(n)
  }

  colors
}
