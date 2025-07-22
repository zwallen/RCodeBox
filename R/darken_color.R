#' Darken colors
#'
#' This function takes one or more colors and darkens them by a specified amount.
#'
#' @param colors A character vector of color specifications (e.g., hex codes, color
#'   names).
#' @param amount A numeric value between 0 and 1 specifying the amount of darkening.
#'   Default is 0.2.
#'
#' @return A character vector of darkened colors in hex format.
#'
#' @details
#' The function performs the following operations:
#' \enumerate{
#'   \item Converts input colors to RGB values
#'   \item Applies darkening by reducing brightness
#'   \item Converts darkened RGB values to hex color codes
#' }
#'
#' @note This function requires the \code{grDevices} package for color conversions.
#'
#' @examples
#' darken_color("red")
#' darken_color(c("#FF0000", "#00FF00", "#0000FF"), amount = 0.3)
#'
#' @importFrom grDevices col2rgb rgb
#'
#' @export

darken_color <- function(colors, amount = 0.2) {
  # Convert the colors to RGB matrix
  rgb_vals <- col2rgb(colors) / 255

  # Apply the darkening to each color by reducing brightness
  darkened_rgb <- rgb_vals * (1 - amount)

  # Convert back to hexadecimal colors, preserving the structure
  darkened_colors <- apply(darkened_rgb, 2, function(x) {
    rgb(x[1], x[2], x[3], maxColorValue = 1)
  })

  return(darkened_colors)
}
