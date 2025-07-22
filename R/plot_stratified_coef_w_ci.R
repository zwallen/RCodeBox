#' Create a Stratified Coefficient Plot with Confidence Intervals
#'
#' Creates a coefficient plot showing the estimated coefficients and confidence
#' intervals of a numerical variable across different strata.
#'
#' @param data A data.frame in "long" format that contains columns denoting the names
#'   of the tested variables (\code{var}), groups tested (\code{strata}), coefficients
#'   (\code{coef}), and the lower (\code{lower}) and upper (\code{upper}) limits of
#'   the coefficient confidence interval. Optionally, it can also contain p-values
#'   (\code{pvalue}) for each coefficient.
#' @param var Character string. The column in \code{data} denoting names of tested
#'   variables to be plotted on the y-axis. The plot will be faceted by this variable.
#' @param strata Character string. The name of the categorical variable in \code{data}
#'   to stratify by, displayed as different color points and ranges.
#' @param coef Character string. The column in \code{data} denoting the estimated
#'   coefficients for each stratum of \code{strata} and variable of \code{var}.
#' @param lower Character string. The column in \code{data} denoting the lower limit
#'   of the confidence interval for each coefficient.
#' @param upper Character string. The column in \code{data} denoting the upper limit
#'   of the confidence interval for each coefficient.
#' @param fill_color Named character vector or character vector. Color specification
#'   for the points and ranges. Can be a named vector mapping variable categories to
#'   colors, or a character vector of colors.
#' @param xlab Character string. The title for the x-axis describing the coefficients
#'   being plotted.
#' @param pvalue Character string, optional. The column in \code{data} denoting the
#'   p-values for each coefficient. If provided, p-values will be displayed as text
#'   labels on the plot. If not provided, no p-values will be displayed.
#'
#' @return A ggplot object containing the coefficient plot with:
#'   \itemize{
#'     \item Coefficients displayed as points with error bars representing confidence
#'       intervals
#'     \item Faceted by variable for multiple comparisons
#'     \item A vertical dashed line at x=0 for reference
#'     \item P-values displayed as text labels on the plot if provided
#'   }
#'
#' @details
#' The function expects data in long format where each row represents one
#' coefficient estimate. Both \code{var} and \code{strata} variables are converted
#' to factors if they are not already. All coefficient-related columns (\code{coef},
#' \code{lower}, \code{upper}, and optionally \code{pvalue}) must be numeric.
#'
#' @examples
#' # Create sample coefficient data
#' set.seed(123)
#' coef_data <- data.frame(
#'   variable = rep(c("Age", "BMI", "Blood Pressure"), each = 3),
#'   group = rep(c("Treatment A", "Treatment B", "Control"), 3),
#'   estimate = c(0.15, 0.22, 0.08, -0.05, 0.12, 0.03, 0.25, 0.18, 0.10),
#'   ci_lower = c(0.05, 0.10, -0.02, -0.15, 0.02, -0.07, 0.15, 0.08, 0.00),
#'   ci_upper = c(0.25, 0.34, 0.18, 0.05, 0.22, 0.13, 0.35, 0.28, 0.20),
#'   p_value = c(0.003, 0.001, 0.12, 0.45, 0.02, 0.68, 0.0001, 0.001, 0.05)
#' )
#'
#' # Basic usage without p-values
#' p1 <- plot_stratified_coef_w_ci(
#'   data = coef_data,
#'   var = "variable",
#'   strata = "group",
#'   coef = "estimate",
#'   lower = "ci_lower",
#'   upper = "ci_upper",
#'   fill_color = c("Treatment A" = "blue", "Treatment B" = "red", "Control" = "gray"),
#'   xlab = "Effect Size (95% CI)"
#' )
#' print(p1)
#'
#' # With p-values displayed
#' p2 <- plot_stratified_coef_w_ci(
#'   data = coef_data,
#'   var = "variable",
#'   strata = "group",
#'   coef = "estimate",
#'   lower = "ci_lower",
#'   upper = "ci_upper",
#'   fill_color = c("lightblue", "lightcoral", "lightgray"),
#'   xlab = "Standardized Coefficient",
#'   pvalue = "p_value"
#' )
#' print(p2)
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom stats reformulate
#'
#' @export

plot_stratified_coef_w_ci <- function(data, var, strata, coef, lower, upper,
                                      fill_color, xlab, pvalue = NULL) {
  # Check var is factor/character
  if (!is.factor(data[[var]]) && !is.character(data[[var]])) {
    stop(paste(
      "The variable supplied to 'var' must be categorical",
      "(factor or character)."
    ))
  }

  if (!is.factor(data[[var]])) {
    message(paste(
      "The variable supplied to 'var' was not a factor.",
      "Converting to factor with no specific ordering of categories."
    ))
    data[[var]] <- as.factor(data[[var]])
  }

  # Check strata is factor/character
  if (!is.factor(data[[strata]]) && !is.character(data[[strata]])) {
    stop(paste(
      "The variable supplied to 'strata' must be categorical",
      "(factor or character)."
    ))
  }

  if (!is.factor(data[[strata]])) {
    message(paste(
      "The variable supplied to 'strata' was not a factor.",
      "Converting to factor with no specific ordering of categories."
    ))
    data[[strata]] <- as.factor(data[[strata]])
  }

  # Check coef, lower, and upper are numerical
  if (!is.numeric(data[[coef]])) {
    stop("The variable supplied to 'coef' must be numeric.")
  }
  if (!is.numeric(data[[lower]])) {
    stop("The variable supplied to 'lower' must be numeric.")
  }
  if (!is.numeric(data[[upper]])) {
    stop("The variable supplied to 'upper' must be numeric.")
  }
  if (!is.null(pvalue) && !is.numeric(data[[pvalue]])) {
    stop("The variable supplied to 'pvalue' must be numeric.")
  }

  # If pvalue provided, create p-value labels
  if (!is.null(pvalue)) {
    data <- data %>%
      mutate(
        pvalue_labs = case_when(
          .data[[pvalue]] < 0.005 ~ sprintf("P=%.1e", .data[[pvalue]]),
          .data[[pvalue]] >= 0.005 ~ sprintf("P=%.2f", .data[[pvalue]]),
          TRUE ~ ""
        )
      )
  }

  # Handle fill_color
  if (is.null(names(fill_color))) {
    # If fill_color is not named, create names based on strata levels
    strata_levels <- levels(data[[strata]])
    if (length(fill_color) < length(strata_levels)) {
      fill_color <- rep_len(fill_color, length(strata_levels))
    }
    names(fill_color) <- strata_levels[1:length(fill_color)]
  }

  # Generate coefficient plot
  g <- ggplot(
    data = data,
    aes(x = .data[[coef]], y = .data[[strata]], color = .data[[strata]])
  ) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_errorbarh(
      aes(xmin = .data[[lower]], xmax = .data[[upper]]),
      position = position_dodge(0.3),
      height = 0.1,
      size = 1,
      show.legend = FALSE
    ) +
    geom_point(position = position_dodge(0.3), size = 3) +
    scale_fill_manual(values = fill_color) +
    scale_color_manual(values = fill_color) +
    facet_grid(reformulate(".", var)) +
    theme_bw() +
    theme(
      text = element_text(size = 10),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_text(margin = margin(t = 20)),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      strip.text.y = element_text(angle = 0),
      strip.background = element_rect(fill = "lightgrey")
    ) +
    labs(x = xlab)

  # Add p-value labels if provided
  if (!is.null(pvalue)) {
    # Calculate x position for p-value labels
    x_pos <- max(data[[upper]], na.rm = TRUE) + 0.1 *
      (max(data[[upper]], na.rm = TRUE) - min(data[[lower]], na.rm = TRUE))

    g <- g +
      geom_text(
        aes(label = pvalue_labs),
        x = x_pos,
        hjust = 0,
        size = 3,
        show.legend = FALSE,
        nudge_x = 0.02
      ) +
      coord_cartesian(xlim = c(
        min(data[[lower]], na.rm = TRUE),
        max(data[[upper]], na.rm = TRUE) + 0.3 *
          (max(data[[upper]], na.rm = TRUE) - min(data[[lower]], na.rm = TRUE))
      ))
  }

  return(g)
}
