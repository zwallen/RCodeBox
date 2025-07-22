#' Create a Stratified Violin Box Plot
#'
#' Creates a violin box plot showing the distribution of a numerical variable across
#' different strata, including an 'All cases' comparison group.
#'
#' @param data A data.frame containing the variables of interest.
#' @param var Character string. The name of the numerical variable in \code{data}
#'   to be plotted on the y-axis.
#' @param strata Character string. The name of the categorical variable in \code{data}
#'   to stratify by, displayed on the x-axis.
#' @param ylab Character string. The title for the y-axis, describing the numerical
#'   variable.
#' @param xlab Character string, optional. The title for the x-axis. If not provided,
#'   it defaults to the name of the \code{strata} variable with underscores replaced
#'   by spaces.
#'
#' @return A ggplot object containing the stratified violin box plot with:
#'   \itemize{
#'     \item Violin box plot showing distribution of \code{var} across \code{strata}
#'       categories as data densities (violin plot) and the median + interquartile
#'       range (box plot)
#'     \item The mean and 95% confidence interval for each stratum displayed as a red
#'       point range
#'     \item An 'All cases' group for comparison
#'     \item Median values displayed as text labels on the plot
#'   }
#'
#' @details
#' Missing values in \code{var} are automatically filtered out before plotting.
#' An error will occur if no data is left after filtering. The \code{strata} variable
#' is converted to a factor if it is not already.
#'
#' @examples
#' # Create sample data
#' set.seed(123)
#' sample_data <- data.frame(
#'   patient_id = 1:200,
#'   age = rnorm(200, mean = 45, sd = 15),
#'   treatment = factor(sample(c("Drug A", "Drug B", "Placebo"), 200, replace = TRUE)),
#'   hospital = factor(sample(c("Site 1", "Site 2", "Site 3"), 200, replace = TRUE)),
#'   score = rnorm(200, mean = 75, sd = 20)
#' )
#'
#' # Basic usage
#' p1 <- plot_stratified_violin_boxplot(
#'   data = sample_data,
#'   var = "age",
#'   strata = "treatment",
#'   ylab = "Patient Age (years)"
#' )
#' print(p1)
#'
#' # With custom x-axis label
#' p2 <- plot_stratified_violin_boxplot(
#'   data = sample_data,
#'   var = "score",
#'   strata = "hospital",
#'   ylab = "Test Score",
#'   xlab = "Study Site"
#' )
#' print(p2)
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom stats median
#'
#' @export

plot_stratified_violin_boxplot <- function(data, var, strata, ylab, xlab = NULL) {
  # Check var is numerical
  if (!is.numeric(data[[var]])) {
    stop("The variable supplied to 'var' must be numeric.")
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

  # Filter out missing values for var
  plot_data <- data[!is.na(data[[var]]), ]
  if (nrow(plot_data) == 0) {
    stop(paste(
      "No data available for 'var' and 'strata' after filtering",
      "out missing values of 'var'."
    ))
  }

  # Add data for an all cases group
  all_cases_data <- plot_data
  all_cases_data[[strata]] <- "All cases"

  plot_data <- rbind(plot_data, all_cases_data)

  # Ensure strata categories has proper ordering with "All cases" first
  strata_levels <- c("All cases", levels(data[[strata]]))
  plot_data[[strata]] <- factor(
    plot_data[[strata]],
    levels = strata_levels, ordered = TRUE
  )

  # Calculate median for each stratum
  summ_stats <- plot_data |>
    group_by(across(all_of(strata))) |>
    summarise(median_val = median(.data[[var]], na.rm = TRUE), .groups = "drop") |>
    mutate(median_lab = paste0("Median=", round(median_val, 0)))

  # Generate violin boxplot
  if (is.null(xlab)) {
    xlab <- gsub("_", " ", strata)
  }

  # Set seed for reproducible jittering
  set.seed(1234)

  # Calculate y-axis limits
  y_max <- max(plot_data[[var]], na.rm = TRUE)
  y_min <- min(plot_data[[var]], na.rm = TRUE)
  y_range <- y_max - y_min

  g <- ggplot(data = plot_data, aes(y = .data[[var]], x = .data[[strata]])) +
    geom_violin(scale = "width", fill = "lightgrey", color = "black") +
    geom_boxplot(
      width = 0.5, fill = "white", color = "black",
      outlier.size = 0, outlier.alpha = 0
    ) +
    geom_jitter(width = 0.05, color = "black", size = 1, alpha = 0.6) +
    geom_text(
      data = summ_stats,
      aes(y = y_min - 0.05 * y_range, label = median_lab, x = .data[[strata]]),
      size = 3, inherit.aes = FALSE
    ) +
    stat_summary(
      fun.data = "mean_cl_normal", geom = "pointrange",
      color = "red", size = 0.8
    ) +
    coord_cartesian(ylim = c(y_min - 0.1 * y_range, y_max + 0.05 * y_range)) +
    labs(x = xlab, y = ylab) +
    theme_bw() +
    theme(
      text = element_text(size = 10),
      axis.text.x = element_text(angle = 20, hjust = 1)
    )

  return(g)
}
