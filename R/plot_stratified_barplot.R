#' Create a stratified bar plot
#'
#' Creates a stratified bar plot showing the frequency distribution of a categorical
#' variable across different strata, including an 'All cases' comparison group.
#'
#' @param data A data.frame containing the variables of interest.
#' @param var Character string. The name of the categorical variable in \code{data}
#'   to be plotted on the y-axis as frequencies.
#' @param strata Character string. The name of the categorical variable in \code{data}
#'   to stratify by, displayed on the x-axis.
#' @param case_id Character string. The name of the column in \code{data} used as
#'   the case identifier for counting observations.
#' @param fill_color Named character vector or character vector. Color specification
#'   for the bars. Can be a named vector mapping variable categories to colors, or
#'   a character vector of colors.
#' @param xlab Character string, optional. The label for the x-axis. If not provided,
#'   it defaults to the name of the \code{strata} variable with underscores replaced
#'   by spaces.
#'
#' @return A ggplot object containing the stratified bar plot with:
#'   \itemize{
#'     \item Bars showing frequency distribution of \code{var} across \code{strata} categories
#'     \item An 'All cases' group for comparison
#'     \item Frequency percentages displayed above bars
#'     \item Sample counts (N=x) displayed below x-axis
#'     \item Dodged bar positioning for multiple categories
#'   }
#'
#' @details
#' Missing values in \code{var} are automatically filtered out before plotting.
#' An error will occur if no data is left after filtering. Both \code{var} and
#' \code{strata} variables are converted to factors if they are not already.
#'
#' @examples
#' # Create sample data
#' set.seed(123)
#' sample_data <- data.frame(
#'   patient_id = 1:200,
#'   treatment = factor(sample(c("A", "B", "Control"), 200, replace = TRUE)),
#'   outcome = factor(sample(c("Success", "Failure"), 200, replace = TRUE)),
#'   hospital = factor(sample(c("Hospital1", "Hospital2", "Hospital3"), 200, replace = TRUE))
#' )
#'
#' # Basic usage
#' p1 <- plot_stratified_barplot(
#'   data = sample_data,
#'   var = "outcome",
#'   strata = "treatment",
#'   case_id = "patient_id",
#'   fill_color = c("Success" = "lightgreen", "Failure" = "lightcoral")
#' )
#' print(p1)
#'
#' # With custom x-axis label
#' p2 <- plot_stratified_barplot(
#'   data = sample_data,
#'   var = "outcome",
#'   strata = "hospital",
#'   case_id = "patient_id",
#'   fill_color = c("lightblue", "orange"),
#'   xlab = "Medical Center"
#' )
#' print(p2)
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @importFrom scales percent_format
#' @importFrom stats setNames
#'
#' @export

plot_stratified_barplot <- function(
    data,
    var,
    strata,
    case_id,
    fill_color,
    xlab = NULL) {
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

  # Filter out missing values for var
  plot_data <- data[!is.na(data[[var]]), ]
  if (nrow(plot_data) == 0) {
    stop(paste(
      "No data available for 'var' and 'strata' after filtering",
      "out missing values of 'var'."
    ))
  }

  # Get counts for each combination of var and strata
  strat_data <- plot_data |>
    group_by(across(all_of(c(strata, var)))) |>
    summarise(count = n(), .groups = "drop")
  names(strat_data)[names(strat_data) == "count"] <- case_id

  # Ensure all combinations of categories are accounted for
  complete_combinations <- expand_grid(
    !!sym(strata) := levels(plot_data[[strata]]),
    !!sym(var) := levels(plot_data[[var]])
  )

  strat_data <- complete_combinations |>
    left_join(strat_data, by = c(strata, var)) |>
    replace_na(setNames(list(0), case_id))

  # Calculate frequencies
  total_counts <- plot_data |>
    group_by(across(all_of(strata))) |>
    summarise(total = n(), .groups = "drop")

  strat_data <- strat_data |>
    left_join(total_counts, by = strata) |>
    mutate(freq = ifelse(total > 0, .data[[case_id]] / total, NA_real_))

  # Aggregate data for all cases
  full_data <- plot_data |>
    group_by(across(all_of(var))) |>
    summarise(!!sym(case_id) := n(), .groups = "drop") |>
    mutate(!!sym(strata) := "All cases",
      freq = .data[[case_id]] / nrow(plot_data)
    )

  # Combine full and stratified data and remove any missing frequencies
  summ_stats <- bind_rows(full_data, strat_data |> select(-total)) |>
    filter(!is.na(freq))

  # Make labels for counts and frequencies
  summ_stats <- summ_stats |>
    mutate(
      count_lab = paste0("N=", .data[[case_id]]),
      freq_lab = paste0(round(freq * 100, 1), "%")
    )

  # Ensure strata categories has initial ordering
  strata_levels <- c("All cases", levels(plot_data[[strata]]))
  summ_stats[[strata]] <- factor(
    summ_stats[[strata]],
    levels = strata_levels, ordered = TRUE
  )

  # Generate stratified bar plot
  if (is.null(xlab)) {
    xlab <- gsub("_", " ", strata)
  }

  # Handle fill_color
  if (is.null(names(fill_color))) {
    # If fill_color is not named, create names based on var levels
    var_levels <- levels(summ_stats[[var]])
    if (length(fill_color) < length(var_levels)) {
      fill_color <- rep_len(fill_color, length(var_levels))
    }
    names(fill_color) <- var_levels[1:length(fill_color)]
  }

  g <- ggplot(
    data = summ_stats,
    aes(x = .data[[strata]], y = freq, fill = .data[[var]])
  ) +
    geom_bar(position = "dodge", stat = "identity", color = "black") +
    geom_text(aes(y = freq + 0.01, label = freq_lab),
      position = position_dodge(0.9), angle = 90, size = 3
    ) +
    geom_text(aes(y = -0.03, label = count_lab),
      position = position_dodge(0.9), size = 2.5
    ) +
    scale_y_continuous(
      breaks = seq(0, 1, 0.2),
      labels = scales::percent_format(accuracy = 1),
      limits = c(-0.05, max(summ_stats$freq) + 0.15)
    ) +
    scale_fill_manual(name = gsub("_", " ", var), values = fill_color) +
    labs(x = xlab, y = "Frequency (%)") +
    guides(fill = guide_legend(title.position = "top", ncol = 2)) +
    theme_bw() +
    theme(
      text = element_text(size = 10),
      axis.text.x = element_text(angle = 20, hjust = 1),
      legend.position = "top"
    )

  return(g)
}
