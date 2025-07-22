#' Perform Fisher's exact test by strata
#'
#' Performs Fisher's exact test between a binary variable and a categorical strata
#' variable, stratifying the analysis by each category of the strata variable. The
#' function compares a category against all others for each category in turn.
#'
#' @param var Character string. The name of the binary variable in \code{data} to be 
#'   tested.
#' @param strata Character string. The name of the categorical variable in \code{data} 
#'   to stratify by.
#' @param data Data frame containing the variables of interest.
#'
#' @return A named list with each combination of variable category and strata category 
#'   as keys, and values containing:
#'   \itemize{
#'     \item \code{n}: Number of observations in category
#'     \item \code{stats}: Summary statistics (N, %) for category
#'     \item \code{coef}: The estimated odds ratio with 95% confidence interval
#'     \item \code{pvalue}: The p-value from Fisher's exact test
#'   }
#'
#' @details
#' If \code{strata} contains only 2 groups, the resulting odds ratio will be the same 
#' for each category, just the inverse of the other. The p-values should be exactly the 
#' same.
#'
#' Both \code{var} and \code{strata} will be converted to factors if they are not 
#' already. Rows with missing values in either variable are automatically removed.
#'
#' @examples
#' # Create sample data
#' set.seed(123)
#' sample_data <- data.frame(
#'   binary_var = sample(c("A", "B"), 100, replace = TRUE),
#'   strata_var = sample(c("Group1", "Group2", "Group3"), 100, replace = TRUE)
#' )
#'
#' # Run Fisher's exact test by strata
#' results <- fisher_exact_by_strata("binary_var", "strata_var", sample_data)
#' print(results)
#'
#' @importFrom stats complete.cases fisher.test
#' @export

fisher_exact_by_strata <- function(var, strata, data) {
  # Check if required columns exist
  if (!var %in% names(data)) {
    stop(paste("Variable", var, "not found in data"))
  }
  if (!strata %in% names(data)) {
    stop(paste("Variable", strata, "not found in data"))
  }

  # Convert to factors if not already
  if (!is.factor(data[[var]])) {
    message(paste(
      "The variable supplied to 'var' was not a factor.",
      "Converting this variable to factor with no specific ordering of levels."
    ))
    data[[var]] <- as.factor(data[[var]])
  }

  if (!is.factor(data[[strata]])) {
    message(paste(
      "The variable supplied to 'strata' was not a factor.",
      "Converting this variable to factor with no specific ordering of levels."
    ))
    data[[strata]] <- as.factor(data[[strata]])
  }

  # Remove rows with missing values
  complete_cases <- complete.cases(data[, c(var, strata)])
  data <- data[complete_cases, c(var, strata)]

  if (nrow(data) == 0) {
    stop("No complete cases found after removing missing values")
  }

  # Create empty list to store results
  results <- list()

  # Determine number of categories in strata variable
  cat_n <- length(levels(data[[strata]]))

  # For each level of var, perform testing between var and
  # each category of strata vs all others
  for (group in levels(data[[var]])) {
    if (cat_n >= 2) {
      for (cat in levels(data[[strata]])) {
        # Create dummy variables for analysis
        var_dummy <- ifelse(data[[var]] == group, 1, 0)
        strata_dummy <- ifelse(data[[strata]] == cat, 1, 0)

        if (sum(var_dummy) > 0) {
          # Create a 2x2 contingency table
          table <- table(var_dummy, strata_dummy)

          # Calculate N and summary statistics
          n <- sum(table[, "1"])
          summ_stats <- paste0(
            table[, "1"], " (",
            round(table[, "1"] / n * 100, 1), "%)"
          )
          names(summ_stats) <- rownames(table)

          # Perform statistical testing
          fisher_result <- fisher.test(table, alternative = "two.sided")
          statistic <- fisher_result$estimate
          pvalue <- fisher_result$p.value

          # Calculate confidence interval and add to statistic
          ci_low <- fisher_result$conf.int[1]
          ci_high <- fisher_result$conf.int[2]
          statistic <- paste0(
            round(statistic, 2), " [",
            round(ci_low, 2), "; ",
            round(ci_high, 2), "]"
          )

          # Get results
          result_key <- paste(group, cat, sep = ", ")
          results[[result_key]] <- list(
            n = n,
            stats = summ_stats,
            coef = statistic,
            pvalue = pvalue
          )
        }
      }
    } else {
      stop("Given strata variable has < 2 categories.")
    }
  }

  return(results)
}
