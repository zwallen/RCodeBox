#' Perform linear regression by strata
#'
#' Performs linear regression for a numerical variable and a categorical strata
#' variable, stratifying the analysis by each category of the strata variable. The
#' function tests a category against all others for each category in turn.
#'
#' @param var Character string. The name of the numerical variable in \code{data} to be
#'   tested.
#' @param strata Character string. The name of the categorical variable in \code{data}
#'   to stratify by.
#' @param data Data frame containing the variables of interest.
#'
#' @return A named list with each strata category as keys, and values containing:
#'   \itemize{
#'     \item \code{n}: Number of observations in category
#'     \item \code{stats}: Summary statistics (mean <c2><b1> SD) for category
#'     \item \code{coef}: The estimated beta coefficient with 95% confidence interval
#'     \item \code{pvalue}: The p-value from linear regression testing
#'   }
#'
#' @details
#' If \code{strata} contains only 2 groups, the resulting coefficient will be the same
#' for each category, just the inverse of the other. The p-values should be exactly the
#' same.
#'
#' The \code{var} variable must be numeric. The \code{strata} variable will be converted
#' to a factor if it is not already. Rows with missing values in either variable are
#' automatically removed.
#'
#' @examples
#' # Create sample data
#' set.seed(123)
#' sample_data <- data.frame(
#'   numeric_var = rnorm(100, mean = 50, sd = 10),
#'   strata_var = sample(c("Group1", "Group2", "Group3"), 100, replace = TRUE)
#' )
#'
#' # Run linear regression by strata
#' results <- linear_reg_by_strata("numeric_var", "strata_var", sample_data)
#' print(results)
#'
#' @importFrom stats complete.cases lm summary.lm sd
#' @export

linear_reg_by_strata <- function(var, strata, data) {
  # Check if required columns exist
  if (!var %in% names(data)) {
    stop(paste("Variable", var, "not found in data"))
  }
  if (!strata %in% names(data)) {
    stop(paste("Variable", strata, "not found in data"))
  }

  # Check var is numerical
  if (!is.numeric(data[[var]])) {
    stop("The variable supplied to 'var' must be numeric.")
  }

  # Check strata is categorical
  if (!is.factor(data[[strata]])) {
    message(paste(
      "The variable supplied to 'strata' was not a factor.",
      "Converting this variable to factor with no specific ordering of levels."
    ))
    data[[strata]] <- as.factor(data[[strata]])
  }

  # Create empty dictionary to store results
  results <- list()

  # Drop rows with NA in var or strata
  data <- data[complete.cases(data[, c(var, strata)]), c(var, strata)]

  if (nrow(data) == 0) {
    stop("No complete cases found after removing missing values")
  }

  # Determine number of categories in strata variable
  cat_n <- length(levels(data[[strata]]))

  # Perform testing between var and each category of strata vs all others
  if (cat_n >= 2) {
    for (cat in levels(data[[strata]])) {
      # Create dummy variable for denoting category vs others
      strata_dummy <- ifelse(data[[strata]] == cat, 1, 0)

      # Calculate N and summary statistics
      n <- sum(strata_dummy)
      cat_data <- data[data[[strata]] == cat, var]
      summ_stats <- paste0(round(mean(cat_data), 1), "<c2><b1>", round(sd(cat_data), 1))

      # Perform statistical testing
      lm_model <- lm(data[[var]] ~ strata_dummy)
      lm_summary <- summary(lm_model)

      statistic <- lm_summary$coefficients["strata_dummy", "Estimate"]
      stderr <- lm_summary$coefficients["strata_dummy", "Std. Error"]
      pvalue <- lm_summary$coefficients["strata_dummy", "Pr(>|t|)"]

      # Calculate confidence interval and add to statistic
      ci_low <- statistic - 1.96 * stderr
      ci_high <- statistic + 1.96 * stderr
      statistic <- paste0(
        round(statistic, 2), " [",
        round(ci_low, 2), "; ",
        round(ci_high, 2), "]"
      )

      # Get results
      results[[cat]] <- list(
        n = n,
        stats = summ_stats,
        coef = statistic,
        pvalue = pvalue
      )
    }
  } else {
    stop("Given strata variable has < 2 categories.")
  }

  # Return results
  return(results)
}
