#' Stratified Linear Regression
#'
#' Performs linear regression between a numeric test variable and a categorical
#' strata variable, stratifying the analysis by each category of the strata variable.
#' The function compares a category against all others for each category in turn.
#'
#' @param var The name of the numerical variable in `data` to be tested.
#' @param strata The name of the categorical variable in `data` to stratify by.
#' @param data The data frame containing the variables of interest.
#' @return A named list with each category as a key and the following values:
#'   - n: Number of observations in category.
#'   - stats: Summary statistics (Mean±SD) for category.
#'   - coef: The estimated beta coefficient with confidence interval.
#'   - pvalue: The p-value from linear regression.
#' @importFrom stats confint lm sd
#' @export
#'
linear_reg_by_strata <- function(var, strata, data) {
  # Ensure test variable is numeric
  if (!is.numeric(data[[var]])) {
    data[[var]] <- as.numeric(data[[var]])
  }

  # Ensure strata variable is factor
  if (!is.factor(data[[strata]])) {
    data[[strata]] <- factor(data[[strata]])
  }

  # Check to make sure variables have at least two levels
  if (length(unique(data[[var]])) < 2) {
    stop("Data specified for `var` has < 2 levels")
  }
  if (length(levels(data[[strata]])) < 2) {
    stop("Data specified for `strata` has < 2 levels")
  }

  # Perform analysis for each level of `strata`
  res <- list()
  for (x in levels(data[[strata]])) {
    # Create dummy variable
    x_dummy <- as.integer(data[[strata]] == x)

    # Calculate summary statistics
    stats <- paste0(
      round(mean(data[data[[strata]] == x, ][[var]], na.rm = TRUE), 1),
      "\u00B1",
      round(sd(data[data[[strata]] == x, ][[var]], na.rm = TRUE), 1)
    )

    # Perform statistical testing
    fit <- lm(data[[var]] ~ x_dummy)

    # Get beta coefficient and confidence intervals
    coef <- paste0(
      round(coef(fit)[2], 2),
      " [",
      round(confint(fit)[2, 1], 2),
      "; ",
      round(confint(fit)[2, 2], 2),
      "]"
    )

    # Collate results
    res[[x]] <- list(
      n = sum(!is.na(data[data[[strata]] == x, ][[var]])),
      stats = stats,
      coef = coef,
      pvalue = summary(fit)$coefficients[2, 4]
    )
  }
  return(res)
}
