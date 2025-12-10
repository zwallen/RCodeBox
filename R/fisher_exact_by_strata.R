#' Stratified Fisher's Exact Test
#'
#' Performs Fisher's exact test between a categorical test variable and a
#' categorical strata variable, stratifying the analysis by each category of
#' the strata variable. The function compares a category against all others for
#' each category in turn.
#'
#' @param var The name of the categorical variable in `data` to be tested.
#' @param strata The name of the categorical variable in `data` to stratify by.
#' @param data The data frame containing the variables of interest.
#' @return A named list with each category as a key and the following values:
#'   - n: Number of observations in category.
#'   For each level of `var`:
#'     - stats: Summary statistics (N, %) for category.
#'     - coef: The estimated odds ratio with confidence interval.
#'     - pvalue: The p-value from Fisher's exact test.
#' @importFrom stats fisher.test
#' @export
#'
fisher_exact_by_strata <- function(var, strata, data) {
  # Ensure variables are factors
  if (!is.factor(data[[var]])) {
    data[[var]] <- factor(as.character(data[[var]]))
  }
  if (!is.factor(data[[strata]])) {
    data[[strata]] <- factor(as.character(data[[strata]]))
  }

  # Check to make sure variables have at least two levels
  if (length(levels(data[[var]])) < 2) {
    stop("Data specified for `var` has < 2 levels")
  }
  if (length(levels(data[[strata]])) < 2) {
    stop("Data specified for `strata` has < 2 levels")
  }

  # Perform analysis for each combination of `var` and `strata` variables
  res <- list()
  for (x in levels(data[[strata]])) {
    res[[x]] <- list()

    # Get total n for strata level
    res[[x]][["n"]] <- sum(!is.na(data[data[[strata]] == x, ][[var]]))

    for (y in levels(data[[var]])) {
      # Create dummy variables
      x_dummy <- as.integer(data[[strata]] == x)
      y_dummy <- as.integer(data[[var]] == y)

      # Create 2x2 table
      tbl <- table(x_dummy, y_dummy)

      # Calculate summary statistics
      stats <- paste0(
        tbl["1", "1"],
        " (",
        round(tbl["1", "1"] / sum(tbl["1", ]) * 100, 1),
        "%)"
      )

      # Perform statistical testing
      fit <- fisher.test(tbl, alternative = "two.sided")

      # Get odds ratio and confidence intervals
      coef <- paste0(
        round(fit$estimate, 2),
        " [",
        round(fit$conf.int[1], 2),
        "; ",
        round(fit$conf.int[2], 2),
        "]"
      )

      # Collate results
      res[[x]][[y]] <- list(
        stats = stats,
        coef = coef,
        pvalue = fit$p.value
      )
    }
  }
  res
}
