#' Calculate the 95% confidence interval of a coefficient
#'
#' This function calculates the 95% confidence interval for a given coefficient and
#' its standard error.
#'
#' @param coef Numeric value representing the coefficient estimate.
#' @param se Numeric value representing the standard error of the coefficient.
#'
#' @return A named numeric vector containing two elements:
#' \describe{
#'   \item{lower_ci}{The lower bound of the 95% confidence interval}
#'   \item{upper_ci}{The upper bound of the 95% confidence interval}
#' }
#'
#' @details
#' The function calculates the 95% confidence interval using the formula:
#' \code{CI = coefficient ± (1.96 * standard error)}
#' where 1.96 is the z-score for a 95% confidence level.
#'
#' @examples
#' ci(0.5, 0.1)
#' ci(coef = 2.3, se = 0.4)
#'
#' @export

ci <- function(coef, se) {
  lower_ci <- coef - 1.96 * se
  upper_ci <- coef + 1.96 * se
  return(c(lower_ci = lower_ci, upper_ci = upper_ci))
}
