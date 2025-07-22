#' Log transformation with zero handling
#'
#' This function performs a log transformation on a numeric vector,
#' handling zero values by replacing them with half the minimum non-zero value.
#'
#' @param x A numeric vector to be log-transformed.
#'
#' @return A numeric vector of log-transformed values.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Identifies zero values in the input vector
#'   \item Replaces zero values with half the minimum non-zero value in the input
#'   \item Applies the natural logarithm to the resulting vector
#' }
#'
#' This approach allows for log transformation of data containing zeros,
#' which would otherwise result in undefined values (negative infinity).
#'
#' @note Assumes the data being provided is count data with no negative values.
#'
#' @examples
#' x <- c(0, 1, 2, 3, 4, 5)
#' logTrans(x)
#'
#' @export

logTrans <- function(x) {
  y <- replace(x, x == 0, min(x[x > 0]) / 2)
  return(log(y))
}
