#' Sort a matrix based on mutual exclusivity
#'
#' This function sorts a matrix based on mutual exclusivity of the contents (e.g.,
#' mutation presence/absence, missing/non-missing data, etc.).
#' It prioritizes rows with more prevalent values so more abundant
#' features are placed on top.
#'
#' @param M A matrix to be sorted
#'
#' @return A sorted matrix
#'
#' @examples
#' M <- matrix(c(1, NA, 3, 0, 2, 4, "", 5, 6), nrow = 3, ncol = 3)
#' sorted_M <- mutExSort(M)
#'
#' @export

mutExSort <- function(M) {
  # Sort rows based on number of valid entries
  geneOrder <- order(
    rowSums(!is.na(M) & M != "" & M > 0 & M != FALSE),
    decreasing = TRUE
  )

  # Function to calculate column scores
  scoreCol <- function(x) {
    valid <- !is.na(x) & x != "" & x > 0 & x != FALSE
    sum(2^(length(x) - which(valid)))
  }

  # Calculate and sort column scores
  scores <- apply(M[geneOrder, ], 2, scoreCol)
  sampleOrder <- order(scores, decreasing = TRUE)

  # Return sorted matrix
  return(M[geneOrder, sampleOrder])
}
