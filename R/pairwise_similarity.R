#' Calculate pairwise proportions of similarity between cases
#'
#' @description
#' This function computes the pairwise proportion of similarity between all rows
#' in a dataframe or matrix. Similarity is defined as the proportion of columns
#' for which two rows have equal (non-missing) values, divided by the total number
#' of columns in which at least one of the two rows is non-missing. The function
#' returns an `n x n` matrix, where each cell contains the proportion of overlap
#' between two cases, and diagonal entries are set to `NA`.
#'
#' @param df
#' A dataframe or matrix containing the variables for which pairwise similarity
#' should be calculated. Rows represent cases and columns represent features.
#' Missing values (`NA`) are allowed and are incorporated into the similarity
#' calculation.
#'
#' @return
#' A numeric matrix of size `n x n` (where `n` is the number of rows in `df`)
#' containing the pairwise proportions of similarity. Row and column names are
#' preserved from the input. Diagonal elements are set to `NA`, as self-similarity
#' is not computed.
#'
#' @details
#' For each pair of rows, the function:
#' \itemize{
#'   \item Computes a logical equality comparison across all columns.
#'   \item Counts the number of equal (non-`NA`) entries.
#'   \item Identifies the number of columns where at least one row is non-missing.
#'   \item Divides the number of equal entries by the number of valid comparisons.
#' }
#'
#' The computation is performed in C++ via Rcpp to improve performance for large
#' datasets.
#'
#' @export
#'
pairwise_similarity = function(df) {
  if (!is.matrix(df)) {
    df = as.matrix(df)
  }
  pairwise_similarity_cpp(df)
}
