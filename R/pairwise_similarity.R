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
#' The computation is vectorized across columns to improve performance for large
#' datasets.
#'
#' @export
#'
pairwise_similarity = function(df) {
  # Get dimensions
  n = nrow(df)
  p = ncol(df)

  # Create mask for NA values
  na_mask = !is.na(df)

  # Compare all rows at once making 3D array
  equal_array = array(
    unlist(lapply(seq_len(p), function(j) {
      outer(df[, j], df[, j], `==`)
    })),
    dim = c(n, n, p)
  )

  # Count equality across columns
  num_equal = apply(
    equal_array,
    1:2,
    function(x) sum(x, na.rm = TRUE)
  )

  # Get the "valid" non-NA rows
  valid_array = array(
    unlist(lapply(seq_len(p), function(j) {
      outer(na_mask[, j], na_mask[, j], `|`)
    })),
    dim = c(n, n, p)
  )

  # Calculate the denominators
  denom = apply(valid_array, 1:2, sum)

  # Calculate the proportions
  prop_mat = num_equal / denom

  # Mask diagonal
  diag(prop_mat) = NA

  # Assign original rownames to proportion matrix
  rownames(prop_mat) = rownames(df)
  colnames(prop_mat) = rownames(df)

  return(prop_mat)
}
