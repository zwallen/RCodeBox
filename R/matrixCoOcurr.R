#' Test for co-occurrence of features in a matrix
#'
#' This function calculates a co-occurrence matrix for a given set of features,
#' optionally adjusting for covariates.
#'
#' @param M A matrix or data frame where rows represent cases and columns represent
#'   features.
#' @param covars An optional matrix or data frame of covariates, where rows represent
#'   cases and columns are variables wanting to be included as covariates. Needs to have
#'   the same rownames as \code{M}.
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{result_summary}{A data frame summarizing the co-occurrence results}
#'   \item{or_matrix}{A matrix of odds ratios}
#'   \item{or_lower_matrix}{A matrix of lower bounds for odds ratio confidence intervals}
#'   \item{or_upper_matrix}{A matrix of upper bounds for odds ratio confidence intervals}
#'   \item{p_matrix}{A matrix of p-values}
#'   \item{padj_matrix}{A matrix of adjusted p-values}
#'   \item{prop_matrix}{A matrix of co-occurrence proportions}
#' }
#'
#' @details
#' The function calculates odds ratios, confidence intervals, p-values, and
#' proportions of co-occurrence for all pairs of features in the input matrix.
#' If covariates are provided, the function uses logistic regression (via the
#' \code{logistf} package) to adjust for these covariates. Otherwise, it uses Fisher's
#' exact test.
#'
#' The function efficiently handles comparing features to themselves.
#'
#' P-values are adjusted for multiple testing using the Benjamini-Hochberg method
#' (\code{p.adjust(..., method = 'BH')}).
#'
#' @note This function requires the \code{logistf} package to be installed.
#' Case names in \code{M} and \code{covars} must match exactly if providing covariates.
#'
#' @examples
#' M <- matrix(rbinom(1000, 1, 0.5), ncol = 10)
#' colnames(M) <- paste0("Feature", 1:10)
#' result <- matrixCoOcurr(M)
#'
#' @importFrom logistf logistf
#' @importFrom stats fisher.test na.omit p.adjust
#' @importFrom utils combn
#'
#' @export

matrixCoOcurr <- function(M, covars = NULL) {
  # Create matrices for outputs
  or_matrix <- matrix(
    nrow = ncol(M),
    ncol = ncol(M),
    dimnames = list(colnames(M), colnames(M))
  )
  or_lower_matrix <- matrix(
    nrow = ncol(M),
    ncol = ncol(M),
    dimnames = list(colnames(M), colnames(M))
  )
  or_upper_matrix <- matrix(
    nrow = ncol(M),
    ncol = ncol(M),
    dimnames = list(colnames(M), colnames(M))
  )
  p_matrix <- matrix(
    nrow = ncol(M),
    ncol = ncol(M),
    dimnames = list(colnames(M), colnames(M))
  )
  prop_matrix <- matrix(
    nrow = ncol(M),
    ncol = ncol(M),
    dimnames = list(colnames(M), colnames(M))
  )

  # If covariates given, make sure covariate data lines up with feature matrix
  if (!is.null(covars)) {
    if (
      !(sum(rownames(M) %in% rownames(covars)) != nrow(M)) |
        !(sum(rownames(covars) %in% rownames(M)) != nrow(covars))
    ) {
      stop("Row names included in feature and covariate data do not align")
    }
    covars <- covars[rownames(M), ]
  }

  # Begin calculations and testing
  for (x in seq_len(ncol(M))) {
    for (y in seq_len(ncol(M))) {
      # Create testing data for pair of features
      if (!is.null(covars)) {
        test_data <- data.frame(y = M[, y], x = M[, x], covars)
      } else {
        test_data <- data.frame(y = M[, y], x = M[, x])
      }

      # Perform testing
      if (colnames(M)[x] == colnames(M)[y]) {
        # Give null results if testing the same feature
        or_matrix[x, y] <- 1
        or_lower_matrix[x, y] <- NA
        or_upper_matrix[x, y] <- NA
        p_matrix[x, y] <- 1
        prop_matrix[x, y] <- 1
      } else if (
        sum(test_data$x[!is.na(test_data$x) & !is.na(test_data$y)]) == 0 |
          sum(test_data$y[!is.na(test_data$x) & !is.na(test_data$y)]) == 0
      ) {
        # Give null results for features with no overlapping results due to missing data
        or_matrix[x, y] <- 1
        or_lower_matrix[x, y] <- NA
        or_upper_matrix[x, y] <- NA
        p_matrix[x, y] <- 1
        prop_matrix[x, y] <- 0
      } else {
        # Perform test
        if (!is.null(covars)) {
          res <- logistf::logistf(y ~ ., data = test_data)
        } else {
          res <- with(test_data, fisher.test(y, x))
        }

        # Add results to matrices
        if (!is.null(covars)) {
          or_matrix[x, y] <- exp(res$coefficients["x"])
          or_lower_matrix[x, y] <- exp(res$ci.lower["x"])
          or_upper_matrix[x, y] <- exp(res$ci.upper["x"])
          p_matrix[x, y] <- res$prob["x"]
        } else {
          or_matrix[x, y] <- res$estimate
          or_lower_matrix[x, y] <- res$conf.int[1]
          or_upper_matrix[x, y] <- res$conf.int[2]
          p_matrix[x, y] <- res$p.value
        }
        prop_matrix[x, y] <- sum(na.omit(M[, x] + M[, y] == 2)) /
          sum(na.omit(M[, x] + M[, y] > 0))
      }
    }
  }

  # Multiple test correct p-values for unique tests
  padj_matrix <- p_matrix
  padj_matrix[lower.tri(padj_matrix)] <- p.adjust(
    padj_matrix[lower.tri(padj_matrix)],
    method = "BH"
  )
  padj_matrix[upper.tri(padj_matrix)] <- p.adjust(
    padj_matrix[upper.tri(padj_matrix)],
    method = "BH"
  )

  # Combine results into one data.frame
  res <- data.frame(
    Feature_1 = t(combn(rownames(or_matrix), 2))[, 1],
    Feature_2 = t(combn(rownames(or_matrix), 2))[, 2],
    Proportion = prop_matrix[lower.tri(prop_matrix)],
    OR = or_matrix[lower.tri(or_matrix)],
    OR_lower = or_lower_matrix[lower.tri(or_lower_matrix)],
    OR_upper = or_upper_matrix[lower.tri(or_upper_matrix)],
    P = p_matrix[lower.tri(p_matrix)],
    FDR = padj_matrix[lower.tri(padj_matrix)]
  )
  res <- res[order(res$FDR), ]

  # Return data
  return(list(
    result_summary = res,
    or_matrix = or_matrix,
    or_lower_matrix = or_lower_matrix,
    or_upper_matrix = or_upper_matrix,
    p_matrix = p_matrix,
    padj_matrix = padj_matrix,
    prop_matrix = prop_matrix
  ))
}
