#' Differential abundance testing using generalized linear mixed models
#'
#' Performs differential abundance testing on a matrix of feature data
#' (e.g., microbiome, RNA-seq) using generalized linear mixed models (GLMMs) with
#' various normalization and transformation options.
#'
#' @param feats A numeric matrix or data frame of feature abundances with samples
#'   as rows and features as columns.
#' @param samps A data frame of sample metadata with samples as rows and variables
#'   as columns. Row names must match those in \code{feats}.
#' @param fixed_formula Character string specifying the fixed effects formula. Variables
#'   should be separated by \code{+}. For example, \code{"treatment + age"}. Listed
#'   variables must be present in the \code{samps} data frame.
#' @param min_prevalence Numeric value between 0 and 1 specifying the minimum
#'   prevalence threshold for feature inclusion. Default is 0.1.
#' @param normalization Character string specifying normalization method. Options are
#'   \code{"tss"} (total sum scaling, default), \code{"clr"} (centered log-ratio),
#'   or \code{"none"}.
#' @param transformation Character string specifying transformation method. Options are
#'   \code{"log"} (default) or \code{"none"}. Will be ignored if \code{"clr"} is
#'   chosen for the \code{normalization}.
#' @param random_formula Character string specifying the random effects formula. See
#'   \code{glmmTMB} documentation for syntax. For example, \code{"(1|subject_id)"}.
#'   Listed variables must be present in the \code{samps} data frame. Default is
#'   \code{NULL} for no random effects.
#' @param family Character string specifying the error distribution family. Must be a
#'   distribution family supported by \code{glmmTMB}. Default is \code{"gaussian"}.
#' @param ziformula Character string specifying the zero-inflation formula. Default is
#'   \code{"~0"} for no zero-inflation.
#' @param correction Character string specifying the multiple testing correction
#'   method. Default is \code{"BH"} for Benjamini-Hochberg.
#'
#' @return A list containing:
#' \describe{
#'   \item{result_summary}{Data frame with test results including coefficients,
#'     p-values, and FDR-adjusted p-values for each feature and variable}
#'   \item{model_fits}{List of fitted glmmTMB model objects for each feature}
#' }
#'
#' @details
#' This function performs differential abundance testing on feature data using
#' the following workflow:
#' \enumerate{
#'   \item Applies the specified normalization method to feature data
#'   \item Applies the specified transformation method
#'   \item Filters features based on minimum prevalence threshold
#'   \item Fits GLMMs for each feature using the specified formula
#'   \item Extracts coefficients, standard errors, and p-values
#'   \item Applies multiple testing correction
#' }
#'
#' For CLR normalization, the transformation is applied within the normalization
#' step and any additional transformation is skipped.
#'
#' The log transformations use a pseudo-count approach, adding half the minimum
#' positive value to handle zeros before taking the logarithm.
#'
#' @note This function requires the \code{glmmTMB} package to be installed and loaded.
#' Sample names in \code{feats} and \code{samps} must match exactly.
#'
#' @examples
#' # Generate example data
#' set.seed(123)
#' n_samples <- 50
#' n_features <- 20
#'
#' # Create sample names
#' sample_names <- paste0("Sample_", 1:n_samples)
#'
#' # Generate feature abundance data
#' feature_matrix <- matrix(
#'   rpois(n_samples * n_features, lambda = 100),
#'   nrow = n_samples,
#'   ncol = n_features,
#'   dimnames = list(
#'     sample_names,
#'     paste0("Feature_", 1:n_features)
#'   )
#' )
#'
#' # Add some zeros to simulate realistic sparse data
#' zero_indices <- sample(
#'   1:(n_samples * n_features),
#'   size = n_samples * n_features * 0.3
#' )
#' feature_matrix[zero_indices] <- 0
#'
#' # Generate sample metadata
#' sample_metadata <- data.frame(
#'   treatment = factor(rep(c("Control", "Treatment"), each = n_samples / 2)),
#'   age = rnorm(n_samples, mean = 45, sd = 10),
#'   subject_id = factor(rep(1:(n_samples / 2), 2)),
#'   row.names = sample_names
#' )
#'
#' # With fixed and random effects and zero inflation
#' results <- diffGLMM(
#'   feats = feature_matrix,
#'   samps = sample_metadata,
#'   transformation = "none",
#'   fixed_formula = "treatment + age",
#'   random_formula = "(1|subject_id)",
#'   family = "poisson",
#'   ziformula = "~1"
#' )
#'
#' # With CLR normalization and no transformation
#' results <- diffGLMM(
#'   feats = feature_matrix,
#'   samps = sample_metadata,
#'   normalization = "clr",
#'   transformation = "none",
#'   fixed_formula = "treatment"
#' )
#'
#' # View results
#' head(results$result_summary)
#'
#' @seealso \code{\link[glmmTMB]{glmmTMB}} for the underlying model fitting function.
#'
#' @importFrom glmmTMB glmmTMB
#' @importFrom stats as.formula p.adjust
#'
#' @export

diffGLMM <- function(
    feats,
    samps,
    fixed_formula,
    min_prevalence = 0.1,
    normalization = "tss",
    transformation = "log",
    random_formula = NULL,
    family = "gaussian",
    ziformula = "~0",
    correction = "BH") {
  # Check that feature matrix contains all numeric features
  if (sum(apply(feats, 2, is.numeric)) < ncol(feats)) {
    stop("Some features were not found to be numeric, all need to be numeric")
  }

  # Check that sample matrix and fixed formula are provided
  if (missing(samps)) {
    stop("Please provide a sample metadata data frame with variables for testing")
  }
  if (missing(fixed_formula)) {
    stop("Please provide a fixed effects formula for the model")
  }

  # Create function for log transform after adding small constant
  logTrans <- function(x) {
    y <- x + (min(x[x > 0]) / 2)
    return(log(y))
  }

  # Perform requested normalization
  if (normalization == "none") {
    cat("No normalization being performed", "\n")
    feats_norm <- feats
  } else if (normalization == "clr") {
    cat("CLR normalization being performed", "\n")
    feats_norm <- t(apply(feats, 1, function(x) {
      logTrans(x) - mean(logTrans(x))
    }))
  } else if (normalization == "tss") {
    cat("TSS normalization being performed", "\n")
    feats_norm <- t(apply(feats, 1, function(x) {
      x / sum(x)
    }))
  } else {
    stop(
      'Only normalization options at this time are "tss" (default), "clr", or "none"'
    )
  }

  # Perform requested transformation
  if (transformation == "none") {
    cat("No transformation being performed", "\n")
    input_data <- feats_norm
  } else if (normalization == "clr") {
    cat("Log transformation was performed within clr normalization", "\n")
    input_data <- feats_norm
  } else if (transformation == "log") {
    cat("Log transformation being performed", "\n")
    input_data <- t(apply(feats_norm, 1, logTrans))
  } else {
    stop(
      'Only transformation options at this time are "log" (default) or "none"'
    )
  }

  # Filter features based on minimum prevalence specified
  in_features <- colnames(feats)[
    colSums(feats > 0) >= (nrow(feats) * min_prevalence)
  ]
  input_data <- input_data[, in_features]
  cat(
    "Number of features being tested after prevalence filtering: ",
    ncol(input_data),
    "\n"
  )

  # Make sure data contains same samples and that they are in same order
  if (!(sum(rownames(feats) == rownames(samps)) == nrow(feats))) {
    stop("Samples included in feature and sample data are different")
  }
  feats <- feats[rownames(samps), ]
  feats_norm <- feats_norm[rownames(samps), ]

  # Filter for samples with data for variables included in model
  variables <- strsplit(fixed_formula, " \\+ ")[[1]]
  for (var in variables) {
    samps <- samps[!is.na(samps[, var]), ]
    feats <- feats[rownames(samps), ]
    feats_norm <- feats_norm[rownames(samps), ]
  }

  # Build formula
  if (!is.null(random_formula)) {
    cat(
      "Formula to be tested: ",
      paste("x ~ ", fixed_formula, " + ", random_formula, sep = ""),
      "\n"
    )
    formula <- as.formula(paste(
      "x ~ ",
      fixed_formula,
      " + ",
      random_formula,
      sep = ""
    ))
  } else {
    cat("Formula to be tested: ", paste("x ~ ", fixed_formula, sep = ""), "\n")
    formula <- as.formula(paste("x ~ ", fixed_formula, sep = ""))
  }

  # Run the linear mixed model for each feature
  cat("Running differential abundance testing...", "\n")
  fits <- apply(input_data, 2, function(x) {
    glmmTMB::glmmTMB(
      formula,
      family = family,
      ziformula = as.formula(ziformula),
      data = data.frame(samps, x = x, check.names = FALSE)
    )
  })

  # Get summary of results
  cat("Grabbing summary statistics and coalescing results...", "\n")
  res <- data.frame()
  for (i in seq_len(length(variables))) {
    # For variables with 2 groups, get N samples positive for feature in each group,
    # and calculate the mean value for feature in each group
    if (length(table(samps[, variables[i]])) == 2) {
      group_1_index <- samps[, variables[i]] ==
        names(table(samps[, variables[i]]))[2]
      group_1_index[is.na(group_1_index)] <- FALSE
      group_2_index <- samps[, variables[i]] ==
        names(table(samps[, variables[i]]))[1]
      group_2_index[is.na(group_2_index)] <- FALSE
      n1 <- colSums(feats[group_1_index, ] > 0)
      n2 <- colSums(feats[group_2_index, ] > 0)
      mean1 <- colMeans(feats_norm[group_1_index, ])
      mean2 <- colMeans(feats_norm[group_2_index, ])
    } else {
      n1 <- rep(sum(table(samps[, variables[i]])), ncol(feats))
      names(n1) <- colnames(feats)
      n2 <- rep(NA, ncol(feats))
      names(n2) <- colnames(feats)
      mean1 <- colMeans(feats_norm)
      mean2 <- rep(NA, ncol(feats_norm))
      names(mean2) <- colnames(feats_norm)
    }

    # Find the actual coefficient name(s) that correspond to this variable
    # Get coefficient names from the first model as a reference
    coef_names <- rownames(summary(fits[[1]])$coefficients$cond)

    # Find coefficient names that start with the variable name
    matching_coefs <- coef_names[grepl(paste0("^", variables[i]), coef_names)]

    # If no exact match, try to find coefficients containing the variable name
    if (length(matching_coefs) == 0) {
      matching_coefs <- coef_names[grepl(variables[i], coef_names)]
    }

    # If still no match, skip this variable with a warning
    if (length(matching_coefs) == 0) {
      warning(paste("No coefficient found for variable:", variables[i]))
      next
    }

    # Use the first matching coefficient (for factors, this is typically the main effect)
    coef_name <- matching_coefs[1]

    # Summarize results for variable
    rvar <- data.frame(
      Variable = variables[i],
      Feature = colnames(input_data),
      N1 = n1[colnames(input_data)],
      N2 = n2[colnames(input_data)],
      Mean1 = mean1[colnames(input_data)],
      Mean2 = mean2[colnames(input_data)],
      Beta = sapply(fits, function(x) {
        coef_matrix <- summary(x)$coefficients$cond
        if (coef_name %in% rownames(coef_matrix)) {
          coef_matrix[coef_name, 1]
        } else {
          NA
        }
      }),
      SE = sapply(fits, function(x) {
        coef_matrix <- summary(x)$coefficients$cond
        if (coef_name %in% rownames(coef_matrix)) {
          coef_matrix[coef_name, 2]
        } else {
          NA
        }
      }),
      P = sapply(fits, function(x) {
        coef_matrix <- summary(x)$coefficients$cond
        if (coef_name %in% rownames(coef_matrix)) {
          coef_matrix[coef_name, 4]
        } else {
          NA
        }
      }),
      check.names = FALSE
    )

    # Calculate FDR for non-NA p-values
    p_values <- sapply(fits, function(x) {
      coef_matrix <- summary(x)$coefficients$cond
      if (coef_name %in% rownames(coef_matrix)) {
        coef_matrix[coef_name, 4]
      } else {
        NA
      }
    })

    rvar$FDR <- p.adjust(p_values, method = correction)

    res <- rbind(res, rvar[order(rvar$P, na.last = TRUE), ])

    # Add untested features to results if they exist
    if (nrow(rvar) != ncol(feats)) {
      res <- rbind(
        res,
        data.frame(
          Variable = variables[i],
          Feature = colnames(feats)[
            !(colnames(feats) %in% colnames(input_data))
          ],
          N1 = n1[colnames(feats)[
            !(colnames(feats) %in% colnames(input_data))
          ]],
          N2 = n2[colnames(feats)[
            !(colnames(feats) %in% colnames(input_data))
          ]],
          Mean1 = mean1[colnames(feats)[
            !(colnames(feats) %in% colnames(input_data))
          ]],
          Mean2 = mean2[colnames(feats)[
            !(colnames(feats) %in% colnames(input_data))
          ]],
          Beta = NA,
          SE = NA,
          P = NA,
          FDR = NA,
          check.names = FALSE
        )
      )
    }
  }
  cat("Done", "\n")
  return(list(result_summary = res, model_fits = fits))
}
