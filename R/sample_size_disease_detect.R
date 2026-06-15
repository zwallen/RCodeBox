#' Calculate sample size for clinical studies involving assays for
#' disease detection
#'
#' @description
#' Calculates the total sample size needed for a clinical studies aimed at assessing
#' the performance of a disease detection assay. The function uses Buderer's
#' formulas (Buderer, 1996) to obtain an initial analytic estimate for sensitivity
#' and specificity then refines that estimate via Monte Carlo simulation to
#' achieve a target probability (power) that the resulting confidence interval
#' half-widths for sensitivity and specificity do not exceed the requested
#' precision. The calculation accounts for expected disease prevalence,
#' participant dropout, and assay failure.
#'
#' @param disease_prevalence
#' Expected disease prevalence (prevalence among enrolled patients).
#' @param dropout_rate
#' Expected dropout rate (proportion of enrolled patients who do not complete
#' follow-up).
#' @param failure_rate
#' Expected assay failure rate (proportion of successfully followed samples that
#' fail testing) (default: 0).
#' @param sensitivity
#' Assay sensitivity (default: 0.9 or 90%).
#' @param specificity
#' Assay specificity (default: 0.85 or 85%).
#' @param precision
#' Half-width of desired confidence interval for sensitivity/specificity
#' (default: 0.1 or \eqn{\pm 10\%}).
#' @param confidence_interval
#' Confidence level for sensitivity/specificity estimation (default: 0.95 or 95%).
#' @param optimize_sample_size
#' Logical; if TRUE, the function will iteratively increase the sample size until
#' the estimated power for achieving the desired precision for both sensitivity and
#' specificity meets or exceeds `target_power` (default: TRUE).
#' @param target_power
#' Desired probability that the simulated confidence interval half-widths for
#' sensitivity and specificity will be less than or equal to `precision`
#' (default: 0.9).
#' @param n_sim
#' Number of Monte Carlo simulation replicates per candidate sample size
#' (default: 1000).
#' @param step_size
#' Increment to increase the total sample size when iteratively searching for
#' a sample size that meets `target_power` (default: 10).
#' @param max_iter
#' Maximum number of iterations to attempt when searching for a convergent
#' sample size (default: 10000).
#' @param seed
#' Random seed for reproducible simulation results (default: 42).
#' @param verbose
#' Logical; if TRUE progress messages are emitted during the iterative search
#' (default: TRUE).
#'
#' @return
#' A named list with the following elements:
#' - final_sample_size: integer, the total number of patients required (after
#'   adjustment for dropout and assay failure).
#' - power_sensitivity: numeric, estimated probability that the sensitivity
#'   confidence interval half-width is <= `precision` under the simulated design.
#' - power_specificity: numeric, estimated probability that the specificity
#'   confidence interval half-width is <= `precision` under the simulated design.
#' - required_diseased: integer, expected number of diseased (relapse) patients
#'   within the final sample.
#' - required_non_diseased: integer, expected number of non-diseased patients
#'   within the final sample.
#' - iterations: integer, number of iterations performed in the search loop.
#' - statement: character, human-readable summary of the final sample size and
#'   assumptions used.
#'
#' @references
#' Buderer (1996). Statistical methodology: I. Incorporating the prevalence of
#' disease into the sample size calculation for sensitivity and specificity.
#' Academic Emergency Medicine, 3(9), 895-900.
#'
#' @importFrom stats qnorm rbinom
#' @export
#'
sample_size_disease_detect <- function(
  disease_prevalence,
  dropout_rate,
  failure_rate = 0,
  sensitivity = 0.9,
  specificity = 0.85,
  precision = 0.1,
  confidence_interval = 0.95,
  optimize_sample_size = TRUE,
  target_power = 0.9,
  n_sim = 1000,
  step_size = 10,
  max_iter = 10000,
  seed = 42,
  verbose = TRUE
) {
  # Input validation
  if (!(disease_prevalence > 0 && disease_prevalence < 1)) {
    stop("disease_prevalence must be between 0 and 1")
  }
  if (!(dropout_rate >= 0 && dropout_rate < 1)) {
    stop("dropout_rate must be between 0 and 1")
  }
  if (!(failure_rate >= 0 && failure_rate < 1)) {
    stop("failure_rate must be between 0 and 1")
  }
  if (!(sensitivity > 0 && sensitivity < 1)) {
    stop("sensitivity must be between 0 and 1")
  }
  if (!(specificity > 0 && specificity < 1)) {
    stop("specificity must be between 0 and 1")
  }
  if (!(precision > 0 && precision < 1)) {
    stop("precision must be between 0 and 1")
  }

  # Set random seed for reproducibility
  set.seed(seed)

  # Get Z-score of alpha based on normal distribution
  alpha <- 1 - confidence_interval
  z <- qnorm(1 - alpha / 2)

  # Calculate initial sample size based on Buderer's formula
  n_sens <- (z^2 * sensitivity * (1 - sensitivity)) /
    (precision^2 * disease_prevalence)
  n_spec <- (z^2 * specificity * (1 - specificity)) /
    (precision^2 * (1 - disease_prevalence))

  # Get raw total sample size based off of which is larger
  n_total_exact <- max(n_sens, n_spec)
  raw_total <- ceiling(n_total_exact)

  # Inflate the raw sample size based on dropout and failure rates
  inflation_factor <- 1 / ((1 - dropout_rate) * (1 - failure_rate))
  n_current <- ceiling(raw_total * inflation_factor)
  if (verbose) {
    cat("Initial adjusted sample size:", n_current, "\n")
  }

  # Create function to perform simulations
  simulate_once <- function(n_total) {
    # Get simulated relapse event labels based on relapse rate
    disease <- rbinom(n_total, 1, disease_prevalence)

    # Simulate drop outs and assay failures
    keep <- rbinom(n_total, 1, 1 - dropout_rate)
    success <- rbinom(n_total, 1, 1 - failure_rate)
    idx <- which(keep == 1 & success == 1)
    if (length(idx) < 10) {
      return(c(NA, NA))
    }

    # Extract simulated passing cases
    disease_obs <- disease[idx]

    # Initialize test results
    test <- numeric(length(idx))

    # Indicate true positives and negatives
    d_idx <- which(disease_obs == 1)
    nd_idx <- which(disease_obs == 0)

    # Simulate test results for positive and negative patients based on
    # target sensitivity and specificity
    test[d_idx] <- rbinom(length(d_idx), 1, sensitivity)
    test[nd_idx] <- rbinom(length(nd_idx), 1, 1 - specificity)

    # Calculate sensitivity and specificity estimates and their half-widths
    n_d <- length(d_idx)
    sens_est <- if (n_d > 0) mean(test[d_idx]) else NA
    sens_se <- sqrt(sens_est * (1 - sens_est) / n_d)
    sens_hw <- z * sens_se

    n_nd <- length(nd_idx)
    spec_est <- if (n_nd > 0) mean(test[nd_idx] == 0) else NA
    spec_se <- sqrt(spec_est * (1 - spec_est) / n_nd)
    spec_hw <- z * spec_se

    c(sens_hw = sens_hw, spec_hw = spec_hw)
  }

  # Optimize sample size based on target power
  if (optimize_sample_size) {
    iter <- 0
    repeat {
      # Count iteration
      iter <- iter + 1
      if (iter > max_iter) {
        stop("Max iterations reached without convergence")
      }

      # Perform simulations
      sim_res <- replicate(n_sim, simulate_once(n_current))
      sim_res <- t(sim_res)

      # Calculate power for sensitivity and specificity
      sens_power <- mean(sim_res[, "sens_hw"] <= precision, na.rm = TRUE)
      spec_power <- mean(sim_res[, "spec_hw"] <= precision, na.rm = TRUE)

      # Report results
      if (verbose) {
        cat(sprintf(
          "N=%d | Sens power=%.3f | Spec power=%.3f\n",
          n_current,
          sens_power,
          spec_power
        ))
      }

      # Stop optimization when target power is met
      if (sens_power >= target_power && spec_power >= target_power) {
        break
      }

      n_current <- n_current + step_size
    }
  } else {
    sens_power <- NA
    spec_power <- NA
    iter <- 0
  }

  # Final subgroup breakdown
  n_diseased <- ceiling(n_current * disease_prevalence)
  n_nondiseased <- n_current - n_diseased

  # Create statement output
  statement <- sprintf(
    "To reliably evaluate the assay, approximately %d patients are required, including %d expected to be positive and %d to be negative. This sample size is designed to estimate sensitivity and specificity with an accuracy of about \u00B1%.2f at the %.0f%% confidence level. The calculation assumes a %.0f%% disease rate, %.0f%% dropout rate, and %.0f%% assay failure rate. Based on simulation, this design has an estimated %.1f%% probability of achieving the desired precision for sensitivity (target %.2f) and %.1f%% for specificity (target %.2f).",
    n_current,
    n_diseased,
    n_nondiseased,
    precision,
    confidence_interval * 100,
    disease_prevalence * 100,
    dropout_rate * 100,
    failure_rate * 100,
    sens_power * 100,
    sensitivity,
    spec_power * 100,
    specificity
  )

  # Return results
  list(
    final_sample_size = n_current,
    power_sensitivity = sens_power,
    power_specificity = spec_power,
    required_diseased = n_diseased,
    required_non_diseased = n_nondiseased,
    iterations = iter,
    statement = statement
  )
}
