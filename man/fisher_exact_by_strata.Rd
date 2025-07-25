% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fisher_exact_by_strata.R
\name{fisher_exact_by_strata}
\alias{fisher_exact_by_strata}
\title{Perform Fisher's exact test by strata}
\usage{
fisher_exact_by_strata(var, strata, data)
}
\arguments{
\item{var}{Character string. The name of the binary variable in \code{data} to be
tested.}

\item{strata}{Character string. The name of the categorical variable in \code{data}
to stratify by.}

\item{data}{Data frame containing the variables of interest.}
}
\value{
A named list with each combination of variable category and strata category
as keys, and values containing:
\itemize{
\item \code{n}: Number of observations in category
\item \code{stats}: Summary statistics (N, \%) for category
\item \code{coef}: The estimated odds ratio with 95\% confidence interval
\item \code{pvalue}: The p-value from Fisher's exact test
}
}
\description{
Performs Fisher's exact test between a binary variable and a categorical strata
variable, stratifying the analysis by each category of the strata variable. The
function compares a category against all others for each category in turn.
}
\details{
If \code{strata} contains only 2 groups, the resulting odds ratio will be the same
for each category, just the inverse of the other. The p-values should be exactly the
same.

Both \code{var} and \code{strata} will be converted to factors if they are not
already. Rows with missing values in either variable are automatically removed.
}
\examples{
# Create sample data
set.seed(123)
sample_data <- data.frame(
  binary_var = sample(c("A", "B"), 100, replace = TRUE),
  strata_var = sample(c("Group1", "Group2", "Group3"), 100, replace = TRUE)
)

# Run Fisher's exact test by strata
results <- fisher_exact_by_strata("binary_var", "strata_var", sample_data)
print(results)

}
