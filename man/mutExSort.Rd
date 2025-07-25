% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mutExSort.R
\name{mutExSort}
\alias{mutExSort}
\title{Sort a matrix based on mutual exclusivity}
\usage{
mutExSort(M)
}
\arguments{
\item{M}{A matrix to be sorted}
}
\value{
A sorted matrix
}
\description{
This function sorts a matrix based on mutual exclusivity of the contents (e.g.,
mutation presence/absence, missing/non-missing data, etc.).
It prioritizes rows with more prevalent values so more abundant
features are placed on top.
}
\examples{
M <- matrix(c(1, NA, 3, 0, 2, 4, "", 5, 6), nrow = 3, ncol = 3)
sorted_M <- mutExSort(M)

}
