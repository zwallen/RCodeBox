#' TCGA COAD/READ gene expression (TPM)
#'
#' Gene-level expression as \emph{Transcripts Per Million} (TPM) for TCGA
#' \strong{COAD} and \strong{READ} samples, harmonized by GDC.
#'
#' @format A numeric matrix-like object with genes in rows and samples in columns:
#' \itemize{
#'   \item \strong{rownames}: Ensembl gene IDs matching \code{\link{gex_data}}.
#'   \item \strong{colnames}: TCGA sample barcodes (e.g., \code{TCGA-XX-YYYY-01A}).
#'   \item \strong{values}: Non-negative numeric TPM values.
#' }
#'
#' @details
#' TPM matrices align sample-wise with \code{\link{gex_counts}} and gene-wise
#' with \code{\link{gex_data}}. TPM values are suitable for expression
#' visualization and cross-sample comparison of a gene’s abundance; avoid
#' mixing TPM and counts in the same statistical model.
#'
#' @source National Cancer Institute Genomic Data Commons (GDC), TCGA-COAD and
#' TCGA-READ; retrieved via \pkg{TCGAbiolinks}.
#'
#' @seealso \code{\link{gex_counts}}, \code{\link{gex_data}}, \code{\link{clinical_data}}
#'
#' @usage data(gex_tpm)
#'
#' @examples
#' data(gex_tpm)
#' summary(as.numeric(gex_tpm[, 1]))
"gex_tpm"
