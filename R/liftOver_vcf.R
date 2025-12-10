#' Liftover VCF Coordinates from GRCh37 to GRCh38
#'
#' This function lifts over variant coordinates in a VCF object using a chain
#' file.
#'
#' @param vcf A `VCF` object (i.e., when reading in VCF via
#' `VariantAnnotation::readVcf`)
#' containing variant data including coordinates.
#' @param chain_path A character string specifying the file path to the chain
#' file for conversion.
#' @return A `VCF` object with updated coordinates. Only variants that
#' successfully map to a single location are retained.
#' @note The `seqlevel` of the VCF will be converted to `UCSC` in the mapping
#' process.
#' @importFrom rtracklayer import.chain liftOver
#' @importFrom S4Vectors elementNROWS
#' @importFrom GenomeInfoDb seqlevelsStyle
#' @importFrom SummarizedExperiment rowRanges
#' @export
#'
liftover_vcf <- function(vcf, chain_path) {
  if (!requireNamespace("GenomeInfoDb", quietly = TRUE)) {
    stop("Package 'GenomeInfoDb' is required.")
  }
  if (!requireNamespace("rtracklayer", quietly = TRUE)) {
    stop("Package 'rtracklayer' is required.")
  }
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    stop("Package 'SummarizedExperiment' is required.")
  }
  if (!requireNamespace("S4Vectors", quietly = TRUE)) {
    stop("Package 'S4Vectors' is required.")
  }

  # Check to make sure seqlevel type is UCSC
  if (!GenomeInfoDb::seqlevelsStyle(vcf) == "UCSC") {
    GenomeInfoDb::seqlevelsStyle(vcf) <- "UCSC"
  }

  # Import chain file
  chain <- rtracklayer::import.chain(chain_path)

  # Perform liftover
  lifted_ranges <- rtracklayer::liftOver(
    SummarizedExperiment::rowRanges(vcf),
    chain
  )

  # Get variants that mapped to only one range
  idx <- S4Vectors::elementNROWS(lifted_ranges) == 1
  lifted_ranges <- unlist(lifted_ranges[idx])

  # Filter VCF for successfully mapped variants
  vcf_new <- vcf[idx, ]

  # Add newly updated ranges and return updated VCF
  SummarizedExperiment::rowRanges(vcf_new) <- lifted_ranges
  vcf_new
}
