#' Gene annotation for TCGA COAD/READ expression data
#'
#' Annotation describing the genes present in the expression matrices
#' (\code{gex_counts} and \code{gex_tpm}).
#'
#' @format A data frame with one row per gene. Typical columns include:
#' \itemize{
#'   \item \code{ensembl_gene_id} (character): Ensembl gene identifier (e.g., \code{ENSG00000141510}); version may be stripped.
#'   \item \code{gene_symbol} (character): HGNC-approved gene symbol.
#'   \item \code{gene_biotype} (character): Biotype (e.g., \code{protein_coding}, \code{lncRNA}).
#'   \item \code{entrez_id} (character/integer): NCBI Entrez Gene ID where available.
#'   \item \code{chromosome} (character): Chromosome or scaffold name.
#'   \item \code{start}, \code{end} (integer): Genomic coordinates (build as provided by GDC harmonization).
#'   \item \code{strand} (character): \code{"+"} or \code{"-"}.
#'   \item \code{...}: Additional annotation fields as provided by the pipeline.
#' }
#'
#' @details
#' This table provides the mapping needed to interpret rows of
#' \code{\link{gex_counts}} and \code{\link{gex_tpm}}. Gene identifiers
#' correspond to the GDC harmonized annotation used for TCGA RNA-seq.
#'
#' @source National Cancer Institute Genomic Data Commons (GDC), TCGA-COAD and
#' TCGA-READ; retrieved via \pkg{TCGAbiolinks}.
#'
#' @seealso \code{\link{gex_counts}}, \code{\link{gex_tpm}}, \code{\link{clinical_data}}
#'
#' @examples
#' data(gex_data)
#' head(gex_data)
"gex_data"