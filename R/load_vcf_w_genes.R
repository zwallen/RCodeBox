#' Load a VCF into R with automatic gene name annotation
#'
#' @description
#' Imports a VCF into R and adds gene names (HGNC symbols) to the INFO field of
#' a VCF by intersecting variant coordinates with gene intervals downloaded
#' from the Ensembl database.
#'
#' @param vcf_path
#' Path to the input VCF file.
#' @param output_path
#' Output file path for outputting the annotated VCF file. If NULL (default),
#' the funtion outputs an annotated `VariantAnnotation::VCF` object.
#' @param GRCh
#' GRCh version of the VCF genomic positions. Currently this can only be set
#' to `37` (default is NULL, i.e., use current GRCh38 build).
#' @param exclude_antisense
#' Whether or not to exclude antisense versions of genes (i.e., those with a
#' "-AS1", "-AS2", etc. designation).
#'
#' @return
#' If `output_path` is `NULL`, an annotated `VariantAnnotation::VCF` object;
#' otherwise nothing is returned and annotated VCF is written to `output_path`.
#'
#' @importFrom VariantAnnotation readVcf writeVcf info header
#' @importFrom SummarizedExperiment rowRanges
#' @importFrom biomaRt useEnsembl getBM
#' @importFrom GenomicRanges makeGRangesFromDataFrame findOverlaps mcols
#' @importFrom GenomeInfoDb seqlevelsStyle
#' @importFrom S4Vectors DataFrame
#' @export
#'
load_vcf_w_genes <- function(
  vcf_path,
  output_path = NULL,
  GRCh = NULL,
  exclude_antisense = FALSE
) {
  if (!requireNamespace("VariantAnnotation", quietly = TRUE)) {
    stop("Package 'VariantAnnotation' is required.")
  }
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    stop("Package 'SummarizedExperiment' is required.")
  }
  if (!requireNamespace("biomaRt", quietly = TRUE)) {
    stop("Package 'biomaRt' is required.")
  }
  if (!requireNamespace("GenomeInfoDb", quietly = TRUE)) {
    stop("Package 'GenomeInfoDb' is required.")
  }

  # Load VCF and get variant ranges
  vcf <- VariantAnnotation::readVcf(vcf_path)
  variant_ranges <- SummarizedExperiment::rowRanges(vcf)

  # Connect to Ensembl BioMart
  mart <- biomaRt::useEnsembl("genes", "hsapiens_gene_ensembl", GRCh = GRCh)

  # Get gene positions and HGNC symbols from BioMart
  gene_df <- biomaRt::getBM(
    attributes = c(
      "chromosome_name",
      "start_position",
      "end_position",
      "hgnc_symbol"
    ),
    mart = mart
  )

  # Remove entries with missing HGNC symbols and antisense genes (if requested)
  gene_df <- gene_df[gene_df[["hgnc_symbol"]] != "", ]
  if (exclude_antisense) {
    gene_df <- gene_df[!grepl("-AS[0-9]*$", gene_df[["hgnc_symbol"]]), ]
  }

  # Create a GRanges object from gene information
  gene_annotation <- GenomicRanges::makeGRangesFromDataFrame(
    gene_df,
    seqnames.field = "chromosome_name",
    start.field = "start_position",
    end.field = "end_position",
    keep.extra.columns = TRUE
  )

  # Match seqlevels style of genes to the variants
  GenomeInfoDb::seqlevelsStyle(
    gene_annotation
  ) <- GenomeInfoDb::seqlevelsStyle(variant_ranges)

  # Find overlapping genomic positions between genes and variants
  overlaps <- suppressWarnings(
    GenomicRanges::findOverlaps(variant_ranges, gene_annotation)
  )

  # Annotate variants with gene names
  variant_gene_list <- split(
    S4Vectors::mcols(gene_annotation)[[
      "hgnc_symbol"
    ]][S4Vectors::subjectHits(overlaps)],
    S4Vectors::queryHits(overlaps)
  )

  # Prepare a character vector of gene symbols for each variant
  gene_symbols <- rep(NA, length(variant_ranges))
  gene_symbols[as.integer(names(variant_gene_list))] <- sapply(
    variant_gene_list,
    function(x) paste(unique(x), collapse = ",")
  )

  # Add header information for HGNC annotation and
  # add HGNC annotation to INFO fields
  VariantAnnotation::info(VariantAnnotation::header(vcf)) <- rbind(
    VariantAnnotation::info(VariantAnnotation::header(vcf)),
    S4Vectors::DataFrame(
      Number = "1",
      Type = "String",
      Description = "HGNC symbol for gene",
      row.names = "HGNC"
    )
  )
  VariantAnnotation::info(vcf)[["HGNC"]] <- gene_symbols

  # Output
  if (is.null(output_path)) {
    return(vcf)
  } else {
    VariantAnnotation::writeVcf(vcf, output_path)
  }
}
