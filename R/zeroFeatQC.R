#' Minimal quality control (QC): remove features with all zero values.
#'
#' @param sce SingleCellExperiment object.
#' @param assayName Character.
#' @param verbose Logical. Whether to report the result with [message()]
#'   (default: TRUE).
#'
#' @return An updated SingleCellExperiment object after minimal QC.
#'
#' @examples
#' sce <- scuttle::mockSCE(ncells = 20, ngenes = 10)
#' counts(sce)[1:2, ] <- 0
#' sce <- zeroFeatQC(sce)
#' dim(sce)
#' @export
zeroFeatQC <- function(sce, assayName = "counts", verbose = TRUE){

  geneSpars <- rowMeans(assay(sce, assayName) == 0)
  if(max(geneSpars) == 1){
    sce <- sce[geneSpars < 1, ]
    if(verbose) message("Deleted features with all zeros.")
  } else {
    if(verbose) message("All features have at least 1 nonzero value.")
  }

  return(sce)
}
