#' Run scran normalisation
#'
#' Computes pooled size factors with `scran::computeSumFactors()` and
#' log-normalises the counts with [logNormTransf()].
#'
#' @param sce SCE object.
#' @param assayName Assay name.
#' @param clusts Cluster labels (optional).
#'
#' @return SCE object with the assay `logcounts` and the size factors, centred
#'   to mean 1, in `colData(sce)$sizeFactor`.
#' @examples
#' sce <- scuttle::mockSCE(ncells = 200, ngenes = 200)
#' # Pool cells within known groups to compute the size factors.
#' sce <- scranTransf(sce, clusts = sce$Cell_Cycle)
#' assayNames(sce)
#' @export
scranTransf <- function(
    sce,
    assayName = "counts",
    clusts = NULL
){

  if(is.null(clusts)) clusts <- scran::quickCluster(sce, assay.type = assayName)
  sce <- scran::computeSumFactors(sce, assay.type = assayName, cluster = clusts)
  # Replace non-positive size factors by the smallest positive one
  if(min(sce$sizeFactor) <= 0){
    smallestPositive <- min(sce$sizeFactor[sce$sizeFactor > 0])
    sce$sizeFactor[sce$sizeFactor <= 0] <- smallestPositive
  }
  sce <- logNormTransf(sce, assayName = assayName, sizeFactors = sce$sizeFactor)

  return(sce)
}
