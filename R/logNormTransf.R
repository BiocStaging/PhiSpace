#' Log-normalise counts with size factors
#'
#' Divides the counts of each cell by its size factor and takes
#' \eqn{\log_2(x + 1)}. By default the size factors are the library sizes,
#' centred to mean 1. This gives the same values as the defaults of
#' `scuttle::logNormCounts()`, which Bioconductor 3.24 deprecates.
#'
#' @param sce A `SingleCellExperiment` or `SummarizedExperiment` object.
#' @param assayName Name of the count assay.
#' @param targetAssay Name of the new assay.
#' @param sizeFactors Optional positive size factors, one per cell. The
#'   default `NULL` uses the library sizes (column sums of the counts).
#'
#' @return `sce` with the log-normalised assay `targetAssay` and the size
#'   factors, centred to mean 1, in `colData(sce)$sizeFactor`. A sparse or
#'   `DelayedArray` count matrix gives a `dgCMatrix`, a dense one a dense
#'   matrix.
#'
#' @seealso [scranTransf()] for pooled size factors, [logTransf()] for
#'   `log1p()` of counts or relative counts.
#' @examples
#' sce <- scuttle::mockSCE(ncells = 20, ngenes = 10)
#' sce <- logNormTransf(sce)
#' assay(sce, "logcounts")[1:3, 1:3]
#' @export
logNormTransf <- function(
    sce,
    assayName = "counts",
    targetAssay = "logcounts",
    sizeFactors = NULL
){

  x <- assay(sce, assayName)
  if(is.null(sizeFactors)) sizeFactors <- Matrix::colSums(x)
  if(length(sizeFactors) != ncol(x)){
    stop("sizeFactors must have one value per cell.")
  }
  if(any(!is.finite(sizeFactors) | sizeFactors <= 0)){
    stop("Size factors must be positive; remove cells with no counts.")
  }
  sizeFactors <- sizeFactors / mean(sizeFactors)

  # log1p(y) / log(2), as scuttle computes it, gives identical values
  if(methods::is(x, "DelayedArray")){
    x <- methods::as(x, "dgCMatrix")
  } else if(methods::is(x, "sparseMatrix")){
    x <- methods::as(methods::as(methods::as(x, "dMatrix"), "generalMatrix"),
                     "CsparseMatrix")
  }
  if(methods::is(x, "dgCMatrix")){
    x@x <- log1p(x@x / rep(sizeFactors, diff(x@p))) / log(2)
  } else {
    x <- log1p(sweep(as.matrix(x), 2, sizeFactors, "/")) / log(2)
  }

  assay(sce, targetAssay) <- x
  sce$sizeFactor <- sizeFactors
  return(sce)
}
