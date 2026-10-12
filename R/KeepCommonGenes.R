#' Keep common genes of two SingleCellExperiment objects.
#'
#' @param reference SingleCellExperiment object.
#' @param query SingleCellExperiment object.
#' @param dots2hyphen Logic.
#'
#' @return A list containing updated SingleCellExperiment obejcts with matching features.
#'
#' @examples
#' ref <- scuttle::mockSCE(ncells = 20, ngenes = 10)
#' query <- scuttle::mockSCE(ncells = 20, ngenes = 10)
#' query <- query[3:10, ]
#' res <- KeepCommonGenes(ref, query)
#' nrow(res$reference)
#' nrow(res$query)
#' @export
KeepCommonGenes <- function(reference, query, dots2hyphen = FALSE){
  if(dots2hyphen){
    rownames(reference) <- gsub("\\.", "-", rownames(reference))
    rownames(query) <- gsub("\\.", "-", rownames(query))
  }
  commonGenes <- intersect(rownames(query), rownames(reference))
  reference <- reference[commonGenes, ]
  query <- query[commonGenes, ]
  return(list(reference=reference, query=query))
}
