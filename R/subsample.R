#' Subsample from a sce object
#'
#' @param sce SCE object to be sampled from
#' @param key Required for stratefied subsampling
#' @param proportion Proportion of cells to sample
#' @param minCellNum Minimum number of cells in each category
#' @param seed Random seed. The function sets the seed with [withr::local_seed()], so the session's random-number state is restored on exit.
#'
#' @return A downsized SCE object
#' @examples
#' sce <- scuttle::mockSCE(ncells = 200, ngenes = 10)
#' small <- subsample(sce, proportion = 0.2)
#' ncol(small)
#' # Stratified by a colData column, keeping at least 20 cells per class.
#' strat <- subsample(sce, key = "Cell_Cycle", proportion = 0.1, minCellNum = 20)
#' table(strat$Cell_Cycle)
#' @export
subsample <- function(
    sce,
    key = NULL,
    proportion = 0.1,
    minCellNum = 50,
    seed = 5202056
  ){

  if(is.null(key)){

    sceSize <- ncol(sce)
    outSize <- max(1, ceiling(sceSize * proportion) )

    withr::local_seed(seed)
    idx <- sample(
      seq_len(ncol(sce)),
      outSize
    )

  } else {


    # Stratefied subsampling
    keys <- as.character(colData(sce)[,key])
    keyNames <- unique(keys)

    withr::local_seed(seed)
    idx_list <- lapply(
      keyNames,
      function(keyName){

        fullIdx <- which(keys == keyName)
        fullIdxSize <- length(fullIdx)
        # How many to sample should not be smaller than minCellNum
        subIdxSize <- max(minCellNum, ceiling(fullIdxSize * proportion))
        # But also should not be larger than total number of cells in that category
        subIdxSize <- min(subIdxSize, fullIdxSize)
        subIdx <- sample(fullIdx, subIdxSize)

        return(subIdx)
      }
    )
    idx <- unlist(idx_list)

  }


  return(sce[,idx])
}
