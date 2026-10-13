#' Tune ncomp and nfeat by cross-validation
#'
#' Computes the K-fold cross-validation loss of a PhiSpace model over a grid
#' of numbers of components (`ncomp`) and then over a grid of numbers of
#' features per phenotype (`nfeat`), and returns the chosen values with the
#' selected features. In an interactive session the function plots each loss
#' curve and asks for a value unless `ncomp` or `nfeat` is given. In a
#' non-interactive session it chooses the `ncomp` with the lowest loss, and
#' `nfeat` must be given when `tune_nfeat = TRUE`.
#'
#' @param reference `SingleCellExperiment` object.
#' @param assayName Character.
#' @param phenotypes Character.
#' @param YY Matrix.
#' @param ncomp Integer. With `tune_ncomp = TRUE`, the value to use after the
#'   cross-validation instead of asking or taking the lowest loss. With
#'   `tune_ncomp = FALSE`, the value to use; `NULL` uses the number of
#'   columns of `YY`.
#' @param impScores Matrix.
#' @param center Logical.
#' @param scale Logical.
#' @param tune_ncomp Logical: compute the cross-validation loss for each
#'   value in the ncomp grid.
#' @param tune_nfeat Logical: compute the cross-validation loss for each
#'   value in the nfeat grid.
#' @param ncompLimits Vector.
#' @param ncompGridSize Integer.
#' @param ncompGrid Vector.
#' @param selectedFeat Vector.
#' @param nfeatLimits Vector.
#' @param nfeatGridSize Integer.
#' @param nfeatV Vector.
#' @param Kfolds Integer.
#' @param regMethod Character.
#' @param labelCode Character.
#' @param Ncores Integer.
#' @param seed Integer. The function sets the seed with [withr::local_seed()], so the session's random-number state is restored on exit.
#' @param nfeat Integer. With `tune_nfeat = TRUE`, the number of features
#'   per phenotype to use after the cross-validation instead of asking;
#'   required in a non-interactive session.
#' @param cellTypeThreshold Integer or NULL. If a positive integer, cell types with fewer than this many cells in the reference will be removed before tuning. Only used when `phenotypes` is provided. Default is `NULL` (no filtering).
#'
#' @return A list with `ncomp`, `nfeat`, `impScores` (feature importance
#'   scores), `selectedFeat` (selected features), and `tuneNcompErrs` and
#'   `tuneNfeatErrs` (data frames of the cross-validation loss for each grid
#'   value, or `NULL` when that parameter was not tuned).
#' @examples
#' set.seed(1)
#' ref <- logNormTransf(scuttle::mockSCE(ncells = 200, ngenes = 300))
#' # Outside an interactive session, nfeat must be given.
#' tuned <- tunePhiSpace(ref, assayName = "logcounts", phenotypes = "Cell_Cycle",
#'                       ncompGrid = 1:4, nfeatV = c(20, 50), Kfolds = 3,
#'                       nfeat = 50)
#' tuned$ncomp
#' tuned$tuneNcompErrs
#' @export
tunePhiSpace <- function(reference,
                         assayName = "logcounts",
                         phenotypes = NULL,
                         YY = NULL,
                         ncomp = NULL,
                         impScores = NULL,
                         #
                         center = TRUE,
                         scale = FALSE,
                         #
                         tune_ncomp = TRUE,
                         tune_nfeat = TRUE,
                         #
                         ncompLimits = c(1,100),
                         ncompGridSize = 20,
                         ncompGrid = NULL,
                         selectedFeat = NULL,
                         #
                         nfeatLimits = c(10, 15000),
                         nfeatGridSize = 30,
                         nfeatV = NULL,
                         #
                         Kfolds = 5,
                         regMethod = 'PLS',
                         labelCode = "-1,1",
                         Ncores = 1,
                         seed = 5202056,
                         cellTypeThreshold = NULL,
                         nfeat = NULL){

  if(tune_nfeat && is.null(nfeat) && !interactive()) stop(.nfeatNeeded)

  # Validate cellTypeThreshold
  if(!is.null(cellTypeThreshold)){
    if(!(length(cellTypeThreshold) == 1 && is.numeric(cellTypeThreshold) &&
         cellTypeThreshold == as.integer(cellTypeThreshold) && cellTypeThreshold > 0)){
      stop("cellTypeThreshold must be a positive integer or NULL.")
    }
  }

  # Filter rare cell types if cellTypeThreshold is set
  if(!is.null(cellTypeThreshold) && !is.null(phenotypes)){
    for(ph in phenotypes){
      cellTypeCounts <- table(colData(reference)[, ph])
      rareCellTypes <- names(cellTypeCounts[cellTypeCounts < cellTypeThreshold])
      if(length(rareCellTypes) > 0){
        message(
          "Removing cell types with fewer than ", cellTypeThreshold,
          " cells in '", ph, "': ",
          paste(rareCellTypes, " (n=", cellTypeCounts[rareCellTypes], ")",
                sep = "", collapse = ", ")
        )
        keepCells <- !(as.character(colData(reference)[, ph]) %in% rareCellTypes)
        reference <- reference[, keepCells]
      }
    }
    if(ncol(reference) == 0) stop("No cells remain after filtering rare cell types.")
  }

  if(tune_ncomp){

    tune_ncomp_res <- CVTune_ncomp(reference = reference,
                                   assayName = assayName,
                                   phenotypes = phenotypes,
                                   YY = YY,
                                   #
                                   center = center,
                                   scale = scale,
                                   #
                                   ncompLimits = ncompLimits,
                                   gridSize = ncompGridSize,
                                   ncompGrid = ncompGrid,
                                   selectedFeat = selectedFeat,
                                   #
                                   Kfolds = Kfolds,
                                   regMethod = regMethod,
                                   labelCode = labelCode,
                                   Ncores = Ncores,
                                   seed = seed,
                                   ncomp = ncomp)
    ncomp <- tune_ncomp_res$ncomp
    tuneNcompErrs <- tune_ncomp_res$tuneNcompErrs
  } else {

    tuneNcompErrs <- NULL

    if(is.null(phenotypes)){

      if(is.null(YY)) stop("ncomp and YY cannot be both NULL.")
    } else {

      YY <- codeY(reference, phenotypes)
    }

    if(is.null(ncomp)){

      ncomp <- ncol(YY)
    }
  }

  if(tune_nfeat){
    tune_nfeat_res <- CVTune_nfeat(reference = reference,
                                   assayName = assayName,
                                   phenotypes = phenotypes,
                                   YY = YY,
                                   ncomp = ncomp,
                                   impScores = impScores,
                                   #
                                   center = center,
                                   scale = scale,
                                   #
                                   nfeatLimits = nfeatLimits,
                                   gridSize = nfeatGridSize,
                                   nfeatV = nfeatV,
                                   #
                                   Kfolds = Kfolds,
                                   regMethod = regMethod,
                                   labelCode = labelCode,
                                   seed = seed,
                                   Ncores = Ncores,
                                   nfeat = nfeat)
    nfeat <- tune_nfeat_res$nfeat
    tuneNfeatErrs <- tune_nfeat_res$tuneNfeatErrs
    impScores <- tune_nfeat_res$impScores
    selectedFeat <- tune_nfeat_res$selectedFeat
  } else {
    tuneNfeatErrs <- NULL
    nfeat <- NULL
  }

  # If do not tune ncomp nor nfeat, simply calculate importance scores
  if(!tune_ncomp & !tune_nfeat){

    selectedFeat <- rownames(reference)

    impScores <- .coefSlice(
      mvr(
        X = t(assay(reference, assayName)),
        Y = YY,
        ncomp = ncomp,
        method = regMethod,
        center = center,
        scale = scale,
        keepComps = ncomp
      )$coefficients,
      ncomp
    )
  }



  return(list(
    ncomp = ncomp,
    nfeat = nfeat,
    impScores = impScores,
    selectedFeat = selectedFeat,
    tuneNfeatErrs = tuneNfeatErrs,
    tuneNcompErrs = tuneNcompErrs
  ))
}
