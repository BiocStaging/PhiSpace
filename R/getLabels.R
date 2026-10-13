#' Turn scores into labels
#'
#' Gives each row of a score matrix the name of the column with the highest
#' score, or the names of the `labPerSample` highest-scoring columns. This
#' function was called `getClass()` before PhiSpace 0.99.0; it was renamed
#' because that name masked `methods::getClass()`.
#'
#' @param X Matrix of scores, one row per sample (for example per cell) and
#'   one named column per label.
#' @param labPerSample Positive integer: the number of labels per row. The
#'   default `NULL` gives one label.
#'
#' @return With one label per row, a character vector of column names, one
#'   per row of `X`. With more, a character matrix with one row per row of
#'   `X`, labels in decreasing order of score.
#'
#' @examples
#' set.seed(1)
#' scores <- matrix(rnorm(20), 5, 4, dimnames = list(NULL, LETTERS[1:4]))
#' getLabels(scores)
#' getLabels(scores, labPerSample = 2)
#' @export
getLabels <- function(X, labPerSample = NULL){

  if(is.null(labPerSample)) labPerSample <- 1

  if(labPerSample <= 0) stop("labPerSample has to be a positive integer.")

  if(labPerSample == 1){

    out <- colnames(X)[apply(X, 1, which.max)]

  } else {

    out <- vapply(seq_len(nrow(X)),
                  function(x){
                    vec <- X[x,,drop=FALSE]
                    ct_sort <- colnames(vec)[order(-vec)]
                    ct_sort[seq_len(labPerSample)]
                    # colnames(X)[rank(-vec, ties.method = "last") <= labPerSample]
                  }, character(labPerSample))
    out <- t(out)

  }


  return(out)
}
