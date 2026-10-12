#' Dicretise soft annotation to labels.
#'
#' @param X Matrix.
#' @param labPerSample Integer.
#'
#' @return A vector contaning labels.
#'
#' @examples
#' set.seed(1)
#' scores <- matrix(rnorm(20), 5, 4, dimnames = list(NULL, LETTERS[1:4]))
#' getClass(scores)
#' getClass(scores, labPerSample = 2)
#' @export
getClass <- function(X, labPerSample = NULL){

  if(is.null(labPerSample)) labPerSample <- 1

  if(labPerSample <= 0) stop("labPerSample has to be postive integer.")

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
