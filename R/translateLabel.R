#' Translate cell type labels from finer to coarser according to a dictionary.
#'
#' @param text Character vector.
#' @param dictionary Data frame.
#' @param dFrom Character.
#' @param dTo Character.
#'
#' @return A vector containing translated labels.
#'
#' @examples
#' dictionary <- data.frame(
#'   label.fine = c("CD4 T", "CD8 T", "Naive B"),
#'   label.main = c("T cell", "T cell", "B cell")
#' )
#' translateLabel(c("CD8 T", "Naive B", "CD4 T"), dictionary)
#' @export
translateLabel <- function(text, dictionary, dFrom = "label.fine", dTo = "label.main"){
  translated <- plyr::mapvalues(text,
                                from = dictionary[,dFrom],
                                to = dictionary[,dTo], warn_missing = FALSE)
  return(translated)
}
