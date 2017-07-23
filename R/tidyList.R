#' Generates a neat list of elements
#'
#' This function takes as input one character vector and constructs a meaningful text. By default it results in a list with elements separated by commas and the last element separated by ", and".
#'
#' @param x Name of the object containing character vector.
#' @param sep How to separate pieces of information displayed. Defaults to a commas followed by a space.
#' @param last How to separate the last element. Defaults to "and".
#' @return A string of text to be used in markdown documents.
#' @export
#' @examples
#' x <- c("apples", "oranges", "grapes")
#' tidyList(x)
#' tidyList(c(2.3, 5.4, 3), dec=",")

tidyList <- function(x, sep=",", last="and", dec="."){
  num <- is.numeric(x)
  if(length(x) > 2) {
    res <- sub(paste0("(.*)",sep), 
      paste0("\\1", sep, " ", last), 
      paste(x, collapse=paste0(sep, " ")))
    if(num & dec != ".") 
      gsub(".", dec, res, fixed=T) else res
  } else {
    if(length(x) == 2){ paste(x, collapse=paste0(" ", last, " ")) }
      else x
  }
}