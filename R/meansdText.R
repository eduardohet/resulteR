#' Generates strings of text for the use in markdown documents
#'
#' This function takes as input one vector and returns its mean and standard deviation in the most standard notation (mean ± sd).
#'
#' @param x A numeric vector
#' @param dec Which decimal separator should be used? Defaults to ".". Allows to quickly changing to a comma in case you are producing a manuscript in German or Portuguese.
#' @param digits How many digits should be kept for each piece of numeric information? Defaults to c(1, 1).
#' @return A string of text to be included in a markdown object.
#' @export
#' 
#' @examples
#' x <- rnorm(50, mean=10, sd=20)
#' meansdText(x)
#' # Rounding 
#' meansdText(x, digits=c(3, 3))

meansdText <- function(x, dec="default", digits=c(1, 1)){
  if (!is.numeric(x)) stop("x is not a numeric vector")
  res <- paste0(round(mean(x), digits[1]), " $ \\pm $ ", round(sd(x), digits[2]))
  if(dec != "default") gsub(".", dec, res, fixed=T) else res
}