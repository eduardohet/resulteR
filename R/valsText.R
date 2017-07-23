#' Rounds and changes decimal markers in single numeric values
#'
#' This function takes as input one numeric value, rounds and changes decimal markers whenever necessary.
#'
#' @param x Name of the object containing the numeric value.
#' @param dec Which decimal separator should be used? Defaults to ".". Allows to quickly changing to a comma in case you are producing a manuscript in German or Portuguese.
#' @param digits To how many digitis should the value be rounded to? Default is 3. 
#' @return A string of text to be used in markdown documents.
#' @export
#' @examples
#' x <- 5.46923
#' valsText(x, dec=",", digits=2)

valsText <- function(x, dec="default", digits=3){
  res <- round(x, digits)
  if(dec != "default") gsub(".", dec, res, fixed=T) else res
}