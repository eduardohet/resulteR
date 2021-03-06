#' Generates strings of text for the use in markdown documents
#'
#' This function takes as input one object containing a test result and returns its most important informations. Function t.text takes the result of a t-test (returned from a call to the t.test function), returning its t-value, number of degrees of freedom (possibly corrected) and the test P-value. It offers options to customize the output, like changing separators and decimal markers (helpful in case you are producing a manuscript in German or Portuguese), and choosing the number of digits to round numbers to.
#'
#' @param x Name of the object containing the result of the analysis from which stats should be extracted
#' @param sep How to separate pieces of information displayed. Defaults to a semicolon followed by a space (sep = "; ").
#' @param dec Which decimal separator should be used? Defaults to ".". Allows to quickly changing to a comma in case you are producing a manuscript in German or Portuguese.
#' @param digits How many digits should be kept for each piece of numeric information? Defaults to c(3, 1, 3). Importantly, when a P-value rounded to the number of digits hereby defined results in zero, the result is replaced by a more theoretically meaningful alternative (e.g. P < 0.001).
#' @return A string of text to be included in a markdown object.
#' @export
#' 
#' @examples
#' res <- t.test(extra ~ group, data = sleep)
#' tText(res)
#' # Changing decimal marker in case you are producing a manuscript in German or Portuguese 
#' tText(res, dec=",")

tText <- function(x, sep="; ", dec="default", digits=c(3, 1, 3)){
  if (class(x) != "htest") stop("x is not an object of class 'htest' ")

  lis <- list()
  lis$stat <- round(x$statistic, digits[1])
  lis$df <- round(x$parameter, digits[2])
  p <- round(x$p.value, digits[3])
  lis$pval <- ifelse(p == 0, 
    paste0("P < ", 1/(10^digits[3])), paste0("P = ", p))
  out <- paste(
    paste0("t~", lis$df, "~ = ", lis$stat), 
    lis$pval, sep=sep)
  if(dec != "default") gsub(".", dec, out, fixed=T) else out
}