#' Generates strings of text for the use in markdown documents
#'
#' This function takes as input one object containing the result of a chi-squared test (the result of either a call to chisq.test, fisher.test, or prop.test). It returns chi-square, degrees of freedom, and P-value, except when the test result is from a Fisher's exact test for which only the P-value is available. Function chisqText offers options to customize the output, like changing separators and decimal markers (helpful in case you are producing a manuscript in German or Portuguese), and choosing the number of digits to round numbers. 
#'
#' @param x Name of the object containing the result of the analysis from which stats should be extracted.
#' @param df How to name degrees of freedom. Defaults to the shortened commonly used version "df".
#' @param sep How to separate pieces of information displayed. Defaults to a semicolon followed by a space (sep = "; ").
#' @param dec Which decimal separator should be used? Defaults to ".". Allows to quickly changing to a comma in case you are producing a manuscript in German or Portuguese.
#' @param digits How many digits should be kept for each piece of numeric information? Defaults to c(3, 3). Importantly, when a P-value rounded to the number of digits hereby defined results in zero, the result is replaced by a more theoretically meaningful alternative (e.g. P < 0.001).
#' @return A string of text to be included in a markdown object.
#' @export
#' 
#' @examples
#' x <- matrix(c(6, 10, 2, 20), ncol=2)
#' res <- chisq.test(x)
#' chisqText(res)
#' res <- fisher.test(x)
#' chisqText(res)
#' res <- prop.test(x)
#' chisqText(res)

chisqText <- function(x, df="df", sep="; ", dec="default", digits=c(3, 3)) {
  if (class(x) != "htest") 
    stop("x is not an object of class 'htest' ")
  
  if(grepl("Fisher's Exact Test", x$method)){
    p <- round(x$p.value, digits[2])
    out <- ifelse(p == 0, 
      paste0("P < ", 1/(10^digits[2])), paste0("P = ", p))
  } else {
    lis <- list()
    lis$stat <- round(x$statistic, digits[1])
    lis$df <- x$parameter
    p <- round(x$p.value, digits[2])
    lis$pval <- ifelse(p == 0, 
      paste0("P < ", 1/(10^digits[2])), paste0("P = ", p))
    out <- paste(
      paste0("&chi;² = ", lis$stat), 
      paste0(df, " = ", lis$df),
      lis$pval, sep=sep)
  }
  if(dec != "default") gsub(".", dec, out, fixed=T) else out
}
