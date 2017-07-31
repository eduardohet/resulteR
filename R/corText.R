#' Generates strings of text for the use in markdown documents
#'
#' This function takes as input one object containing the result of a correlation test (from a call to the cor.test function) and returns its most important informations. The exact information returned depends on the method used for the correlation: Pearson's correlation returns r, t and degrees of freedom, and P-value; Spearman returns rho, S, and P-value; and Kendall returns tau, z, and P-value. Function corText offers options to customize the output, like changing separators and decimal markers (helpful in case you are producing a manuscript in German or Portuguese), and choosing the number of digits to round numbers.
#'
#' @param x Name of the object containing the result of the analysis from which stats should be extracted.
#' @param sep How to separate pieces of information displayed. Defaults to a semicolon followed by a space (sep = "; ").
#' @param dec Which decimal separator should be used? Defaults to ".". Allows to quickly changing to a comma in case you are producing a manuscript in German or Portuguese.
#' @param digits How many digits should be kept for each piece of numeric information? Defaults to c(3, 3, 3). Importantly, when a P-value rounded to the number of digits hereby defined results in zero, the result is replaced by a more theoretically meaningful alternative (e.g. P < 0.001).
#' @return A string of text to be included in a markdown object.
#' @export
#' 
#' @examples
#' res <- cor.test(~Postwt + Prewt, data=anorexia)
#' corText(res)
#' res <- cor.test(~Postwt + Prewt, data=anorexia, method="spearman")
#' corText(res)
#' res <- cor.test(~Postwt + Prewt, data=anorexia, method="kendall")
#' corText(res)

corText <- function(x, sep="; ", dec="default", digits=c(3, 3, 3)) {
  if (class(x) != "htest") 
    stop("x is not an object of class 'htest' ")
  
  stat.names <- c(names(x$estimate), names(x$statistic))
  stat.names[1] <- ifelse(stat.names[1] == "cor", "r = ",
    ifelse(stat.names[1] == "rho", "$\rho$ = ",
    ifelse(stat.names[1] == "tau", "$\tau$ = ", "? = ")))
  lis <- list()
  lis$cor <- round(x$estimate, digits[1])
  lis$stat <- round(x$statistic, digits[2])
  lis$df <- x$parameter
  stat.names[2] <- ifelse(stat.names[2] == "t", 
    paste0(stat.names[2], "~", lis$df, "~ = "), 
    paste0(stat.names[2], " = "))
  p <- round(x$p.value, digits[3])
  lis$pval <- ifelse(p == 0, 
    paste0("P < ", 1/(10^digits[3])), paste0("P = ", p))
  out <- paste(
    paste0(stat.names[1], lis$cor),
    paste0(stat.names[2], lis$stat), 
    lis$pval, sep=sep)
  if(dec != "default") gsub(".", dec, out, fixed=T) else out
}