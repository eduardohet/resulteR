#' Generates strings of text for the use in markdown documents
#'
#' This function takes as input one object containing the result of a one- and two-sample Wilcoxon tests on vectors of data (from a call to the wilcox.test function) and returns its most important informations. The exact information returned depends on the method used for the correlation: Pearson's correlation returns r, t and degrees of freedom, and P-value; Spearman returns rho, S, and P-value; and Kendall returns tau, z, and P-value. Function corText offers options to customize the output, like changing separators and decimal markers (helpful in case you are producing a manuscript in German or Portuguese), and choosing the number of digits to round numbers to.
#'
#' @param x Name of the object containing the result of the analysis from which stats should be extracted.
#' @param df Allows to included the number of degrees of freedom there were in the data. This information cannot be retrieved from the object containing the result of a call to wilcox.test. For two-sample cells it seems to be n1+n2, but (n1+n2)/2 for a paired test.
#' @param sep How to separate pieces of information displayed. Defaults to a semicolon followed by a space (sep = "; ").
#' @param dec Which decimal separator should be used? Defaults to ".". Allows to quickly changing to a comma in case you are producing a manuscript in German or Portuguese.
#' @param digits How many digits should be kept for each piece of numeric information? Defaults to c(3, 3). Importantly, when a P-value rounded to the number of digits hereby defined results in zero, the result is replaced by a more theoretically meaningful alternative (e.g. P < 0.001).
#' @return A string of text to be included in a markdown object.
#' @export
#' 
#' @examples
#' res <- wilcox.test(anorexia$Postwt, anorexia$Prewt)
#' wilcoxText(res, df=length(anorexia$Postwt) + 
#'   length(anorexia$Prewt))
#' res <- wilcox.test(anorexia$Postwt, anorexia$Prewt, paired=T)
#' wilcoxText(res, df=length(anorexia$Postwt))

wilcoxText <- function(x, df=NULL, sep="; ", dec="default", digits=c(3, 3)) {
  if (class(x) != "htest") 
    stop("x is not an object of class 'htest' ")
  
  lis <- list()
  lis$df <- x$parameter
  stat.name <- names(x$statistic)
  lis$stat.name <- paste0(stat.name, 
    ifelse(is.numeric(df), paste0(" ~", df, "~"), ""), " = ")
  lis$stat <- paste0(lis$stat.name, 
    round(x$statistic, digits[1]))
  p <- round(x$p.value, digits[2])
  lis$pval <- ifelse(p == 0, 
    paste0("P < ", 1/(10^digits[2])), paste0("P = ", p))
  out <- paste(lis$stat, lis$pval, sep=sep)
  if(dec != "default") gsub(".", dec, out, fixed=T) else out
}