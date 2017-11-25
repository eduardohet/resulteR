#' Generates strings of text for the use in markdown documents
#'
#' This function takes as input one object containing a test result and returns its most important informations. Function adonisText takes the result of a Permanova (returned from a call to the adonis function from the vegan package), returning its F (with corresponding model and residuals degrees of freedom) and P-values for a selected variable in the result. In addition, it offers options to customize the output, changing separators and decimal markers (helpful in case you are producing a manuscript in German or Portuguese), and choosing the number of digits to round number to.
#'
#' @param x Name of the object containing the result of the analysis from which stats should be extracted.
#' @param sep How to separate pieces of information displayed. Defaults to a semicolon followed by a space (sep = "; ").
#' @param dec Which decimal separator should be used? Defaults to ".". Allows to quickly changing to a comma in case you are producing a manuscript in German or Portuguese.
#' @param digits How many digits should be kept for each piece of numeric information? Defaults to c(3, 3). Importantly, when a P-value rounded to the number of digits hereby defined results in zero, the result is replaced by a more theoretically meaningful alternative (e.g. P < 0.001).
#' @param which.coefs Vector indexing which of the potential many variables should be accessed when type = "coefs".
#' @return A string of text to be included in a markdown object.
#' @export
#' @examples
#' library(vegan)
#' data(dune)
#' data(dune.env)
#' res <- adonis2(dune ~ Management*A1, data = dune.env)
#' adonisText(res, which.coef=1)
#' adonisText(res, which.coef=3, dec=",")

adonisText <- function(x, sep="; ", dec=".", digits=c(3, 3), which.coef=1) {
  if (!"anova" %in% class(x)) stop("Not an object of class 'anova' ")
     
  tmp <- getVals(x, c("F", "Df", "Pr(>F)"))
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  if(length(which.coef) > 1){
    warning("Which.coef has length > 1 and only the first element will be used.")
    which.coef <- which.coef[1]
  }
  
  if(is.character(which.coef) & !which.coef %in% trim(rownames(tmp)))
    stop(paste0("Unkown variable = **", which.coef, "**"))
    
  if(is.numeric(which.coef) & (which.coef > nrow(tmp)))
    stop(paste0("which.coef = **", which.coef, "** exceeds the number of parameters in the model"))

  lis <- list()
  lis$f <- round(tmp[which.coef, 1], digits[1])
  lis$numdf <- round(tmp[which.coef, 2])
  lis$dendf <- round(tmp[nrow(tmp), 2])
  p <- round(tmp[which.coef, 3], digits[2])
  lis$pval <- ifelse(p == 0, 
    paste0("P < ", 1/(10^digits[2])), paste0("P = ", p))
  out <- paste(
    paste0("F ~", lis$numdf, ",~ ", "~", lis$dendf, "~ = ", 
      lis$f), 
    lis$pval, sep=sep)
      
  if(dec != ".") gsub(".", dec, out, fixed=T) else out
}