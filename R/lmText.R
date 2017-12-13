#' Generates strings of text for the use in markdown documents
#'
#' This function takes as input one object containing a test result and returns its most important informations. Function lmText takes the result of a regression (returned from a call to the lm function), returning either its global statistics -- R², F-value (with corresponding model and residuals degrees of freedom) and P-value -- or statistics for specific coefficients. In addition, it offers options to customize the output, like replacing names of some parameters, changing separators and decimal markers (helpful in case you are producing a manuscript in German or Portuguese), and choosing the number of digits to round number to.
#'
#' @param x Name of the object containing the result of the analysis from which stats should be extracted.
#' @param type Either "global" for global model statistics or "coefs", for specific variable betas and their significance. The default is "global".
#' @param sep How to separate pieces of information displayed. Defaults to a semicolon followed by a space (sep = "; ").
#' @param dec Which decimal separator should be used? Defaults to ".". Allows to quickly changing to a comma in case you are producing a manuscript in German or Portuguese.
#' @param digits How many digits should be kept for each piece of numeric information? Defaults to c(3, 3, 3). Importantly, when a P-value rounded to the number of digits hereby defined results in zero, the result is replaced by a more theoretically meaningful alternative (e.g. P < 0.001).
#' @param adj.r.squared Logic value indicating whether an automatic shift to adjusted R² should be allowed for multivariate models. Default is TRUE.
#' @param which.coef Vector indexing which of the potential many variables should be accessed when type = "coefs".
#' @return A string of text to be included in a markdown object.
#' @export
#' @examples
#' ## loading the anorexia dataset
#' utils::data(anorexia, package = "MASS")
#' anorex.1 <- lm(Postwt ~ Prewt, data = anorexia)
#' lmText(anorex.1)
#' lmText(anorex.1, type="coefs", which.coef="Prewt")
#' lmText(anorex.1, type="coefs", which.coef=2)
#' anorex.2 <- lm(Postwt ~ Prewt + Treat, data = anorexia)
#' lmText(anorex.2)
#' lmText(anorex.2, type="coefs", which.coef="TreatCont")
#' lmText(anorex.2, type="coefs", which.coef=3)

lmText <- function(x, type="global", sep="; ", dec=".", digits=c(3, 3, 3), adj.r.squared=TRUE, which.coef=NULL) {
  if (class(x) != "lm") stop("Not an object of class 'lm' ")
  
  if(type=="global"){
  
    tmp <- summary(x)
    lis <- list()
    lis$f <- round(tmp$fstatistic[1], digits[2])
    lis$numdf <- tmp$fstatistic[2]
    lis$dendf <- tmp$fstatistic[3]
    lis$r <- ifelse((lis$numdf > 1 & adj.r.squared), 
      paste0("$\\bar{R}^{2}$ = ", round(tmp$adj.r.squared, 
        digits[1])),
      paste0("R^2^ = ", round(tmp$r.squared, digits[1])))
    p <- round(pf(tmp$fstatistic[1], lis$numdf, lis$dendf, lower.tail=F), digits[3])
    lis$pval <- ifelse(p == 0, 
      paste0("P < ", 1/(10^digits[3])), paste0("P = ", p))
    out <- paste(
      lis$r,
      paste0("F ~", lis$numdf, ",~ ", "~", lis$dendf, "~ = ", 
        lis$f), 
      lis$pval, sep=sep)

  } else {
    if(type=="coefs"){
      
      tmp <- summary(x)$coefficients
      
      if(length(which.coef) > 1){
        warning("which.coef has length > 1 and only the first element will be used.")
        which.coef <- which.coef[1]
      }
      
      if(is.character(which.coef) & !which.coef %in% rownames(tmp))
        stop(paste0("Unkown variable = **", which.coef, "**")) 
        
      if(is.numeric(which.coef) & nrow(tmp) < which.coef)
        stop(paste0("which.coef = **", which.coef, "** exceeds the number of parameters in the model"))

      lis <- list()
      lis$b <- round(tmp[which.coef, 1], digits[1])
      lis$t <- round(tmp[which.coef, 3], digits[2])
      p <- round(tmp[which.coef, 4], digits[3])
      lis$pval <- ifelse(p == 0, 
        paste0("P < ", 1/(10^digits[3])), paste0("P = ", p))
      out <- paste(
        paste0("*b* = ", lis$b),
        paste0("t = ", lis$t), 
        lis$pval, sep=sep)

    } else { stop(paste0("Unkown type = **", type, "**")) }
  }
  if(dec != ".") gsub(".", dec, out, fixed=T) else out
}