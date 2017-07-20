#' Generates strings of text to add on existing plots
#'
#' This function takes as input one object containing a test result and returns its most important informations. Function plot.lm.text takes the result of a regression (returned from a call to the lm function), returning its R², F-value (with corresponding model and residuals degrees of freedom) and P-value in the somewhat confusing expression notation. The result is aimed to be used on existing plots. In addition, the function offer options to customize the output, like replacing names of some parameters, changing separators and decimal markers (helpful in case you are producing a manuscript in German or Portuguese), and choosing the number of digits to round number to.
#'
#' @param x Name of the object containing the result of the analysis from which stats should be extracted
#' @param sep How to separate pieces of information displayed. Defaults to a semicolon followed by a space (sep = "; ").
#' @param dec Which decimal separator should be used? Defaults to ".". Allows to quickly changing to a comma in case you are producing a manuscript in German or Portuguese.
#' @param digits How many digits should be kept for each piece of numeric information? Defaults to c(3, 3, 3). Importantly, when a P-value rounded to the number of digits hereby defined results in zero, the result is replaced by a more theoretically meaningful alternative (e.g. P < 0.001).
#' @return A string of text in R expression notation to be added on plots
#' @export
#' @examples
#' ## loading the anorexia dataset
#' utils::data(anorexia, package = "MASS")
#' anorex.1 <- lm(Postwt ~ Prewt, data = anorexia)
#' plot(Postwt ~ Prewt, data = anorexia)
#' text(locator(1), text.reg.sum(anorex.1))

plot.lm.text <- function(x, sep="; ", dec="default", digits=c(3, 3, 3)) {
  if (class(x) != "lm") stop("x is not an object of class 'lm' ")
  f <- summary(x)$fstatistic
  p <- round(pf(f[1], f[2], f[3], lower.tail=F), digits[3])
  p <- ifelse(p == 0, "P < 0.001", paste0("P = ", p))
  attributes(p) <- NULL
  lis <- list(r2 = round(summary(x)$r.squared, 3),
    fval=round(f[1], 3), dfnum=f[2], dfde=f[3],
    pval=p, sep=sep)
  if(dec != "default") lapply(lis, 
    function(x) gsub(".", dec, x, fixed=T))
      
  eq <- substitute(plain(R)^2~"="~r2*sep*~~plain(F)[dfnum][", "][dfde]~"="~fval*sep*~pval, lis)
  as.expression(eq)            
}

