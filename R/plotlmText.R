#' Generates strings of text to add on existing plots
#'
#' This function takes as input one object containing a test result and returns its most important informations. Function plotlmText takes the result of a regression (returned from a call to the lm function), returning its R², F-value (with corresponding model and residuals degrees of freedom) and P-value in the somewhat confusing expression notation. The result is aimed to be used on existing plots. In addition, the function offer options to customize the output, like replacing names of some parameters, changing separators and decimal markers (helpful in case you are producing a manuscript in German or Portuguese), and choosing the number of digits to round number to.
#'
#' @param x Name of the object containing the result of the analysis from which stats should be extracted.
#' @param sep How to separate pieces of information displayed. Defaults to a semicolon followed by a space (sep = "; ").
#' @param dec Which decimal separator should be used? Defaults to ".". Allows to quickly changing to a comma in case you are producing a manuscript in German or Portuguese.
#' @param digits How many digits should be kept for each piece of numeric information? Defaults to c(3, 3, 3). Importantly, when a P-value rounded to the number of digits hereby defined results in zero, the result is replaced by a more theoretically meaningful alternative (e.g. P < 0.001).
#' @param h Horizontal positioning of the information to be added. Increases from 0 (left) to 1 (right).
#' @param v Vertical positioning of the information to be added. Increases from 0 (bottom) to 1 (top).
#' @param ... Additional arguments to be passed to text function.
#' @return A string of text in R expression notation to be added on plots.
#' @export
#' @examples
#' ## loading the anorexia dataset
#' utils::data(anorexia, package = "MASS")
#' anorex.1 <- lm(Postwt ~ Prewt, data = anorexia)
#' plot(Postwt ~ Prewt, data = anorexia)
#' plotlmText(anorex.1)
#' # Previous example got a little bit off. The next settings should fix it.
#' plot(Postwt ~ Prewt, data = anorexia)
#' plotlmText(anorex.1, h=0.15, v=0.8, pos=4)

plotlmText <- function(x, sep="; ", dec=".", digits=c(3, 3, 3), h=0.5, v=0.85, ...) {
  if (class(x) != "lm") stop("x is not an object of class 'lm' ")
  f <- summary(x)$fstatistic
  p <- round(pf(f[1], f[2], f[3], lower.tail=F), digits[3])
  p <- ifelse(p == 0, "P < 0.001", paste0("P = ", p))
  attributes(p) <- NULL
  lis <- list(r2 = round(summary(x)$r.squared, digits[1]),
    fval=round(f[1], digits[2]), dfnum=f[2], dfde=f[3],
    pval=p, sep=sep)
  if(dec != ".") lapply(lis, 
    function(x) gsub(".", dec, x, fixed=T))
      
  eq <- substitute(plain(R)^2~"="~r2*sep*~~plain(F)[dfnum][", "][dfde]~"="~fval*sep*~pval, lis)
  text(x=grconvertX(h,'nfc'), y=grconvertY(v,'nfc'), as.expression(eq), ...)
}

