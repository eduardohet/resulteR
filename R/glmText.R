#' Generates strings of text for the use in markdown documents
#'
#' This function takes as input one object containing a test result and returns its most important informations. Function glmText takes the result of a generalized linear model (returned from a call to the glm function), returning either its global statistics -- pseudo-R², F-value or chi-squared statistics with corresponding P-value -- or statistics for specific coefficients. In addition, it offers options to customize the output, like replacing names of some parameters, changing separators and decimal markers (helpful in case you are producing a manuscript in German or Portuguese), and choosing the number of digits to round numbers to.
#'
#' @param x Name of the object containing the result of the analysis from which stats should be extracted.
#' @param type Either "global" for global model statistics or "coefs", for specific variable betas and their significance. The default is "global".
#' @param test Whether the model provided should be constrasted to another possibly simpler alternative. Can be logic or may directly provide the test name for one option available in anova.glm.
#' @param contrast If test is required, the model provided is constrasted to a possibly simpler alternative. The default is the intercept-only model (contrast="~ 1"). Setting different models will change the output of delta-AIC and the deviance statistics.
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
#' anorex.1 <- glm(Postwt ~ Prewt, data = anorexia, family=Gamma(link="log"))
#' glmText(anorex.1, type="global")
#' glmText(anorex.1, type="global", contrast="~ Prewt", test=TRUE)
#' glmText(anorex.1, type="coefs", which.coef="Prewt")

glmText <- function(x, type="global", test=FALSE, contrast="~ 1", which.coef=NULL, sep="; ", dec=".", digits=c(3, 3, 3)) {
  if (!"glm" %in% class(x)) stop("Not an object of class 'glm' ")
  
  pseudo.r2 <- function(m){ 1-(m$deviance / m$null.deviance) }
  
  if(type=="global"){
  
    pr2 <- pseudo.r2(x)
    f <- formula(paste0(".", contrast))
    m0 <- update(x, f)
    if(test != FALSE){
      if(test == TRUE){
        test <- ifelse(tolower(x$family$family) %in% 
          c("gaussian", "quasibinomial", "quasipoisson", 
          "gamma"), "F", ifelse(tolower(x$family$family) %in% 
          c("poisson", "binomial"), "Chisq"))
      }
      res <- anova(m0, x, test=test)
      lis <- list()
      lis$r <- paste0("pseudo-R^2^ = ", round(pr2, digits[1]))
      lis$aic.diff <- round(abs(AIC(m0) - AIC(x)), digits[2])
      lis$dev <- round(res$Deviance, digits[2])
      p <- round(res$Pr[2], digits[3])
      lis$pval <- ifelse(p == 0, 
        paste0("P < ", 1/(10^digits[3])), paste0("P = ", p))
      out <- paste(
        lis$r,
        paste0("$\\delta$AIC = ", lis$aic.diff), 
        paste0("G^2^ = ", lis$aic.diff), 
        p, sep=sep)
    } else {
      lis <- list()
      lis$r <- paste0("pseudo-R^2^ = ", round(pr2, digits[1]))
      lis$aic.diff <- round(abs(AIC(m0) - AIC(x)), digits[2])
      out <- paste(
        lis$r,
        paste0("\\delta AIC = ", lis$aic.diff), sep=sep)
    }
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
      stat <- ifelse("t value" %in% colnames(tmp)[3], "t", "z")
      lis$b <- round(tmp[which.coef, 1], digits[1])
      lis$t <- round(tmp[which.coef, 3], digits[2])
      p <- round(tmp[which.coef, 4], digits[3])
      lis$pval <- ifelse(p == 0, 
        paste0("P < ", 1/(10^digits[3])), paste0("P = ", p))
      out <- paste(
        paste0("*b* = ", lis$b),
        paste0(stat, " = ", lis$t), 
        lis$pval, sep=sep)

    } else { stop(paste0("Unkown type = **", type, "**")) }
  }
  if(dec != ".") gsub(".", dec, out, fixed=T) else out
}