\name{resulteR-package}
\alias{resulteR-package}
\alias{resulteR}
\docType{package}
\title{
Test Output for Reproducible Documents
}

\description{ The \pkg{resulteR} package aims to facilitate the production of reproducible documents. }

\details{
The \pkg{resulteR} package contains functions to extract information from the most common statistical analyses (regressions, Anova, t-tests, correlations, and &Chi;²-tests). Information extracted from tests and formatting applied follows the most common standards in scientific journals. Options to customize the output are also available.

The \pkg{resulteR} package is developed at GitHub
(\url{https://github.com/eduardohet/resulteR}). GitHub provides up-to-date information and forums for bug reports.

To see the preferable citation of the package, type
\code{citation("resulteR")}.  
}

\author{ The \pkg{resulteR} is developed by Eduardo L. Hettwer Giehl.
}
\examples{
# t-tests
res <- t.test(extra ~ group, data = sleep)
tText(res)
tText(res, dec=",")

# Regressions
utils::data(anorexia, package = "MASS")
anorex.1 <- lm(Postwt ~ Prewt, data = anorexia)
lmText(anorex.1)
lmText(anorex.1, type="coefs", which.coef="Prewt")

# Regression plots
plot(Postwt ~ Prewt, data = anorexia)
abline(anorex.1)
plotlmText(anorex.1, h=0.15, v=0.8, pos=4)

# Anova
anorex.1 <- aov(Postwt ~ Treat + Prewt, data = anorexia)
aovText(anorex.1, which.coef="Treat")
aovText(anorex.1, which.coef="Prewt")

# Correlations
res <- cor.test(~Postwt + Prewt, data=anorexia)
corText(res)
res <- cor.test(~Postwt + Prewt, data=anorexia, method="spearman")
corText(res)
res <- cor.test(~Postwt + Prewt, data=anorexia, method="kendall")
corText(res)

# Descriptive statistics
x <- rnorm(50, mean=10, sd=20)
meansdText(x)
meansdText(x, digits=c(3, 3))

# Neat lists 
# Hint: useful to grab names of significant parameters from a model result
x <- c("apples", "oranges", "grapes")
tidyList(x)
tidyList(c(2.3, 5.4, 3), dec=",", last="&")
}
\keyword{ package }
\keyword{ reproducibility }
\keyword{ markdown }
\keyword{ models }
\keyword{ anova }
\keyword{ htest }
\keyword{ regression }