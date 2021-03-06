% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chisqText.R
\name{chisqText}
\alias{chisqText}
\title{Generates strings of text for the use in markdown documents}
\usage{
chisqText(x, df = "df", sep = "; ", dec = "default", digits = c(3, 3))
}
\arguments{
\item{x}{Name of the object containing the result of the analysis from which stats should be extracted.}

\item{df}{How to name degrees of freedom. Defaults to the shortened commonly used version "df".}

\item{sep}{How to separate pieces of information displayed. Defaults to a semicolon followed by a space (sep = "; ").}

\item{dec}{Which decimal separator should be used? Defaults to ".". Allows to quickly changing to a comma in case you are producing a manuscript in German or Portuguese.}

\item{digits}{How many digits should be kept for each piece of numeric information? Defaults to c(3, 3). Importantly, when a P-value rounded to the number of digits hereby defined results in zero, the result is replaced by a more theoretically meaningful alternative (e.g. P < 0.001).}
}
\value{
A string of text to be included in a markdown object.
}
\description{
This function takes as input one object containing the result of a chi-squared test (the result of either a call to chisq.test, fisher.test, or prop.test). It returns chi-square, degrees of freedom, and P-value, except when the test result is from a Fisher's exact test for which only the P-value is available. Function chisqText offers options to customize the output, like changing separators and decimal markers (helpful in case you are producing a manuscript in German or Portuguese), and choosing the number of digits to round numbers.
}
\examples{
x <- matrix(c(6, 10, 2, 20), ncol=2)
res <- chisq.test(x)
chisqText(res)
res <- fisher.test(x)
chisqText(res)
res <- prop.test(x)
chisqText(res)
}
