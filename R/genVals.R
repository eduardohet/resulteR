#' Generates strings of text for the use in markdown documents

getVals <- function(x, 
  chunk.names=c("statistic", "p.value")) {
  if (length(x) < 1)
    stop("x has length < 1")
  sapply(chunk.names, function(nm)
    sapply(list(x), "[[", nm))
}

