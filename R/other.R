vals.text <- function(x, dec="default", digits=3){
  res <- round(x, digits)
  if(dec != "default") gsub(".", dec, res, fixed=T) else res
}
tidy.lis <- function(x){
  sub("(.*),", "\\1, e", paste(x, collapse = ", "))
}