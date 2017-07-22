vals.text <- function(x, dec="default", digits=3){
  res <- round(x, digits)
  if(dec != "default") gsub(".", dec, res, fixed=T) else res
}
tidy.list <- function(x, pattern="\\1, e"){
  sub("(.*),", pattern, paste(x, collapse = ", "))
}