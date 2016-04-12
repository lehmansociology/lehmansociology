#' Replace Commas Function
#'
#' This function converts a character representation of a number that contains a comma separator with a numeric value.
#' @keywords read data
#' @export
replaceCommas<-function(x){
  x<-as.numeric(gsub("\\,", "", x))
}
