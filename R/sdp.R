#' The "population" standard deviation
#'
#' Calculates the standarddeviation with n as the denominator.
#'
#' @param x A vector for which a variance can be calculated
#' @param na.rm	logical. Should missing values be removed?
#' @keywords standard deviation, population
#' @export

sdp<-function(x, na.rm = FALSE){

 sqrt(varp(x))

}
