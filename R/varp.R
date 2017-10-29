#' The "population" variance
#'
#' Calculates the variance with n as the denominator.
#'
#' @param x A vector for which a variance can be calculated
#' @param y NULL (default) or a vector, matrix or data frame
#'        with compatible dimensions to x. The default is equivalent to y = x (but more efficient).
#' @param na.rm	logical. Should missing values be removed?
#' @param use an optional character string giving a method for computing covariances in the presence of
#'         missing values. This must be (an abbreviation of) one of the strings "everything", "all.obs",
#'         "complete.obs", "na.or.complete", or "pairwise.complete.obs".
#' @keywords variance, population
#' @export

varp<-function(x, y = NULL, na.rm = FALSE, use){

    n <- sum(!is.na(x))
    varp <- ifelse(n != 0,  var(x)*(n-1)/n, return("All values are missing"))
    varp

}
