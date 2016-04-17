#' Variation Function
#'
#' This function calculates a number of measures of variation for numeric and integer variables and returns them as a table.
#' Note that this uses the unbiased estimates of variance and standard deviation.
#' @keywords variation
#' @export
#' @examples
#' variation(iris$Sepal.Petal)

variation<-function(x){

  if (is.numeric(x) == FALSE){
    warning("Variable is not numeric. The measures cannot be calculated", call. = FALSE)
    return()
  }
  r<-range(x, na.rm=TRUE)
  range<-r[2]-r[1]
  iqr<-IQR(x, na.rm=TRUE)
  mad<-mad(x, center = median(x, na.rm = TRUE), na.rm = TRUE)
  aad<-aad(x, na.rm = TRUE)
  sd<-sd(x, na.rm = TRUE)
  variance<-var(x)
  variation_results<-t(c(range, iqr, mad, aad, sd))
  colnames(variation_results)<-c("Range", "IQR", "MedianAbsoluteDeviation", "MeanAbsoluteDeviation", "StandardDeviation", "Variance")
  variation_results<-as.table(variation_results)
  variation_results
}
