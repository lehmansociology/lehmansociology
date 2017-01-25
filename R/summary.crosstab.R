#' Summary for crosstab results from crosstab()
#'
#' This function uses the summary.table function to get a summary
#' of a crosstab object
#' @param x The crosstab object to be summarized.
#' @keywords data frequency count percent chisquare
#' @export
#'

summary.crosstab<-function(x) {

    summary(x[['tabf']])
}
