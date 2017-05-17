#' Print a crosstab nicely in PDF or HTML
#'
#' @keywords crosstab, pring
#' @export

pretty_tab<-function(ctab){
    library(pander)
    pander(ctab$tab, keep.line.breaks = TRUE, justify = "right")

}
