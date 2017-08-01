#' Print a frequency nicely in PDF or HTML
#'
#' @param freqtable frequency table object (as produced by lehmansociology::frequency)
#' @keywords frequency, print
#' @export

pretty_freq<-function(freqtable){
    require(pander)
    pander(freqtable$table)

}
