
#' Frequency
#'
#' This function creates a single variable frequency table
#' @param Values The variable data for which the table will be created
#' @param freq logical indicating if the freqencies should be displayed
#' @param percent logical indicating if the percents should be displayed
#' @param cumulative.percent logical indicating if the cumulative percents should be displayed
#' @param cumulative.freq logical indicating if the cumulative frequencies should be displayed
#' @param useNA  Handling of NAs, choices are "no", "ifany", "always". Defaults to no.
#' @param title Optional title, enclosed in quotes. Defaults to empty.
#' @keywords data frequency count percent
#' @export

# Allow changing of label
frequency <- function(Values,
                      freq = TRUE,
                      percent = TRUE,
                      cumulative.percent = FALSE,
                      cumulative.freq = FALSE,
                      useNA = "no",
                      title = ""){
    tabf<-table(Values, useNA=useNA)

    if (is.null(dimnames(tabf)$Values)){
        return(print("Object does not exist"))
    }
    tabp<-prop.table(tabf)
    tabp100<-round(tabp*100,1)
    tabmf<-margin.table(tabf)
    tabmp<-margin.table(tabp100)
    tabmp100 <- tabmp*100
    totrow<-c("Total", tabmf, tabmp)
    table<-merge(as.data.frame(tabf, stringsAsFactors = FALSE),
                 as.data.frame(tabp100, responseName = "Percent"))

    if (is.numeric(Values)){
        table$Values <- as.numeric(table$Values)
        table <- table[order(table$Values),]
    }

    if (cumulative.percent) {
        table$`Cum. Percent`<- cumsum(table$Percent)

        totrow<-c(totrow,  "")
    }
    if (cumulative.freq) {
        table$`Cum. Freq`<- cumsum(tabf)

        totrow<-c(totrow,  "")
    }
    table<-rbind(table, totrow)

    if (!freq & !percent){
        table<-table[,c(-2, -3)]
    } else {
        if  (freq == FALSE){
            table<-table[,c(-2)]
        } else {
            if (percent == FALSE){
            table<-table[,c(-3)]
    }}}

    t<-list(table = table,
            frequencies = tabf,
            proportions = tabp,
            cum_prop = cumsum(tabp),
            cum_freq = cumsum(tabf),
            totrow = totrow,
            title = title
            )

    class(t)<-"frequencytable"
    t
}
