#' Function for printing a crosstab created by crosstab().
#'
#' This function creates crosstabs in a flexible manner.
#' @keywords crosstab printing
#' @export



#   http://rstudio-pubs-static.s3.amazonaws.com/6975_c4943349b6174f448104a5513fed59a9.html
#  Catspec is released under GPL >=2
#  Modified by Elin Waring

printCrosstab <- function(x,dec.places=x$dec.places,subtotals=x$subtotals,...) {

    ###################################################################################
    #                                                                                 #
    # Function created by Dr Paul Williamson, Dept. of Geography and Planning,        #
    # School of Environmental Sciences, University of Liverpool, UK.                  #
    #                                                                                 #
    # Adapted from the function print.ctab() in the catspec packge.                   #
    #                                                                                 #
    # Version: 12th July 2013                                                         #
    #                                                                                 #
    # Designed to provide optimal viewing of the output from crosstab()               #
    #                                                                                 #
    ###################################################################################


    row.vars <- x$row.vars
    col.vars <- x$col.vars
    n.row.vars <- length(row.vars)
    n.col.vars <- length(col.vars)
    n.vars <- n.row.vars + n.col.vars

    if (length(x$type)>1) {
        z<-length(names(dimnames(x$crosstab)))
        if (x$style=="long") {
            row.vars<-c(row.vars,z)
        } else {
            col.vars<-c(z,col.vars)
        }
    }

    if (n.vars==1) {
        if (length(x$type)==1) {
            tmp <- data.frame(round(x$crosstab,x$dec.places))
            colnames(tmp)[2] <- ifelse(x$type=="frequency","Count","%")
            print(tmp,row.names=FALSE)
        } else {
            print(round(x$crosstab,x$dec.places))
        }
    }


    #If table has only 2 dimensions, or subtotals required for >2 dimensional table,
    #print table using ftable() on x$crosstab
    if ((n.vars == 2) | ((subtotals==TRUE) & (n.vars>2))) {

        tbl <- ftable(x$crosstab,row.vars=row.vars,col.vars=col.vars)

        if (!all(as.integer(tbl)==as.numeric(tbl))) tbl <- round(tbl,dec.places)
        print(tbl,...)

    }

    #If subtotals NOT required AND > 2 dimensions, print table using write.table() on x$crosstab.nosub
    if ((subtotals==FALSE) & (n.vars>2))  {

        t1 <- x$crosstab.nosub

        #Convert numbers to required decimal places, right aligned
        width <- max( nchar(t1[1,]), nchar(t1[2,]), 7 )
        dec.places <- x$dec.places
        number.format <- paste("%",width,".",dec.places,"f",sep="")
        t1[3:nrow(t1),((n.row.vars+1):ncol(t1))] <- sprintf(number.format,as.numeric(t1[3:nrow(t1),((n.row.vars+1):ncol(t1))]))

        #Adjust column variable label to same width as numbers, left aligned, padding with trailing spaces as required
        col.var.format <- paste("%-",width,"s",sep="")
        t1[1,(n.row.vars+1):ncol(t1)] <- sprintf(col.var.format,t1[1,(n.row.vars+1):ncol(t1)])
        #Adjust column category labels to same width as numbers, right aligned, padding with preceding spaces as required
        col.cat.format <- paste("%",width,"s",sep="")
        t1[2,(n.row.vars+1):ncol(t1)] <- sprintf(col.cat.format,t1[2,(n.row.vars+1):ncol(t1)])

        #Adjust row labels so that each column is of fixed width, using trailing spaces as required
        for (i in 1:n.row.vars) {
            width <- max(nchar(t1[,i])) + 2
            row.lab.format <- paste("%-",width,"s",sep="")
            t1[,i] <- sprintf(row.lab.format,t1[,i])
        }

        write.table(t1,quote=FALSE,col.names=FALSE,row.names=FALSE)

    }

}
