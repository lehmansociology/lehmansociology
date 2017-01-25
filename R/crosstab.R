#' Function for creating a crosstabs
#'
#' This function creates crosstabs in a flexible manner.
#' @param data dataframe to be analyzed
#' @param row.vars A single or vector of row variables, quoted.
#' @param col.vars A single or vector of column variables, quoted
#' @param format  "freq", "col_percent", "row_percent", "total_percent"
#' @param useNa  "ifany", "no", "always"
#' @param title  string Title to be used for printing
#' @param row.margin.format  "percent" to show row percents, "freq" to show row frequency, "none" for no margin
#' @param col.margin.format "percent" to show percents, "freq" to show column frequencies, "none" for no margin
#' @keywords crosstab
#' @export


crosstab<-function(data, row.vars = "", col.vars = "",
                    format = "freq", useNA = "ifany",
                    title = "", row.margin = TRUE,
                    row.margin.format = "none", col.margin.format = "freq"
                    ){
    factorsToUse<-c(row.vars, col.vars)
    if  (length(factorsToUse == 2)){
        form<-as.formula(paste(" ~", paste(factorsToUse, collapse="+"), sep=""))
        tab<-tabf<-xtabs( form, data = data)

        tabn<-margin.table(tabf)
        margin.row.f<-margin.table(tabf,1)
        margin.row.p<-prop.table(margin.row.f)
        margin.col.f<-margin.table(tabf,2)
        margin.col.p<-prop.table(margin.col.f)
        margins<-list(row.freq = margin.row.f,
                      row.prop = margin.row.p,
                      col.freq = margin.col.f,
                      col.prop = margin.col.p)
        if (format == "column_percent"){
            tab<-round(prop.table(tabf, 2)*100, 1)
        } else if (format == "row_percent"){
            tab<-round(prop.table(tabf, 1)*100, 1)
        } else if (format == "total_percent"){
            tab<-round(100*tabf/sum(tabf), 1)
        }
        # Change to a data frame to make it more flexible
        tab<-as.data.frame.matrix(tab)
        for(i in c(1:ncol(tab))) {
            tab[,i] <- as.character(tab[,i])
        }

        # Add requested marginals
        if (row.margin.format != "none") {
            if (row.margin.format == 'percent'){
               tab$Total <- round(100*margin.row.p, 1)
            } else {
                tab$Total <- margin.row.f
            }
            tab$Total <- as.character(tab$Total)
        }
         if (col.margin.format != "none") {
             if (col.margin.format == 'percent'){
                 cm<-as.character(round(100*margin.col.p, 1))
                 rn<-"Row Percent"
             } else {
                 cm<-margin.col.f
                 rn<-"Total N"
             }
            if (row.margin){
                cm<-c(cm, tabn)
            }
             tab<-rbind(tab,cm)
             rownames(tab)[nrow(tab)]<-rn

        }
        crosstab<-list(tabf=tabf,
                       tab = tab,
                       n = tabn,
                       title = title,
                       margins = margins
                        )
        class(crosstab)<-"crosstab"
        return(crosstab)
    }

}
