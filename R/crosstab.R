#' Function for creating a crosstabs
#'
#' This function creates crosstabs in a flexible manner.
#' @param data Data frame to be analyzed
#' @param formula  A formula for the table with a single row column on the left and column variables in the right
#' @param row.vars A single or vector of row variables, quoted. These are appended to any row variable from formula.
#' @param col.vars A single or vector of column variables, quoted. These are appended to any column variables from formula.
#' @param format  "freq", "col_percent", "row_percent", "total_percent"
#' @param useNa  "ifany", "no", "always"
#' @param title  string Title to be used for printing
#' @param row.margin.format  "percent" to show row percents, "freq" to show row frequency, "none" for no margin
#' @param col.margin.format "percent" to show percents, "freq" to show column frequencies, "none" for no margin
#' @param dnames vector Dimension names to replace the variable names.
#' @param pretty.pring logical TRUE means to add linebreaks for printing with Pander or another package.
#' @keywords crosstab
#' @export

crosstab<-function(data, formula = NULL,
                    row.vars = "",
                    col.vars = "",
                    format = "freq",
                    useNA = "ifany",
                    title = "",
                    row.margin.format = "none",
                    col.margin.format = "freq",
                    dnames = "",
                    pretty.print = FALSE
                    ){

    if (!is.null(formula)){
        formula_variables <- all.vars(formula)
        formula_row <- formula_variables[1]
        formula_column <- formula_variables[2:length(formula_variables)]

        if(row.vars == ""){
            row.vars <- formula_row
        } else {
            row.vars <- c(formula_row, row.vars)
        }
        if (col.vars == ""){
            col.vars <- formula_column
        } else {
            col.vars <- c(formula_column, col.vars)
        }
    }

    factorsToUse <- c(row.vars, col.vars)

    # Deal with case where row.vars or col.vars is empty.
    factorsToUse <- factorsToUse[factorsToUse != ""]
    nvars <- length(factorsToUse)
    if (nvars == 0 ){
        return("No variables were specified")
    }

    if (nvars == 1) {
        onedtable <- lehmansociology::frequency(data[factorsToUse])
        return(onedtable)
    }

    # Deal with spaces in variable name, but make it backward compatible.
    if (grepl(" ", factorsToUse) == TRUE && substr(factorsToUse, 1, 1) != "`") {
            factorsToUse<-paste0("`",factorsToUse,"`")
        } else {
            factorsToUse <- factorsToUse
        }

    form <- as.formula(paste(" ~", paste(factorsToUse, collapse="+"), sep=""))

    tab <- tabf <- xtabs( form, data = data)

    if (length(factorsToUse) > 2) {
       tab <- preprocess_multidimensional_tables(tab, row.vars, col.vars, pretty.print, nvars)
    }

    # Now process everything as though it is a two way table.
    tabn<-margin.table(tabf)
    margin.row.f<-margin.table(tabf,1)
    margin.row.p<-prop.table(margin.row.f)
    margin.col.f<-margin.table(tabf,2)
    margin.col.p<-prop.table(margin.col.f)
    margins<-list(row.freq = margin.row.f,
                  row.prop = margin.row.p,
                  col.freq = margin.col.f,
                  col.prop = margin.col.p)

    if (format == "column_percent" | format == "col_percent"){
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
    tab <- add_marginals(tab, tabn, row.margin.format, col.margin.format, margins)

     crosstab<-list(tabf=tabf,
                   tab = tab,
                   n = tabn,
                   title = title,
                   margins = margins,
                   factors = factorsToUse,
                   formula = formula
                    )
    class(crosstab)<-c("crosstab")

    crosstab

}

#' Add marginals
#' Add requested marginals to the table

add_marginals <- function(tab, tabn,  row.margin.format, col.margin.format, margins){

    if (row.margin.format != "none") {
        if (row.margin.format == 'percent'){
            tab$Total <- round(100*margins$row.prop, 1)
            names(tab)[names(tab)=="Total"] <- "Percent"
            tab$Percent <- as.character(tab$Percent)

        } else {
            tab$Total <- margins$row.freq
            tab$Total <- as.character(tab$Total)
        }

    }

    if (col.margin.format != "none") {
        if (col.margin.format == 'percent'){
            cm<-as.character(round(100*margins$col.prop, 1))
            rn<-"Row Percent"
        } else {
            cm<-margins$col.freq
            rn<-"Total N"
        }
     if (row.margin.format != "none"){
            cm<-c(cm, tabn)
        }

        tab<-rbind(tab,cm)

        rownames(tab)[nrow(tab)]<-rn
    }

    tab
}

#' Preprocess multidimensional tables
#' This converts multidimensional tables to two dimensions

preprocess_multidimensional_tables <- function(tab, row.vars, col.vars, pretty.print, nvars ){

    # this is for 3+ variables, basically we make it be 2 variables
    tab<-as.data.frame(tab)

    cname <- paste(col.vars, collapse = " ")
    rname<-paste(row.vars, collapse = " ")

    # Merge the labels for the column variables into a single label with line breaks.
    if (length(col.vars) > 1  ) {
        septouse = ifelse(pretty.print, "\\\n", " ")
        tab$colvars <-with(tab, paste(tab[,length(row.vars)+1],tab[,nvars], sep = septouse))

    } else {
        tab$colvars <-with(tab,tab[,length(row.vars) +1])
    }

    # Merge the labels for row variables into a single label with a space.
    if (length(row.vars) > 1){
        tab$rowvars <- paste(tab[,1], tab[,length(row.vars)], sep = " ")
    } else {
        tab$rowvars <- with(tab,tab[,row.vars])
    }

    names(dimnames(tab))<-list(rname, cname)
    form <- as.formula(paste0("Freq ~ rowvars + colvars"))
    tabf<-tab<-xtabs(form, tab)

}
