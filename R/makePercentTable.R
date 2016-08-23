#' Function for creating a crosstabs
#'
#' This function creates crosstabs in a flexible manner.
#' @keywords crosstab, pecents


makePercentTable <- function(tbl, type) {
    a <- length(row.vars)
    b <- length(col.vars)
    mrgn <- switch(type, column.pct = c(row.vars[-a], col.vars),
                   row.pct = c(row.vars, col.vars[-b]),
                   joint.pct = c(row.vars[-a], col.vars[-b]),
                   total.pct = NULL)
    tbl <- prop.table(tbl, mrgn)
    if (percentages) {
        tbl <- tbl * 100
    }
    tbl
}
