#' Print Frequencytable results from frequency()
#'
#' This function controls printing for class frequencytable. It is the
#' same as print.data.frame except that row.names defaults to FALSE and
#' right defaults to FALSE. It also adds a title if this
#' was created when the frequencytable was created.
#' @param x The object to be printed.
#' @param row.names indicates if the row.names or numbers should be printed.
#' @param right Is a logical indicating if the values should be right aligned.
#' @param quote indicates if the string values should be returned enclosed in quotation marks.
#' @keywords data frequency count percent
#' @export


print.frequencytable <- function (x, ..., digits = NULL, quote = FALSE, right = FALSE,
          row.names = FALSE)
{
    title <- x[["title"]]
    x <- x[["table"]]
    n <- length(row.names(x))
    if (length(x) == 0L) {
        cat(sprintf(ngettext(n, "data frame with 0 columns and %d row",
                             "data frame with 0 columns and %d rows"), n),
            "\n",
            sep = "")
    }
    else if (n == 0L) {
        print.default(names(x), quote = FALSE)
        cat(gettext("<0 rows> (or 0-length row.names)\n"))
    }
    else {
        m <- as.matrix(format.data.frame(x, digits = digits,
                                         na.encode = FALSE))
        if (!isTRUE(row.names))
            dimnames(m)[[1L]] <- if (identical(row.names, FALSE))
                rep.int("", n)
        else row.names
        if (title != ""){
            title<-cat(title, sep ="\n")
            title
        }
        print(m, ..., quote = quote, right = right)
    }
    invisible(x)
}
