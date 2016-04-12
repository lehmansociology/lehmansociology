# Class for numeric data that has commas.

#' Assign Commas Class
#'
#' This function creates and assigns a custom class to handle numbers with commas.
#'
#' @param variable variable name that will be assigned the chr.w.commas class.
#' @keywords colClasses class
#' @export

assignCommasClass <- function(variable)
{
	#Deal with numbers that contain comma separaters for 1000s
	#http://stackoverflow.com/questions/25088144/how-to-load-df-with-1000-separator-in-r-as-numeric-class/25090565#25090565
	setClass("chr.w.commas", contains=numeric())
	setAs("character", "chr.w.commas", function(from)
		as.numeric(gsub("\\,", "",from )) )

	variable <- as(variable, "chr.w.commas")
	return( variable)

}
