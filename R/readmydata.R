
#' Read My Data
#'
#' This function reads a webchip style dataset
#' @keywords
#' @export


readMyData <- function(datasetname)
{
	if (datasetname[1] == "")
	{
		return( warning("You haven't named a dataset"))
	}
	if (str_sub(datasetname, -4) != ".rds" )
	{
		datasetname<-paste0('./data/webchipstyle',datasetname, ".rds")
	}

	x <- readRDS(datasetname)
	if (is.null(x))
	{
		return(warning("No such data set. Check your spelling."))
	}

	return(x)
}
