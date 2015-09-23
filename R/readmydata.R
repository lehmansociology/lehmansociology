
#' Read My Data Function
#'
#' This function reads a stored rds dataset, harcoded to the data folder
#' @keywords read data
#' @export
#' @examples
#' readMyDatas_function()

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
