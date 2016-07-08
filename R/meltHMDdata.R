#' Function preprocessing mortality.org for making population pyramids
#'
#' This function calculates the mode for a dataframe, supporting multiple data types and multiple modes.
#' @keywords mortality melt preprocess
#' @export


meltHMDdata <- function(df, year){
  library(dplyr)
  library(reshape2)
  yeardata <- dplyr::filter(df, df$Year  == year)
  yeardata$Male1 <- -1 * yeardata$Male1
  melteddata <- melt(yeardata,
                     value.name='Population',
                     variable.name = 'Gender',  measure.vars  = c("Female1", "Male1"),
                     id.vars=c('Year', 'Age')
                     )
  return(melteddata)
}
