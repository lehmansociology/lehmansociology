#' Function for creating a population pyramid
#'
#' This function uses ggplot to make a population pyramid from mortality.org data.
#' @keywords population pyramid graph
#' @export



populationPyramid <- function (df, year, entityname){
# Inspired by https://rpubs.com/walkerke/pyramids_ggplot2.
  melteddata <- meltHMDdata(df, year)

  library(ggplot2)
  plot<- ggplot(melteddata, aes(x = Age, y = Population, fill = Gender)) +
    geom_bar(position='identity', stat = "identity") +
    scale_y_continuous(breaks = seq(-2500000, 2500000, 200000),
                       labels = paste0(as.character(c(seq(25, 0, -2), seq(1, 25, 2))), "m")) +
    coord_flip() +
    scale_fill_brewer(palette = "Set1") +
    ggtitle(paste0(year, " Population Pyramid for ", entityname))+
    theme(axis.text.x = element_text( angle = -45, hjust= 0, vjust =.5))


  return(plot)
}
