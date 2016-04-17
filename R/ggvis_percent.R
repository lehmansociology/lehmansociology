
#' Layer Percent
#'
#' This function uses ggvis to create a histogram with percents
#' @keywords ggvis percent histogram
#' @export

ggvis_percent<-function(x, x_var)
{
  formula<-as.formula(paste0('~', x_var ))

  compute_count(formula)
  loadNamespace('ggvis')
  ggvis( ~x_, ~count_/sum(count_)*100)
}
