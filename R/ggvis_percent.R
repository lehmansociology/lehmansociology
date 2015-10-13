
#' Layer Percent
#'
#' This function uses ggvis to create a histogram with percents
#' @keywords ggvis percent histogram
#' @export
#' @examples
#' ggvis_percent_function()

ggvis_percent<-function(x, x_var)
{
  formula<-as.formula(paste0('~', x_var ))

  x %>% compute_count(formula) %>% ggvis( ~x_, ~count_/sum(count_)*100)
}
