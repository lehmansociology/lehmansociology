#' Function for calculating a logit
#'
#' @keywords logit, glm
#' @export

logit <- function(x) {

    log(x/(1-x))

}
