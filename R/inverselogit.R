#' Function for calculating an inverse logit
#'
#' @keywords logit, glm
#' @export

inverselogit <- function(x) {
        x <- unname(x)
        exp(x)/(1 + exp(x))

}
