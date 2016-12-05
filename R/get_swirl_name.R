#' Get the swirl name for the current user
#'
#' @keywords swirl
#' @export

get_swirl_name<-function(){
    rstudioname<-system("whoami", intern=TRUE)
    swirlname<-gsub(".", "", rstudioname, fixed=TRUE)
    swirlname
}
