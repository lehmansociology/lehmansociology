#' Get the current swirl name
#'
#' @keywords swirl
#' @export

get_swirl_name<-function(){
    rstudioname<-system("whoami", intern=TRUE)
    swirlname<-gsub(".", "", rstudioname, fixed=TRUE)
    swirlname
}
