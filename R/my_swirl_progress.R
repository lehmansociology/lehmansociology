#' Function for showing progress in swirl
#'
#' @keywords swirl
#' @export

my_swirl_progress<-function(){

    swirlname<-get_swirl_name()
    results<-list.files(path=paste0("/usr/local/lib/R/site-library/swirl_user_data/",swirlname), recursive = TRUE)

    started <-as.numeric(!grepl("done", results) & !grepl("swlog", results))
    finished<-as.numeric(grepl("done", results))
    started_names <- results[started == 1]
    finished_names <- results[finished == 1]

    total_started <- sum(started)
    total_finished <- sum(finished)
    swirl_summary <-
        list(started_names, finished_names,
             total_started, total_finished)
    names(swirl_summary)<-c("Lessons Started but Not Finished", "Lessons Finished",
                            "Number Started but Not Finished", "Number Finished")
    swirl_summary
}
