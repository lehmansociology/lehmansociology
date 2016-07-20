#' Function for reading data from AWS S3 storage
#'
#' This function reads an R data set from storage. It requires that you have keys and the name of
#'  the bucket set as envrionmental variables. It is a convenience function for s3load() from the aws.s3
#'   package. AWS_DEFAULT_BUCKET is the name of the global variable holding the bucketname.
#' @keywords aws request data
#' @export


load_lehman_data<-function(df){
    bucketname <- Sys.getenv("AWS_DEFAULT_BUCKET")
    if (bucketname != ""){
        aws.s3::s3load(paste0(df,'.RData'), bucket = bucketname)
    }

}
