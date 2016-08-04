upload_url <- "https://www.googleapis.com/upload/storage/v1/"

#' Upload a file to a Cloud Storage bucket
#' @param file Name of the uploaded file
#' @param bucket Bucket name
#' @param name Object name
#' @param type Media type
#' @return uploaded object's self link
#' @importFrom mime guess_type
#' @export
upload_file <- function(file, bucket, name = "uploadedObject", type = NULL,
                        token = get_access_cred()) {
  assert_that(is.string(file),
              is.string(bucket),
              is.string(name),
              is.null(type) || is.string(type))

  url <- paste0(upload_url, "b/", bucket, "/o")
  
  if (is.null(type)) {
    type <- guess_type(file)
  }

  resp <- POST(url = url, body = upload_file(file, type = type),
               config = config(token = token),
               add_headers("Content-Type" = type),
               query = list(uploadType = "media", name = name))
  
  content(resp)$selfLink
}

#' Upload a data frame to a Cloud Storage bucket
#' @param df data frame name
#' @param bucket bucket name
#' @param name object name
#' @param token a valid OAuth token
#' @return uploaded object's self link
#' @export
upload_df_as_csv <- function(df, bucket, name = deparse(substitute(df)),
                             token = get_access_cred()) {
  assert_that(is.data.frame(df),
              is.string(bucket),
              is.string(name))

  # create a temp csv file
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  # write data frame to a csv
  # NA values are encoded as empty strings
  write.csv(df, tmp, row.names = FALSE, na = "")
  # upload the csv to bucket in cloud storage
  upload_file(tmp, bucket, name)
}