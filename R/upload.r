base_url <- "https://www.googleapis.com/storage/v1/"
upload_url <- "https://www.googleapis.com/upload/storage/v1/"

#' A simple upload of a file to a Cloud Storage bucket
#' @param file Name of the uploaded file.
#' @param bucket Bucket name.
#' @param name Object name.
#' @param type Media type.
#' @param token A valid OAuth2.0 token.
#' @export
simple_upload <- function(file, bucket, name = "uploadedObject", type = NULL,
                          token = get_access_cred()) {
  assert_that(is.string(file),
              is.string(bucket),
              is.string(name),
              is.null(type) || is.string(type))

  url <- paste0(upload_url, "b/", bucket, "/o")
  
  if (is.null(type)) {
    type <- mime::guess_type(file)
  }

  resp <- POST(url = url, body = upload_file(file, type = type),
               config(token = token),
               add_headers("Content-Type" = type),
               query = list(uploadType = "media", name = name))
  
  resp
}