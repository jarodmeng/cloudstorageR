base_url <- "https://www.googleapis.com/storage/v1/"
upload_url <- "https://www.googleapis.com/upload/storage/v1/"

simple_upload <- function(file, bucket, name = "uploadedObject", type = NULL,
                          token = get_access_cred()) {
  url <- paste0(upload_url, "b/", bucket, "/o")
  
  if (is.null(type)) {
    type <- mime::guess_type(file)
  }

  resp <- POST(url = url, body = upload_file(file, type = type),
               config(token = token),
               add_headers("Content-Type" = type),
               query = list(uploadType = "media", name = name))
}