base_url <- "https://www.googleapis.com/storage/v1/"

#' Delete an object in a bucket.
#' @param bucket bucket name
#' @param object object name
#' @param token a valid OAuth2.0 token
#' @return TRUE if the object is successfully deleted; FALSE otherwise
#' @importFrom httr DELETE
#' @export
delete_obj <- function(bucket, object, token = get_access_cred()) {
  url <- paste0(base_url, "b/", bucket, "/o/", object)
  
  req <- DELETE(url, config = config(token = token))
  
  return(status_code(req) == 204)
}