base_host <- "https://www.googleapis.com"
base_service <- "/storage/v1/"
base_url <- paste0(base_host, base_service)

#' Delete an object in a bucket.
#' @param bucket bucket name
#' @param object object name
#' @param token a valid OAuth2.0 token
#' @return TRUE if the object is successfully deleted; FALSE otherwise
#' @importFrom httr DELETE
#' @export
delete_obj <- function(bucket, obj, token = get_access_cred()) {
  url <- paste0(base_url, "b/", bucket, "/o/", obj)
  
  req <- DELETE(url, config = config(token = token))
  
  return(status_code(req) == 204)
}

delete_obj_req <- function(obj, bucket) {
  s_content_type <- "Content-Type: application/http\r\n"
  s_request_type <- "DELETE"
  s_url <- paste0(base_service, "b/", bucket, "/o/", obj, "\r\n")
  
  return(c(s_content_type, paste(s_request_type, s_url, sep = " ")))
}

#' Batch delete multiple objects
#' @param bucket bucket name
#' @param objs list of object names
#' @param token a valid OAuth2.0 token
#' @return response to the batch operation
#' @export
#' @rdname delete_obj
batch_delete_objs <- function(bucket, objs, token = get_access_cred()) {
  if (length(objs) == 1) {
    stop("Use delete_obj() for single object deletion.")
  }
  
  boundary <- "cloudstorageR"
  sep <- paste0("--", boundary)
  end <- paste0("--", boundary, "--")
  
  l_reqs <- lapply(objs, delete_obj_req, bucket = bucket)
  
  l_body <- list()
  for (i in seq_along(l_reqs)) {
    l_body[[2*i-1]] <- sep
    l_body[[2*i]] <- l_reqs[[i]]
  }
  l_body <- c(l_body, end)
  body <- unlist(l_body)
  
  url <- paste0(base_host, "/batch")
  
  type <- paste0("multipart/mixed; boundary=", boundary)
  
  resp <- POST(url, body = body, config = config(token = token),
               add_headers("Content-Type" = type))
  
  resp
}