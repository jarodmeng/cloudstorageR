cloudstorageR <- oauth_app("google",
                           "1071675102318-1n3sp80hmicrgnf82op59bjv37a57gik.apps.googleusercontent.com",
                           "cZbmW_BK-CbUqQCytSYAzpwx")

cs_env <- new.env(parent = emptyenv())

#' Get and set access credentials
#'
#' @section API console:
#' To manage your google projects, use the API console:
#' \url{https://console.cloud.google.com/}
#'
#' @keywords internal
#' @importFrom httr oauth_app oauth2.0_token oauth_endpoints oauth_service_token
#' @importFrom jsonlite fromJSON
#' @export
#' @param value new access credentials, as returned by
#'  \code{\link[httr]{oauth2.0_token}}
#' @seealso Scope documentation:
#'  \url{https://developers.google.com/identity/protocols/googlescopes}
#' @seealso Cloud Storage OAuth documentation:
#'  \url{https://cloud.google.com/storage/docs/json_api/v1/how-tos/authorizing}
get_access_cred <- function() {
  cred <- bq_env$access_cred
  if (is.null(cred)) {
    set_oauth2.0_cred()
  }
  
  cred
}

#' @rdname get_access_cred
#' @export
set_oauth2.0_cred <- function(app = bigqr) {
  cred <- oauth2.0_token(google, app,
                         scope = c(
                           "https://www.googleapis.com/auth/cloud-platform",
                           "https://www.googleapis.com/auth/devstorage.full_control"))
  
  set_access_cred(cred)
}

#' @rdname get_access_cred
#' @export
set_access_cred <- function(value) {
  cs_env$access_cred <- value
}

#' @rdname get_access_cred
#' @export
reset_access_cred <- function() {
  set_access_cred(NULL)
}

#' @export
#' @rdname get_access_cred
#' @param service_token A JSON string, URL or file, giving or pointing to
#'   the service token file.
set_service_token <- function(service_token) {
  
  service_token <- jsonlite::fromJSON(service_token)
  
  endpoint <- httr::oauth_endpoints("google")
  
  scope <- "https://www.googleapis.com/auth/devstorage.full_control"
  
  cred <- httr::oauth_service_token(endpoint, service_token, scope)
  
  set_access_cred(cred)
}