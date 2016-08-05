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
  cred <- cs_env$access_cred
  
  if (is.null(cred)) {
    scope <- c(
      "https://www.googleapis.com/auth/cloud-platform",
      "https://www.googleapis.com/auth/devstorage.full_control")
    
    use_service <- getOption("cloudstorageR.use.service.account")
    
    if (!is.null(use_service) && isTRUE(use_service)) {
      json_key <- getOption("cloudstorageR.service.account.json")
      if (is.null(json_key) || !is.string(json_key)) {
        stop("You have to provide a valid JSON key file.")
      } else {
        secrets <- fromJSON(json_key)
        cred <- oauth_service_token(google, secrets,
                                    scope = paste(scope, collapse = " "))
      }
    } else {
      app <- getOption("cloudstorageR.oauth.app")
      if (is.null(app)) {
        app <- cloudstorageR
      }
      
      cred <- oauth2.0_token(oauth_endpoints("google"), app, scope = scope)
    }
    
    # Stop if unsuccessful
    set_access_cred(cred)
  }
  
  cred
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