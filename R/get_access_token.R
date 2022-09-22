# Helpers ----
#' Installed App
#'
#' Invisibly returns an OAuth app
#'
#' @return An Invisible OAuth consumer application, produced by [httr::oauth_app()]
#'
#' @export
#' @keywords internal
#' @rdname internal-assets
installed_app <- function() {
  ggoa()
}
#' @export
#' @keywords internal
#' @rdname internal-assets
#' @noRd
print.hidden_fn <- function(x, ...) {
  x <- 'Nope'
  NextMethod('print')
}

#' Get Access Token
#'
#' @param cached_credentials Location to read from/store credentials.
#'
#' @keywords internal
#'
#' @return Access token used to perform AdminSDK tasks
#' @export
#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
#' @importFrom httr content GET add_headers oauth_listener POST
get_access_token <- function(cached_credentials = '~/.googleGroupR_cache.rds') {
  # OAuth 2.0 Info
  ## Token URI
    token_uri <- "https://oauth2.googleapis.com/token"
  ## Scopes
    scopes <- "https://www.googleapis.com/auth/admin.directory.group https://www.googleapis.com/auth/apps.groups.settings"
  ## Authorization Link
    auth_link <- glue::glue('https://accounts.google.com/o/oauth2/auth?client_id={installed_app()$key}&redirect_uri={installed_app()$redirect_uri}&scope={scopes}&response_type=code')

  if(file.exists(cached_credentials) ) {
    cached_token <- readRDS(cached_credentials)
    if ('error' %in% names(cached_token) ) {
      # message(glue::glue('Please visit the following URL to generate an authorization code: {auth_link}'))
      # auth_code <- readline(prompt =  glue::glue('Enter authorization code:'))
      auth_code <- oauth_listener(auth_link, is_interactive = interactive())
      access_token_body <- list(code=auth_code$code,
                                client_id=installed_app()$key,
                                client_secret=installed_app()$secret,
                                redirect_uri=installed_app()$redirect_uri,
                                grant_type='authorization_code')
      token <- httr::content(httr::POST(url = token_uri, body = access_token_body))
      saveRDS(object = token, file = cached_credentials)
      token$access_token
    } else {
      google_oauth_url <- 'https://accounts.google.com/o/oauth2/'
      token_info <- httr::content(httr::GET(glue::glue('{google_oauth_url}tokeninfo?access_token={cached_token$access_token}')))
      if('expires_in' %in% names(token_info) ){
        if(token_info$expires_in > 0 ) {
          message('Using access token from cache.')
          cached_token$access_token
        } else {
          message('Cached token expired, obtaining new access token.')
          # Create Refresh cURL command pieces
          refresh_header <- httr::add_headers('Content-Type' = 'application/x-www-form-urlencoded')
          refresh_body <- list(grant_type='refresh_token',
                               refresh_token=cached_token$refresh_token,
                               client_id=installed_app()$key,
                               client_secret=installed_app()$secret)
          token <- httr::content(httr::POST(url = token_uri, refresh_header, body = refresh_body, encode='form'))
          cached_token$access_token <- token$access_token
          saveRDS(object = cached_token, file = cached_credentials)
          token$access_token
        }
      } else if ('error' %in% names(token_info) ){
        message('Cached token expired, obtaining new access token.')
        # Create Refresh cURL command pieces
        refresh_header <- httr::add_headers('Content-Type' = 'application/x-www-form-urlencoded')
        refresh_body <- list(grant_type='refresh_token',
                             refresh_token=cached_token$refresh_token,
                             client_id=installed_app()$key,
                             client_secret=installed_app()$secret)
        token <- httr::content(httr::POST(url = token_uri, refresh_header, body = refresh_body, encode='form'))
        cached_token$access_token <- token$access_token
        saveRDS(object = cached_token, file = cached_credentials)
        token$access_token
      } else{
        message('Not sure how to handle current token situation. Go ask an adult.')
      }
    }
  } else {
    # message(glue::glue('Please visit the following URL to generate an authorization code: {auth_link}'))
    # auth_code <- readline(prompt =  glue::glue('Enter authorization code:'))
    auth_code <- oauth_listener(auth_link, is_interactive = interactive())
    access_token_body <- list(code=auth_code$code,
                              client_id=installed_app()$key,
                              client_secret=installed_app()$secret,
                              redirect_uri=installed_app()$redirect_uri,
                              grant_type='authorization_code')
    token <- httr::content(httr::POST(url = token_uri, body = access_token_body))
    saveRDS(object = token, file = cached_credentials)
    token$access_token
  }
}
