#' Get Access Token
#'
#' @param cached_credentials Location to read from/store credentials.
#'
#' @return Access token used to perform AdminSDK tasks
#' @export
#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
#' @importFrom httr content GET add_headers POST
get_access_token <- function(cached_credentials = '~/.adminSDK_cache.rds') {
  # OAuth 2.0 Client ID
  secrets <- jsonlite::fromJSON(txt = system.file('extdata/OAuth_ClientID/client_secret.json', package = 'googleGroupR'))
  # Scopes
  scopes <- "https://www.googleapis.com/auth/admin.directory.group https://www.googleapis.com/auth/apps.groups.settings"
  # Create Authorization Link
  auth_link <- glue::glue('https://accounts.google.com/o/oauth2/auth?client_id={secrets$installed$client_id}&redirect_uri=urn:ietf:wg:oauth:2.0:oob&scope={scopes}&response_type=code')

  if(file.exists(cached_credentials) ) {
    cached_token <- readRDS(cached_credentials)
    if ('error' %in% names(cached_token) ) {
      message(glue::glue('Please visit the following URL to generate an authorization code: {auth_link}'))
      auth_code <- readline(prompt =  glue::glue('Enter authorization code:'))
      access_token_body <- list(code=auth_code,
                                client_id=secrets$installed$client_id,
                                client_secret=secrets$installed$client_secret,
                                redirect_uri='urn:ietf:wg:oauth:2.0:oob',
                                grant_type='authorization_code')
      token <- httr::content(httr::POST(url = secrets$installed$token_uri, body = access_token_body))
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
                               client_id=secrets$installed$client_id,
                               client_secret=secrets$installed$client_secret)
          token <- httr::content(httr::POST(url = secrets$installed$token_uri, refresh_header, body = refresh_body, encode='form'))
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
                             client_id=secrets$installed$client_id,
                             client_secret=secrets$installed$client_secret)
        token <- httr::content(httr::POST(url = secrets$installed$token_uri, refresh_header, body = refresh_body, encode='form'))
        cached_token$access_token <- token$access_token
        saveRDS(object = cached_token, file = cached_credentials)
        token$access_token
      } else{
        message('Not sure how to handle current token situation.')
      }
    }
  } else {
    message(glue::glue('Please visit the following URL to generate an authorization code: {auth_link}'))
    auth_code <- readline(prompt =  glue::glue('Enter authorization code:'))
    access_token_body <- list(code=auth_code,
                              client_id=secrets$installed$client_id,
                              client_secret=secrets$installed$client_secret,
                              redirect_uri='urn:ietf:wg:oauth:2.0:oob',
                              grant_type='authorization_code')
    token <- httr::content(httr::POST(url = secrets$installed$token_uri, body = access_token_body))
    saveRDS(object = token, file = cached_credentials)
    token$access_token
  }
}
