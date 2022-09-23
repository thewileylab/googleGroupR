#' List Groups
#'
#' Retrieve a listing of all groups in the specified GSuite domain.
#'
#' @param domain Required. A GSuite domain: 'somedomain.org'
#'
#' @return JSON object with information about all groups in the specified GSuite domain.
#' @export
#' @importFrom glue glue
#' @importFrom httr add_headers content GET
#' @importFrom rlang format_error_bullets inform
list_groups <- function(domain) {
  if (missing(domain) ) {
    rlang::inform(rlang::format_error_bullets(c("x" = 'Please specify a GSuite domain.')))
    } else {
      access_token <- get_access_token()
      if(is.null(access_token)){
        rlang::inform(rlang::format_error_bullets("x" = 'Please provide authentication credentials.'))
        } else {
          auth_header <- httr::add_headers('Authorization' = glue::glue('Bearer {access_token}'))
          httr::content(httr::GET(glue::glue('https://www.googleapis.com/admin/directory/v1/groups/?domain={domain}'),
                                  auth_header)
                        )
          }
      }
}
