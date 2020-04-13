#' List Groups
#'
#' Retrieve a listing of all groups in the learnclinicalda
#'
#' @param domain A Google domain: 'somedomain.org'
#'
#' @return
#' @export
#' @importFrom glue glue
#' @importFrom httr add_headers content GET
list_groups <- function(domain) {
  access_token <- get_access_token()
  auth_header <- httr::add_headers('Authorization' = glue('Bearer {access_token}'))
  httr::content(httr::GET(glue::glue('https://www.googleapis.com/admin/directory/v1/groups/?domain={domain}'),
                          auth_header)
  )
}