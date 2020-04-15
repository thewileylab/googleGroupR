#' List User Info
#'
#' Retrieve a user specific directory info, such as primary email address.
#'
#' @param domain A GSuite domain: 'somedomain.org'
#' @param group The name of a Google Group. A listing of groups in your domain can be obtained by running list_groups(domain = 'mydomain.org')
#' @param memberKey The user's unique ID
#'
#' @return JSON object with directory information pertaining to the user's unique ID.
#' @export
#' @importFrom glue glue
#' @importFrom httr add_headers content
list_user_info <- function(domain, group, memberKey) {
  group_id <- get_group_id(domain, group)
  access_token <- get_access_token()
  auth_header <- httr::add_headers('Authorization' = glue::glue('Bearer {access_token}'))
  httr::content(httr::GET(glue::glue('https://www.googleapis.com/admin/directory/v1/groups/{group_id}/members/{memberKey}'),
                          auth_header)
  )
}
