#' List Group Members
#'
#' Retrieves a paginated list of all members in a group.
#'
#' @param domain Required. A GSuite domain: 'somedomain.org'
#' @param group Required. The name of a Google Group. A listing of groups in your domain can be obtained by running list_groups(domain = 'mydomain.org')#'
#'
#' @return
#' @export
#' @importFrom glue glue
#' @importFrom httr add_headers content GET
list_group_members <- function (domain, group) {
  if (missing(domain) ) {
    message('Please specify a GSuite domain.')
  } else if (missing(group)) {
    message('Please specify a group name.')
  }else {
    group_id <- get_group_id(domain, group)
    access_token <- get_access_token()
    auth_header <- httr::add_headers('Authorization' = glue::glue('Bearer {access_token}'))
    httr::content(httr::GET(glue::glue('https://www.googleapis.com/admin/directory/v1/groups/{group_id}/members'),
                            auth_header)
    )
  }
}
