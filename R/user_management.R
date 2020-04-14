#' Add User
#'
#' Add a user to a Google Group
#'
#' @param domain Required. A GSuite domain: 'somedomain.org'
#' @param group Required. The name of a Google Group. A listing of groups in your domain can be obtained by running list_groups(domain = 'mydomain.org')
#' @param email Required. The Gmail Account of the user that you would like to add to the group.
#'
#' @return JSON object detailing successful user addition attributes.
#' @export
#' @importFrom glue glue
#' @importFrom httr add_headers content POST
add_user <- function(domain, group, email) {
  group_id <- get_group_id(domain, group)
  access_token <- get_access_token()
  add_user_url <- glue('https://www.googleapis.com/admin/directory/v1/groups/{group_id}/members')
  add_user_header <- httr::add_headers('Content-Type' = 'application/json',
                                       'Authorization' = glue('Bearer {access_token}'))
  add_user_body <- list(role='MEMBER',
                        email=email)
  httr::content(httr::POST(url = add_user_url, add_user_header, body = add_user_body, encode='json'))
}

#' Remove User
#'
#' Remove a user from a Google Group
#'
#' @param domain Required. A GSuite domain: 'somedomain.org'
#' @param group Required. The name of a Google Group. A listing of groups in your domain can be obtained by running list_groups(domain = 'mydomain.org')
#' @param email Required. The Gmail Account of the user that you would like to add to the group.
#'
#' @return If successful, an empty JSON responose will be received.
#' @export
#' @importFrom glue glue
#' @importFrom httr add_headers content DELETE
remove_user <- function(domain, group, email) {
  group_id <- get_group_id(domain, group)
  access_token <- get_access_token()
  remove_user_url <- glue('https://www.googleapis.com/admin/directory/v1/groups/{group_id}/members/{email}')
  remove_user_header <- httr::add_headers('Authorization' = glue('Bearer {access_token}'),
                                          "Accept"= 'application/json')
  httr::DELETE(url = remove_user_url, remove_user_header)
}
