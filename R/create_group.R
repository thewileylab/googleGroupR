#' Create Group
#'
#' Create a group in the a GSuite domain.
#'
#' @param email Required. Email address to assign to your group. 'test@gsuitedomain.org'
#' @param name Optional. Provide a descriptive name for your group.
#' @param description Optional. Provide a description for your group.
#'
#' @return JSON object with information about the newly created group.
#' @export
#' @importFrom httr add_headers content POST
create_group <- function (email, name = NULL, description = NULL) {
  access_token <- get_access_token()
  groups_url <- 'https://www.googleapis.com/admin/directory/v1/groups'
  auth_header <- httr::add_headers('Content-Type'= 'application/json',
                                   'Authorization' = glue('Bearer {access_token}'))
  create_group_body <- list(email = email,
                            name= name,
                            description= description)
  httr::content(httr::POST(url = groups_url, auth_header, body = create_group_body, encode='json'))
}
