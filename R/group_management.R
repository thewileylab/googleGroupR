#' Create Group
#'
#' Create a group in a GSuite domain.
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

#' Delete Group
#'
#' Delete a group in a GSuite domain.
#'
#' @param domain A GSuite domain: 'somedomain.org'
#' @param group The name of a Google Group. A listing of groups in your domain can be obtained by running list_groups(domain = 'mydomain.org')
#'
#' @return If successful, an empty JSON responose will be received.
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>% extract2
#' @importFrom tibble enframe
#' @importFrom dplyr mutate filter pull
#' @importFrom purrr map_chr
#' @importFrom glue glue
#' @importFrom httr add_headers content DELETE
delete_group <- function (domain, group) {
  groups <- list_groups(domain) %>%
    magrittr::extract2('groups') %>%
    tibble::enframe(name = NULL, value = 'group_info') %>%
    dplyr::mutate(group_name = purrr::map_chr(.x = .data$group_info,
                                              ~ .x %>%
                                                magrittr::extract2('name')
    ),
    group_id = purrr::map_chr(.x = .data$group_info,
                              ~ .x %>%
                                magrittr::extract2('id'))
    )
  group_id <- groups %>%
    dplyr::filter(.data$group_name == group) %>%
    dplyr::pull(.data$group_id)
  access_token <- get_access_token()
  delete_group_url <- glue('https://www.googleapis.com/admin/directory/v1/groups/{group_id}')
  delete_group_header <- httr::add_headers('Authorization' = glue('Bearer {access_token}'),
                                           "Accept"= 'application/json')
  httr::DELETE(url = delete_group_url, delete_group_header)
}
