#' Add User
#'
#' Add a user to a Google Group
#'
#' @param domain A GSuite domain: 'somedomain.org'
#' @param group The name of a Google Group. A listing of groups in your domain can be obtained by running list_groups(domain = 'mydomain.org')
#' @param email The Gmail Account of the user that you would like to add to the group.
#'
#' @return JSON object detailing successful user addition attributes.
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>% extract2
#' @importFrom tibble enframe
#' @importFrom dplyr mutate filter pull
#' @importFrom purrr map_chr
#' @importFrom glue glue
#' @importFrom httr add_headers content POST
add_user <- function(domain, group, email) {
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
#' @param domain A GSuite domain: 'somedomain.org'
#' @param group The name of a Google Group. A listing of groups in your domain can be obtained by running list_groups(domain = 'mydomain.org')
#' @param email The Gmail Account of the user that you would like to add to the group.
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
remove_user <- function(domain, group, email) {
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
  remove_user_url <- glue('https://www.googleapis.com/admin/directory/v1/groups/{group_id}/members/{email}')
  remove_user_header <- httr::add_headers('Authorization' = glue('Bearer {access_token}'),
                                          "Accept"= 'application/json')
  httr::DELETE(url = remove_user_url, remove_user_header)
}
