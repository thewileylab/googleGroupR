add_user <- function(domain, group, email) {
  groups <- list_groups(domain) %>%
    magrittr::extract2('groups') %>%
    tibble::enframe(name = NULL, value = 'group_info') %>%
    dplyr::mutate(group_name = purrr::map_chr(.x = group_info,
                                               ~ .x %>%
                                                 magrittr::extract2('name')
                                               ),
                  group_id = purrr::map_chr(.x = group_info,
                                            ~ .x %>%
                                              magrittr::extract2('id'))
                  )
  group_id <- groups %>%
    dplyr::filter(group_name == group) %>%
    pull(group_id)
  access_token <- get_access_token()
  add_user_url <- glue('https://www.googleapis.com/admin/directory/v1/groups/{group_id}/members')
  add_user_header <- httr::add_headers('Content-Type' = 'application/json',
                                       'Authorization' = glue('Bearer {access_token}'))
  add_user_body <- list(role='MEMBER',
                        email=email)
  httr::content(httr::POST(url = add_user_url, add_user_header, body = add_user_body, encode='json'))
}

remove_user <- function(domain, group, email) {
  groups <- list_groups(domain) %>%
    magrittr::extract2('groups') %>%
    tibble::enframe(name = NULL, value = 'group_info') %>%
    dplyr::mutate(group_name = purrr::map_chr(.x = group_info,
                                              ~ .x %>%
                                                magrittr::extract2('name')
    ),
    group_id = purrr::map_chr(.x = group_info,
                              ~ .x %>%
                                magrittr::extract2('id'))
    )
  group_id <- groups %>%
    dplyr::filter(group_name == group) %>%
    pull(group_id)
  access_token <- get_access_token()
  remove_user_url <- glue('https://www.googleapis.com/admin/directory/v1/groups/{group_id}/members/{email}')
  remove_user_header <- httr::add_headers('Authorization' = glue('Bearer {access_token}'),
                                          "Accept"= 'application/json')
  httr::DELETE(url = remove_user_url, remove_user_header)
}
