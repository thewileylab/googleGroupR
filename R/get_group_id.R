#' Get Group ID
#'
#' Retrieve the Group ID assosciated with a Group Name
#'
#'
#' @param domain A GSuite domain: 'somedomain.org'
#' @param group The name of a Google Group. A listing of groups in your domain can be obtained by running list_groups(domain = 'mydomain.org')
#'
#' @keywords internal
#'
#' @return
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>% extract2
#' @importFrom tibble enframe
#' @importFrom dplyr mutate filter pull
#' @importFrom purrr map_chr
get_group_id <- function(domain, group) {
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
  groups %>%
    dplyr::filter(.data$group_name == group) %>%
    dplyr::pull(.data$group_id)
}
