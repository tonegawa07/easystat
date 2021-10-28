#' Easy t-test
#'
#' \code{es_t_test()} returns a simple data frame containing the p-values calculated by the t-test
#'
#' @importFrom dplyr %>%
#' @importFrom rlang !!
#'
#' @inheritParams es_dataset
#' @param method student or welch. Default is welch
#'
#' @return Data frame with p-values in the t-test
#'
#' @export
#'
es_t_test <- function(data, value, group, method = "welch") {
  value <- rlang::enquo(value)
  group <- rlang::enquo(group)

  t_df <-
    data %>%
    dplyr::group_by(!!group) %>%
    tidyr::nest() %>%
    dplyr::rename(name = !!group)

  if (nrow(t_df) != 2) {
    stop('The "group" argument of the "es_t_test()" function
         is not set to 2 groups.')
  }

  p_value <-
    switch(method,
           "student" = stats::t.test(
             t_df$data[[1]], t_df$data[[2]], var.equal = TRUE
             ),
           "welch"   = stats::t.test(
             t_df$data[[1]], t_df$data[[2]], var.equal = FALSE
             )
           ) %>%
    magrittr::use_series(p.value)

  t_result <-
    data.frame(A = t_df$name[1],
               B = t_df$name[2],
               p_value = p_value) %>%
    dplyr::mutate(p_signif = dplyr::case_when(p_value < 0.001 ~ "< 0.001",
                                              p_value < 0.01  ~ "< 0.01",
                                              p_value < 0.05  ~ "< 0.05",
                                              TRUE ~ "N.S.")
                  ) %>%
    dplyr::mutate(
      signif = dplyr::case_when(
        p_value < 0.001 ~ stringr::str_dup("\u002A", times = 3),
        p_value < 0.01  ~ stringr::str_dup("\u002A", times = 2),
        p_value < 0.05  ~ stringr::str_dup("\u002A", times = 1),
        TRUE ~ ""))

  cat(insight::print_color(
    paste0("signif.: 0 ", stringr::str_dup("\u002A", times = 3),
           " 0.001 ", stringr::str_dup("\u002A", times = 2),
           " 0.01 ", stringr::str_dup("\u002A", times = 1),
           " 0.05\n"), "yellow"))

  return(t_result)
}
