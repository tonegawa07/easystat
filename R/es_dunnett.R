#' Easy Dunnett's test
#'
#' \code{es_dunnett()} returns a simple data frame containing the p-values calculated by the Dunnett's test
#' @inheritParams es_dataset
#'
#' @importFrom dplyr %>%
#' @importFrom rlang !!
#'
#' @return Data frame with p-values in the Dunnett's test
#'
#' @export
#'
es_dunnett <- function(data, value, group) {
  value <- rlang::enquo(value)
  group <- rlang::enquo(group)

  dunnett <-
    data %>%
    es_dataset(value = !!value, group = !!group) %>%
    stats::aov(value ~ group, data = .) %>%
    multcomp::glht(linfct = multcomp::mcp(group = "Dunnett")) %>%
    summary() %>%
    magrittr::use_series(test)

  dunnett_result <-
    data.frame(
      g = names(dunnett$tstat),
      p_value = dunnett$pvalue
    ) %>%
    tidyr::separate(
      col = g,
      into = c("Treatment", "Control"),
      sep = " - "
    ) %>%
    dplyr::select(Control, Treatment, p_value) %>%
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

  return(dunnett_result)
}

#' Summary of easy Dunnett's test
#'
#' \code{es_dunnett_summary()} returns a simple data frame containing the p-values calculated by the Dunnett's test
#' @inheritParams es_dataset
#'
#' @importFrom dplyr %>%
#' @importFrom rlang !!
#'
#' @return Data frame with p-values in the Dunnett's test
#'
#' @export
#'
es_dunnett_summary <- function(data, value, group) {
  value <- rlang::enquo(value)
  group <- rlang::enquo(group)

  dunnett_result <-
    data %>%
    es_dunnett(value = !!value, group = !!group)

  control_df <-
    data.frame(Treatment = dunnett_result$Control[1],
               p_signif = "",
               signif = "")

  dunnett_summary <-
    dunnett_result %>%
    dplyr::select(Treatment, p_signif, signif) %>%
    dplyr::bind_rows(control_df, .) %>%
    as.data.frame()

  return(dunnett_summary)
}
