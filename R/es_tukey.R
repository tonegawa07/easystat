#' The core of the Tukey's test
#'
#' @importFrom dplyr %>%
#' @importFrom rlang !!
#'
#' @inheritParams es_dataset
#' @param sig_level Significance level
#'
es_tukey_core <- function(data, value, group, sig_level) {
  value <- rlang::enquo(value)
  group <- rlang::enquo(group)

  tukey <-
    data %>%
    es_dataset(value = !!value, group = !!group) %>%
    stats::aov(value ~ group, data = .) %>%
    multcomp::glht(linfct = multcomp::mcp(group = "Tukey")) %>%
    summary()

  return(tukey)
}

#' Easy Tukey's test
#'
#' \code{es_tukey()} returns a simple data frame containing the p-values calculated by the Tukey's test
#'
#' @importFrom dplyr %>%
#' @importFrom rlang !!
#'
#' @inheritParams es_tukey_core
#'
#' @return Data frame with p-values in the Tukey's test
#'
#' @export
#'
es_tukey <- function(data, value, group, sig_level) {
  value <- rlang::enquo(value)
  group <- rlang::enquo(group)

  tukey <-
    data %>%
    es_tukey_core(value = !!value, group = !!group, sig_level = sig_level) %>%
    magrittr::use_series(test)

  tukey_result <-
    data.frame(
      g = names(tukey$tstat),
      p_value = tukey$pvalue
    ) %>%
    tidyr::separate(
      col = g,
      into = c("B", "A"),
      sep = " - "
    ) %>%
    dplyr::select(A, B, p_value) %>%
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

  return(tukey_result)
}

#' Summary of easy Tukey's test
#'
#' \code{es_tukey_summary()} returns a simple data frame containing the alphabet assigned by the Tukey's test
#'
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#' @importFrom rlang !!
#'
#' @inheritParams es_tukey_core
#'
#' @return Data frame containing the alphabet assigned by the Tukey's test
#'
#' @export
#'
es_tukey_summary <- function(data, value, group, sig_level) {
  value <- rlang::enquo(value)
  group <- rlang::enquo(group)

  tukey_summary <-
    data %>%
    es_tukey_core(value = !!value, group = !!group, sig_level = sig_level) %>%
    multcomp::cld(level = sig_level) %>%
    magrittr::use_series(mcletters) %>%
    magrittr::use_series(Letters) %>%
    data.frame(g = names(.), v = .) %>%
    dplyr::rename(!!group := g, !!value := v)

  row.names(tukey_summary) <- NULL

  return(tukey_summary)
}
