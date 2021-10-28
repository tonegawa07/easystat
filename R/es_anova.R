#' Easy anova
#'
#' \code{es_anova()} returns a simple data frame containing the p-values calculated by anova
#'
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#' @importFrom rlang !!
#' @importFrom rlang !!!
#'
#' @param data Data to be used
#' @param y Dependent variable
#' @param  ... qualitative independent variable
#'
#' @export
#'
es_anova <- function(data, y, ...) {
  y <- rlang::enquo(y)
  factors <- rlang::enquos(...)

  if (length(factors) == 0) {
    stop("The qualitative independent variable (factor/factors)
         is not given in the function.")
  }

  anova_df <-
    data %>%
    dplyr::select(!!y, !!!factors) %>%
    dplyr::rename(y := !!y)

  fml_x <- colnames(anova_df)[-1] %>% paste(collapse = "*")

  anova_result <-
    stats::aov(stats::as.formula(paste0("y ~ ", fml_x)), data = anova_df) %>%
    stats::anova() %>%
    as.data.frame() %>%
    dplyr::mutate(factor = rownames()) %>%
    dplyr::select(factor, p_value = `Pr(>F)`) %>%
    stats::na.omit() %>%
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

  rownames(anova_result) <- NULL

  return(anova_result)
}
