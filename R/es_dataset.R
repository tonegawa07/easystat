#' Formatting the data for Tukey or Dunnett
#'
#' Extract the dependent and independent variables and order the factors
#'
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#' @importFrom rlang !!
#'
#' @param data Data to be used
#' @param value Dependent variable
#' @param group qualitative independent variable
#'
es_dataset <- function(data, value, group) {
  value <- rlang::enquo(value)
  group <- rlang::enquo(group)

  d <-
    data %>%
    dplyr::select(!!value, !!group) %>%
    magrittr::set_colnames(c("value", "group")) %>%
    dplyr::mutate(group := forcats::fct_inorder(group))

  return(d)
}
