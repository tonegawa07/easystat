#' Formatting the dataset & setting the factor
#'
#' Extract the dependent and independent variables and order the factors
#'
#' @param data Data to be used
#' @param vx Dependent variable
#' @param fx Independent variable
#'
es_dataset <- function(data, vx, fx)
{
  d <-
    data %>%
    dplyr::select(vx, fx) %>%
    magrittr::set_colnames(c("vx", "fx")) %>%
    dplyr::mutate(fx = forcats::fct_inorder(fx))

  return(d)
}

#' Easy tukey test
#'
#' \code{es_tukey} returns a simple data frame containing the alphabet assigned
#' by the tukey test
#'
#' @inheritParams es_dataset
#' @param sig_level Significance level
#'
#' @return Data frame containing the alphabet assigned by the tukey test
#'
#' @export
#'
es_tukey <- function(data, vx, fx, sig_level)
{
  tukey_result <-
    easystat::es_dataset(data, vx, fx) %>%
    aov(vx ~ fx, data = .) %>%
    multcomp::glht(linfct = multcomp::mcp(fx = "Tukey")) %>%
    multcomp::cld(level = sig_level) %>%
    magrittr::use_series(mcletters) %>%
    magrittr::use_series(Letters) %>%
    data.frame(f = names(.), v = .) %>%
    dplyr::rename(!!fx := f, !!vx := v)

  row.names(tukey_result) <- NULL

  return(tukey_result)
}

#' Easy Dunnett test
#'
#' \code{es_dunnett} returns a simple data frame containing the p-values calculated by the dunnet test
#' @inheritParams es_dataset
#'
#' @return Data frame with p-values in the Dunnett test
#'
#' @export
#'
es_dunnett <- function(data, vx, fx)
{
  dunnett_summary <-
    easystat::es_dataset(data, vx, fx) %>%
    aov(vx ~ fx, data = .) %>%
    multcomp::glht(linfct = multcomp::mcp(fx = "Dunnett")) %>%
    summary() %>%
    magrittr::use_series(test)

  dunnett_result <-
    data.frame(
      f = names(dunnett_summary$tstat),
      p_value = dunnett_summary$pvalue
    ) %>%
    tidyr::separate(
      col = f,
      into = c("Treatment", "Control"),
      sep = " - "
    ) %>%
    dplyr::select(Control, Treatment, p_value)

  return(dunnett_result)
}
