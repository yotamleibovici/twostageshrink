#' Evaluate a shrinkage estimator
#'
#' @param data A tibble, consists the \code{numeric} columns \code{estim_base}
#'   and \code{estim_shrink}, the two estimators, and \code{param}, the real
#'   parameter.
#'
#' @return A tibble consists of MSE of the two estimators, and the MSE-ratio of
#'   them.
#' @export
#'
#' @examples
shrink_evaluate <- function(data, ...) {
  assertthat::assert_that(
    is.data.frame(data),
    assertthat::has_name(data, c(
      # "nobs",
      # "case",
      "estim_base",
      "estim_shrink",
      "param"
    )),
    is.numeric(data$estim_base),
    is.numeric(data$estim_shrink),
    is.numeric(data$param)
  )

  group <- rlang::enexprs(...)

  # other_vars <- data %>%
  #   dplyr::group_by(!!!group) %>%
  #   dplyr::filter(dplyr::row_number() == 1) %>%
  #   ungroup()

  data %>%
    dplyr::mutate(
      quadloss_base = (estim_base - param)^2,
      quadloss_shrink = (estim_shrink - param)^2
    ) %>%
    dplyr::group_by(!!!group) %>%
    dplyr::summarise(
      # gfactor = mean(gfactor),
      # gexp = mean(gexp),
      # bfactor = mean(bfactor),
      # bexp = mean(bexp),
      mse_base = mean(quadloss_base),
      mse_comp = mean(quadloss_shrink),
      mse_ratio = mse_comp / mse_base
    ) %>%
    dplyr::ungroup()
}
