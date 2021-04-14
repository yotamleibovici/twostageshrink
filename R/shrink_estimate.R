#' Estimate parameter with shrinkage estimator
#'
#' @param data A tibble. Consists of raw observations, which may be
#'   manipulated to construct filtration and base test statistics.
#' @param estim_base An expression, that its evaluation, with \code{data} as an
#'   environment, produces a numeric vector of length 1 or \code{data}'s number of
#'   rows. A base statistic.
#' @param estim_filt An expression, that its evaluation, with \code{data} as an
#'   environment, produces a numeric vector of length 1 or \code{data}'s number of
#'   rows. A filtration statistic.
#' @param filtfactor A \code{numeric} constant, used to define the filtration
#'   threshold.
#' @param filtexp A \code{numeric} constant, used to define the filtration
#'   threshold.
#'
#' @return \code{data}, with additional columns:
#' \itemize{
#'   \item \code{estim_base}, \code{estim_filt}: Evaluation
#'     results of the corresponding parameter values.
#'   \item \code{filt}: Whether or not a filtration has occurred:
#'     \code{estim_filt < filtfactor * nobs^(-filtexp)}.
#'   \item \code{estim_shrink}: Shrinkage estimator's value.
#' }
#' @export
#'
#' @examples
shrink_estimate <- function(data, estim_base, estim_filt, filtfactor, filtexp) {
  assertthat::assert_that(is.data.frame(data))

  data %>%
    dplyr::mutate(
      estim_base = !!rlang::enexpr(estim_base),
      estim_filt = !!rlang::enexpr(estim_filt),
      filt = estim_filt < filtfactor * nobs^(-filtexp),
      estim_shrink = dplyr::if_else(filt, 0, estim_base)
    )
}
