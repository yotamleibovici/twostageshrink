#' Perform a two-stage procedure
#'
#' @param data A tibble. Consists of raw observations, which may be
#'   manipulated to construct filtration and base test statistics.
#' @param filt_test An expression, that its evaluation, with \code{data} as an
#'   environment, produces a \code{logical} vector of length 1 or \code{data}'s number of
#'   rows. A \code{TRUE} value correspond to an hypothesis that passes the filtration
#'   test, i.e., is NOT filtered (filtration test rejected).
#' @param base_pval An expression, that its evaluation, with \code{data} as an
#'   environment, produces a numeric vector of length 1 or \code{data}'s number of
#'   rows. A \eqn{p}-value for the base test.
#' @param base_thrl An expression, that its evaluation, with \code{data} as an
#'   environment, produces a numeric vector of length 1 or \code{data}'s number of
#'   rows. A threshold, functioning as a reject region for the base test.
#' @param base_thrl_adj_method A function of 1 argument. Used to adjust the
#'   thresholds of the base test, considering only unfiltered hypotheses.
#' @param ... \code{data}'s variables to group by.
#'
#' @return \code{data}, with additional columns:
#' \itemize{
#'   \item \code{filt_test}, \code{base_pval}, \code{base_thrl}: Evaluation
#'     results of the corresponding parameter values.
#'   \item \code{base_thrl_adj}: Adjusted thresholds for the base test; the
#'     result of \code{base_thrl_adj_method} on the unfiltered thresholds in
#'     \code{base_thrl}.
#'   \item \code{final_result}: \code{logical}, indicating for each hypothesis whether
#'     both the filtration test and the base test of it are rejected (\code{TRUE}) or
#'     not (\code{FALSE}).
#' }
#' @export
#'
#' @examples
perform <- function(data, filt_test, base_pval, base_thrl,
                    base_thrl_adj_method, ...) {
  filt_test <- rlang::enexpr(filt_test)
  base_pval <- rlang::enexpr(base_pval)
  base_thrl <- rlang::enexpr(base_thrl)
  group <- rlang::enexprs(...)

  assertthat::assert_that(is.data.frame(data))

  data %>%
    dplyr::group_by(!!!group) %>%
    dplyr::mutate(
      filt_test = !!filt_test,
      base_pval = !!base_pval,
      base_thrl = !!base_thrl,
      base_thrl_adj = base_thrl_adj_method(
        dplyr::if_else(filt_test, base_thrl, NA_real_)
      ),
      final_result = filt_test & base_pval < base_thrl_adj
    )
}
