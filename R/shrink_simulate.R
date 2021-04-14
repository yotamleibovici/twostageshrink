#' Simulate data for mediation estimation
#'
#' @param nobs Number of observations (may be a vector)
#' @param nrep Number of experiments for each \code{nobs} value.
#' @param config A data frame, consists information about the different cases
#'   (values of true parameters) in the following columns: \code{gfactor} and
#'   \code{bfactor}; and \code{gexp} and \code{bexp}: \code{numeric}
#'    constants used to define the true parameters \eqn{\gamma_n} and
#'   \eqn{\beta_n}.
#'
#' @return A tibble with \code{length(nobs) * nrep * nrows(config)} rows. Each row
#'   corresponds to an realization of an estimator, assumed to be an
#'   approximation to an asymptotically normal estimator with root-\eqn{n} rate
#'   (based on \eqn{n} observations). The simulations here are based on the
#'   normal approximations, and consists of the following columns:
#'   \itemize{
#'     \item \code{nobs}: Number of observations (affecting the standard
#'       deviation). Values are from the given value in the corresponding
#'       parameter \code{nobs}.
#'     \item \code{case}: Identifier of the true case of the hypothesis.
#'     \item \code{g} and \code{b}: The true parameters. Their values depends
#'       on the parameters in the config, and on the current \code{nobs} value:
#'       \eqn{\gamma_n} = \code{gfactor * nobs^gexp}; \eqn{\beta_n} =
#'       \code{bfactor * nobs^bexp}.
#'     \item \code{gestim} and \code{bestim}: estimators for \eqn{\gamma_n} and
#'       \eqn{\beta_n}, distributed independently from Normal distribution with
#'       mean \eqn{\gamma_n} and \eqn{\beta_n} respectively, and with standard
#'       deviation equals \eqn{1/\sqrt n}.
#' @export
#'
#' @examples
shrink_simulate <- function(nobs, nrep, config) {
  assertthat::assert_that(
    is.numeric(nobs),
    assertthat::is.count(nrep),

    is.data.frame(config),
    assertthat::has_name(config, c(
      "gfactor", "bfactor",
      "gexp", "bexp"
    )),
    is.numeric(config$gfactor), is.numeric(config$bfactor),
    is.numeric(config$gexp), is.numeric(config$bexp)
  )

  tidyr::expand_grid(
    nobs,
    config %>% dplyr::mutate(case = seq_len(dplyr::n()))
  ) %>%
    dplyr::mutate(
      g = gfactor * nobs^(-gexp),
      b = bfactor * nobs^(-bexp),
      param = g*b
    ) %>%
    dplyr::slice(rep(seq_len(dplyr::n()), each = nrep)) %>%
    dplyr::mutate(
      gestim = rnorm(g, mean = g, sd = 1/sqrt(nobs)),
      bestim = rnorm(b, mean = b, sd = 1/sqrt(nobs))
    )
}
