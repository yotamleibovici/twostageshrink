#' Simulate data for the mediation hypothesis
#'
#' @param nobs Number of observations (may be a vector)
#' @param nexper Number of experiments for each \code{nobs} value.
#' @param nhyp Total number of hypotheses (nulls and alternatives) in each
#'   experiment.
#' @param config A data frame, consists information about the different cases
#'   (values of true parameters) in the following columns:
#'   \itemize{
#'     \item \code{altr}: \code{logical}, indicating whether this is a case of
#'       the alternative.
#'     \item \code{gaddend} and \code{baddend}; \code{gfactor} and
#'     \code{bfactor}; and \code{gexp} and \code{bexp}: \code{numeric}
#'       constants used to define the true parameters \eqn{\gamma_n} and
#'       \eqn{\beta_n}.
#'     \item \code{prop}: \code{numeric}, discrete distribution for the
#'       different cases in the configuration.
#'   }
#'
#' @return A tibble with \code{length(nobs) * nexper * nhyp} rows. Each row
#'   corresponds to an realization of an estimator, assumed to be an
#'   approximation to an asymptotically normal estimator with root-\eqn{n} rate
#'   (based on \eqn{n} observations). The simulations here are based on the
#'   normal approximations, and consists of the following columns:
#'   \itemize{
#'     \item \code{nobs}: Number of observations (affecting the standard
#'       deviation). Values are from the given value in the corresponding
#'       parameter \code{nobs}.
#'     \item \code{exper}: Serial number of the experiment in the current
#'       \code{nobs} value.
#'     \item \code{case}: Identifier of the true case of the hypothesis. In each
#'       experiment the hypotheses are distributed according to \code{config}.
#'     \item \code{g} and \code{b}: The true parameters. Their values depends
#'       on the parameters in the config, and on the current \code{nobs} value:
#'       \eqn{\gamma_n} = \code{gaddend + gfactor * nobs^gexp}; \eqn{\beta_n} =
#'       \code{baddend + bfactor * nobs^bexp}.
#'     \item \code{gestim} and \code{bestim}: estimators for \eqn{\gamma_n} and
#'       \eqn{\beta_n}, distributed independently from Normal distribution with
#'       mean \eqn{\gamma_n} and \eqn{\beta_n} respectively, and with standard
#'       deviation equals \eqn{1/\sqrt n}.
#'     \item \code{gpval} and \code{bpval}: Corresponding \eqn{p}-values,
#'       under the null hypotheses \eqn{\gamma = 0} and \eqn{\beta = 0},
#'       respectively.
#' @export
#'
#' @examples
simulate <- function(nobs, nexper, nhyp, config) {
  ntotal <- length(nobs) * nexper * nhyp

  tidyr::expand_grid(
    nobs = nobs,
    exper = seq_len(nexper),
    case = seq_len(nrow(config)) %>%
      rep(times = nhyp * config$prop) %>%
      rep(length.out = nhyp)
  ) %>%
    dplyr::mutate(
      altr = config$altr[case],
      g = config$gaddend[case] +
        config$gfactor[case] * nobs^(-config$gexp[case]),
      b = config$baddend[case] +
        config$bfactor[case] * nobs^(-config$bexp[case]),
      gestim = rnorm(ntotal, mean = g, sd = 1/sqrt(nobs)),
      bestim = rnorm(ntotal, mean = b, sd = 1/sqrt(nobs)),
      gpval = 2 * pnorm(-abs(gestim), sd = 1/sqrt(nobs)),
      bpval = 2 * pnorm(-abs(bestim), sd = 1/sqrt(nobs))
    )
}
