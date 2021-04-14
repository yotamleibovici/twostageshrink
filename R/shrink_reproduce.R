configs <- list(
  tibble::tribble(
    ~kval, ~gexp, ~gfactor, ~bexp, ~bfactor,
    "kxs", 0.5, 1, 0.6, 1,
    "ks",  0.5, 1, 0.5, 1,
    "km",  0.5, 2, 0.5, sqrt(5/3),
    "kl",  0.5, 2, 0.5, 2,
    "kxl", 0.4, 2, 0.4, 1
  ),
  tibble::tribble(
    ~kval, ~gexp, ~gfactor, ~bexp, ~bfactor,
    "ks", 0.5, 0.7, 0.5, 0.7,
    "km", 0.5, 1.075, 0.5, 1.075,
    "kl", 0.5, 2, 0.5, 2
  ),
  tibble::tribble(
    ~kval, ~gexp, ~gfactor, ~bexp, ~bfactor,
    "km", 1, 1, 1, 1
  )
)

filtexps <- list(0.7, 1, 1.5)
filtfactors <- list(4, 2.5, 2.5)

#' Reproduce figures
#'
#' @param configid Configuration identifier.
#'
#' @return Plot of the MSE-ratio.
#' @export
#'
#' @examples
#' shrink_reproduce(1)
shrink_reproduce <- function(configid) {
  assertthat::assert_that(
    assertthat::is.count(configid),
    configid >= 1 && configid <= 3
  )

  set.seed(1)

  shrink_simulate(
    nobs = 10*seq(from = 10, to = 1000, by = 10),
    nrep = 3000,
    config = configs[[configid]]
  ) %>%

    shrink_estimate(
      filtexp = filtexps[[configid]], filtfactor = filtfactors[[configid]],
      estim_base = gestim*bestim, estim_filt = abs(gestim*bestim)
    ) %>%

    shrink_evaluate(nobs, case) %>%

    ggplot2::ggplot(ggplot2::aes(
      x = nobs,
      y = mse_ratio,
      colour = case %>%
        forcats::as_factor() %>%
        forcats::fct_reorder2(nobs, mse_ratio)
    )) +
    ggplot2::labs(
      x = "Number of Observations",
      y = "MSE-Ratio",
      colour = "Case"
    ) +
    ggplot2::geom_point()
}

# generate_graph_texdefs(
#   title = "mseratiob", filtexp = 1, filtfactor = 2.5,
#   estim_base = gestim*bestim, estim_filt = abs(gestim*bestim),
#   param_table = tribble(
#     ~kval, ~gexp, ~gfactor, ~bexp, ~bfactor,
#     "ks", 0.5, 0.7, 0.5, 0.7,
#     "km", 0.5, 1.075, 0.5, 1.075,
#     "kl", 0.5, 2, 0.5, 2,
#   )
# )
#
# generate_graph_texdefs(
#   title = "mseratioc", filtexp = 1.5, filtfactor = 2.5,
#   estim_base = gestim*bestim, estim_filt = abs(gestim*bestim),
#   param = tribble(
#     ~kval, ~gexp, ~gfactor, ~bexp, ~bfactor,
#     "km", 1, 1, 1, 1,
#   )
# )
