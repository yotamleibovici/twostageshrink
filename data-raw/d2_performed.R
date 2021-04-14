## code to prepare `d2_performed` dataset goes here

rep_perform <- function(data) {
  perform_spec <- function(filt_test) {
    perform(
      data = data,
      filt_test = !!rlang::enexpr(filt_test),
      base_pval = pmax(gpval, bpval),
      base_thrl = 0.05,
      base_thrl_adj_method = function(thrl) { thrl / sum(!is.na(thrl)) },
      config, nobs, exper
    )
  }

  filter_names <- c(
    "nofilt", "l2norm", "screenmin",
    "product08", "product09", "product1"
  )

  filters <- list(
    expr(TRUE),
    expr(pmin(gpval, bpval) < 0.001),
    expr(pchisq(nobs*(gestim^2 + bestim^2), 2, lower.tail = FALSE) < 0.001),
    expr(abs(gestim*bestim) > 1.2*nobs^(-0.8)),
    expr(abs(gestim*bestim) > 2*nobs^(-0.9)),
    expr(abs(gestim*bestim) > 3*nobs^(-1))
  )

  names(filters) <- filter_names

  # list(
  #   "nofilt" = data %>%
  #     perform_spec(filt_test = TRUE),
  #
  #   "screenmin" = data %>%
  #     perform_spec(filt_test = pmin(gpval, bpval) < 0.001),
  #
  #   "l2norm" = data %>%
  #     perform_spec(filt_test = pchisq(
  #       nobs*(gestim^2 + bestim^2), 2, lower.tail = FALSE
  #     ) < 0.001),
  #
  #   "product08" = data %>%
  #     perform_spec(filt_test = abs(gestim*bestim) > 1.2*nobs^(-0.8)),
  #
  #   "product09" = data %>%
  #     perform_spec(filt_test = abs(gestim*bestim) > 2*nobs^(-0.9)),
  #
  #   "product1" = data %>%
  #     perform_spec(filt_test = abs(gestim*bestim) > 3*nobs^(-1))
  # ) %>%

  filters %>%
    purrr::map(perform_spec) %>%
    dplyr::bind_rows(.id = "method")
}

d2_performed <- rep_perform(d1_simulated)

usethis::use_data(d2_performed, overwrite = TRUE, compress = "xz")
