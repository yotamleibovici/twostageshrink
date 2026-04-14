## code to prepare `d2_performed` dataset goes here

rep_perform <- function(data) {
  perform_spec <- function(data, filt_test, base_pval) {
    perform(
      data = data,
      filt_test = !!rlang::enexpr(filt_test),
      base_pval = !!rlang::enexpr(base_pval),
      base_thrl = 0.05,
      base_thrl_adj_method = function(thrl) thrl,
      config, nobs, exper
    )
  }

  correction <<- "bonferroni"

  l <- list(
    "nofilt_maxp" = data %>%
      perform_spec(
        filt_test = TRUE,
        base_pval = pmax(gpval, bpval) %>%
          dplyr::if_else(filt_test == TRUE, ., NA_real_) %>%
          stats::p.adjust(method = correction)
      ),

    "screenmin_maxp" = data %>%
      perform_spec(
        filt_test = pmin(gpval, bpval) %>% stats::p.adjust(method = correction) < 0.05,
        base_pval = pmax(gpval, bpval) %>%
          dplyr::if_else(filt_test == TRUE, ., NA_real_) %>%
          stats::p.adjust(method = correction)
      ),

    "l2norm_maxp" = data %>%
      perform_spec(
        filt_test = pchisq(nobs*(gestim^2 + bestim^2), 2, lower.tail = FALSE) %>% stats::p.adjust(method = correction) < 0.05,
        base_pval = pmax(gpval, bpval) %>%
          dplyr::if_else(filt_test == TRUE, ., NA_real_) %>%
          stats::p.adjust(method = correction)
      ),

    "nofilt_sobel" = data %>%
      perform_spec(
        filt_test = TRUE,
        base_pval = 2*pnorm(sqrt(nobs)*abs(gestim * bestim) / sqrt(gestim^2 + bestim^2), lower.tail = FALSE) %>%
          dplyr::if_else(filt_test == TRUE, ., NA_real_) %>%
          stats::p.adjust(method = correction)
      ),

    "screenmin_sobel" = data %>%
      perform_spec(
        filt_test = pmin(gpval, bpval) %>% stats::p.adjust(method = correction) < 0.05,
        base_pval = 2*pnorm(sqrt(nobs)*abs(gestim * bestim) / sqrt(gestim^2 + bestim^2), lower.tail = FALSE) %>%
          dplyr::if_else(filt_test == TRUE, ., NA_real_) %>%
          stats::p.adjust(method = correction)
      ),

    "l2norm_sobel" = data %>%
      perform_spec(
        filt_test = pchisq(nobs*(gestim^2 + bestim^2), 2, lower.tail = FALSE) %>% stats::p.adjust(method = correction) < 0.05,
        base_pval = 2*pnorm(sqrt(nobs)*abs(gestim * bestim) / sqrt(gestim^2 + bestim^2), lower.tail = FALSE) %>%
          dplyr::if_else(filt_test == TRUE, ., NA_real_) %>%
          stats::p.adjust(method = correction)
      )

    # "dact" = data %>%
    #   perform_spec(
    #     filt_test = TRUE,
    #     base_pval = DACT::DACT(gpval, bpval, correction = "JC")
    #   )
  )

  l %>% dplyr::bind_rows(.id = "method")
}

d2_performed <- rep_perform(d1_simulated)

usethis::use_data(d2_performed, overwrite = TRUE, compress = "xz")








# ==========================================================================


d2_performed %>%
  select(method, filt_test, gestim, bestim) %>%
  filter(method %in% c("screenmin_maxp", "l2norm_maxp")) %>%
  filter(config == 1 & nobs == 240 & exper < 10) %>%
  ungroup() %>%
  pivot_wider(
    names_from = method,
    values_from = filt_test
  ) %>%
  mutate(filters = paste("screenmin:", screenmin_maxp, "||", "l2norm:", l2norm_maxp)) %>%
  ggplot(aes(x = gestim, y = bestim)) +
  geom_point(aes(color = filters, shape = filters, size = 1))


