library(tidyverse)
library(mvtnorm)

CORRELATION_SD = 0.3

simulate_positive_correlation <- function(nobs, nexper, nhyp, config, correlation_sd = CORRELATION_SD) {
  assertthat::assert_that(
    is.numeric(nobs),
    assertthat::is.count(nexper),
    assertthat::is.count(nhyp),

    is.data.frame(config),
    assertthat::has_name(config, c(
      "altr",
      "g_func", "b_func",
      "prop"
    )),
    is.logical(config$altr),
    is.numeric(config$prop)
  )

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
      g = purrr::map2_dbl(case, nobs, function(case, nobs) config$g_func[[case]](nobs)),
      b = purrr::map2_dbl(case, nobs, function(case, nobs) config$b_func[[case]](nobs))
    ) %>%
    dplyr::group_by(nobs, exper) %>%
    dplyr::mutate(
      gestim = t(mvtnorm::rmvnorm(1, mean = g, 1/nobs[1] * ((1 - correlation_sd) * diag(length(g)) + correlation_sd * matrix(1, length(g), length(g))))), #rnorm(nexper * nhyp, mean = g, sd = 1/sqrt(nobs)) + rnorm(1, sd = correlation_sd * 1/sqrt(nobs)),
      bestim = t(mvtnorm::rmvnorm(1, mean = b, 1/nobs[1] * ((1 - correlation_sd) * diag(length(b)) + correlation_sd * matrix(1, length(b), length(b))))), #rnorm(nexper * nhyp, mean = b, sd = 1/sqrt(nobs)) + rnorm(1, sd = correlation_sd * 1/sqrt(nobs)),
      gpval = 2 * pnorm(-abs(gestim), sd = 1/sqrt(nobs)),
      bpval = 2 * pnorm(-abs(bestim), sd = 1/sqrt(nobs))
    )
}


nobs <- seq(from = 80, by = 80, length.out = 5)
nexper <- 500
nhyp <- 200
base_thrl_adj_method <- function(pvals) { pvals / sum(!is.na(pvals)) }



cases_param <- list(
  tibble::tribble(
    ~prop, ~altr, ~g_func,             ~b_func, ~g_expr, ~b_expr,
    0.65,  FALSE, \(n)              0, \(n) 0, '0', '0',
    0.30,  FALSE, \(n)              0, \(n) 0, '0', '0',
    0.05,  TRUE,  \(n)     3*n^(-1/2), \(n) 3*n^(-1/2), '$3*n^(-1/2)$', '$3*n^(-1/2)$'
  ),

  tibble::tribble(
    ~prop, ~altr, ~g_func,             ~b_func,
    0.70,  FALSE, \(n)              0, \(n) 0,
    0.25,  FALSE, \(n)     3*n^(-1/3), \(n) 0,
    0.05,  TRUE,  \(n) 1 + 3*n^(-1/2), \(n) 3*n^(-1/2)
  ),

  tibble::tribble(
    ~prop, ~altr, ~g_func,             ~b_func,
    0.25,  FALSE, \(n)              0, \(n) 0,
    0.35,  FALSE, \(n)     3*n^(-1/2), \(n) 0,
    0.15,  FALSE, \(n) 1 + 3*n^(-1/2), \(n) 0,
    0.10,  TRUE,  \(n) 1 + 3*n^(-1/2), \(n) 3*n^(-1/2)
  )
)

# cases_param_dbl <- cases_param_expr %>%
#   dplyr::mutate_if(purrr::is_list, ~ purrr::map_dbl(., eval))

rep_simulate <- function() {
  list(
    simulate_positive_correlation(
      nobs = nobs,
      nexper = nexper,
      nhyp = nhyp,
      config = cases_param[[1]]
    ),
    simulate_positive_correlation(
      nobs = nobs,
      nexper = nexper,
      nhyp = nhyp,
      config = cases_param[[2]]
    ),
    simulate_positive_correlation(
      nobs = nobs,
      nexper = nexper,
      nhyp = nhyp,
      config = cases_param[[3]]
    )
  ) %>%
    dplyr::bind_rows(.id = "config")
}

set.seed(CORRELATION_SD * 10)
d1_simulated <- rep_simulate()
# usethis::use_data(d1_simulated, overwrite = TRUE, compress = "xz")


# =================================================================


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

# ===============================================================


## code to prepare `d3_evaluated` dataset goes here

rep_evaluate <- function(data) {
  data %>% evaluate(method, config, nobs, exper)
}

d3_evaluated <- rep_evaluate(d2_performed)

# ==============================================================


rep_measure <- function(data) {
  data %>% measure(method, config, nobs)
}

d4_measured <- rep_measure(d3_evaluated)

# ================================================================

library(tidyverse)
library(twostageshrink)
library(ggpubr)

#setwd(r"(C:\Users\yotam\My Drive\Studies\MSc\Yair-Yotam-SuperEfficiency\twostageshrink)")

method_names <- c(
  "nofilt_maxp" = "No Filtration / Max p-value",
  "screenmin_maxp" = "ScreenMin / Max p-value",
  "l2norm_maxp" = "L2-norm / Max p-value",
  "nofilt_sobel" = "No Filtration / Sobel",
  "screenmin_sobel" = "ScreenMin / Sobel",
  "l2norm_sobel" = "L2 norm / Sobel"
)

true_rejections_plots <- d3_evaluated %>%
  group_by(nobs, method, config) %>%
  summarise(mean_trej = mean(trej)) %>%
  ungroup() %>%
  ggplot2::ggplot(ggplot2::aes(
    x = nobs %>% forcats::as_factor(),
    y = mean_trej,
    colour = method %>% forcats::fct_relevel(
      "nofilt_maxp",
      "nofilt_sobel",
      "screenmin_maxp",
      "screenmin_sobel",
      "l2norm_maxp",
      "l2norm_sobel"
    )
  )) +
  ggplot2::scale_colour_discrete(labels = method_names %>% latex2exp::TeX()) +
  ggplot2::labs(
    x = "Number of Observations",
    y = "True Rejections",
    colour = "Method"
  ) +
  ggplot2::geom_point() +
  ggplot2::geom_line(aes(group = method)) +
  facet_wrap(vars(config), nrow = 3, labeller =
               as_labeller(function(string) paste("Configuration", string)))




fwer_plots <- d4_measured %>%
  ggplot2::ggplot(ggplot2::aes(
    x = nobs %>% forcats::as_factor(),
    y = fwer,
    colour = method %>% forcats::fct_relevel(
      "nofilt_maxp",
      "nofilt_sobel",
      "screenmin_maxp",
      "screenmin_sobel",
      "l2norm_maxp",
      "l2norm_sobel"
    )
  )) +
  ggplot2::scale_colour_discrete(labels = method_names %>% latex2exp::TeX()) +
  ggplot2::labs(
    x = "Number of Observations",
    y = "FWER",
    colour = "Method"
  ) +
  ggplot2::geom_point() +
  ggplot2::geom_line(aes(group = method)) +
  ggplot2::facet_wrap(vars(config), nrow = 3, labeller =
                        as_labeller(function(string) paste("Configuration", string)))




ggarrange(true_rejections_plots, fwer_plots, ncol = 2, common.legend = TRUE)

ggsave(
  paste0("pvals-all-new-corr-", CORRELATION_SD, "-", nexper, "-", nhyp, ".pdf"),
  path = ".",
  width = 8, height = 6
)

