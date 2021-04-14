nobs <- seq(from = 80, by = 80, length.out = 5)
nexper <- 500
nhyp <- 200
base_thrl_adj_method <- function(pvals) { pvals / sum(!is.na(pvals)) }



config_prop <- list(
  c(0.65,   # null (0, 0)
    0.00,   # null (3n^(-3/4), 0)
    0.30,   # null (3n^(-1/2), 0)
    0.00,   # null (3n^(-1/3), 0)
    0.00,   # null (1 + 3n^(-1/2), 0)
    0.05,   # altr (3n^(-1/2), 3n^(-1/2))
    0.00,   # altr (3n^(-1/3), 3n^(-1/2))
    0.00),  # altr (1 + 3n^(-1/2), 3n^(-1/2))

  c(0.25,   # null (0, 0)
    0.15,   # null (3n^(-3/4), 0)
    0.25,   # null (3n^(-1/2), 0)
    0.10,   # null (3n^(-1/3), 0)
    0.15,   # null (1 + 3n^(-1/2), 0)
    0.04,   # altr (3n^(-1/2), 3n^(-1/2))
    0.03,   # altr (3n^(-1/3), 3n^(-1/2))
    0.03),  # altr (1 + 3n^(-1/2), 3n^(-1/2))

  c(0.25,   # null (0, 0)
    0.00,   # null (3n^(-3/4), 0)
    0.35,   # null (3n^(-1/2), 0)
    0.00,   # null (3n^(-1/3), 0)
    0.15,   # null (1 + 3n^(-1/2), 0)
    0.00,   # altr (3n^(-1/2), 3n^(-1/2))
    0.00,   # altr (3n^(-1/3), 3n^(-1/2))
    0.10)   # altr (1 + 3n^(-1/2), 3n^(-1/2))
)

cases_param_expr <- tibble::tribble(
  ~descr, ~altr, ~gaddend, ~gfactor, ~gexp, ~baddend, ~bfactor, ~bexp,
  "nulllxl", FALSE, 0, 0,                0, 0, 0,                0,
  "nullll",  FALSE, 0, 3, rlang::expr(3/4), 0, 0,                0,
  "nulllm",  FALSE, 0, 3, rlang::expr(1/2), 0, 0,                0,
  "nullls",  FALSE, 0, 3, rlang::expr(1/3), 0, 0,                0,
  "nulllxs", FALSE, 1, 3, rlang::expr(1/2), 0, 0,                0,
  "altrlm",  TRUE,  0, 3, rlang::expr(1/2), 0, 3, rlang::expr(1/2),
  "altrls",  TRUE,  0, 3, rlang::expr(1/3), 0, 3, rlang::expr(1/2),
  "altrlxs", TRUE,  1, 3, rlang::expr(1/2), 0, 3, rlang::expr(1/2)
)

cases_param_dbl <- cases_param_expr %>%
  dplyr::mutate_if(purrr::is_list, ~ purrr::map_dbl(., eval))

rep_simulate <- function() {
  purrr::map(
    config_prop,
    ~ simulate(
      nobs = nobs,
      nexper = nexper,
      nhyp = nhyp,
      config = dplyr::mutate(cases_param_dbl, prop = .)
    )
  ) %>%
    dplyr::bind_rows(.id = "config")
}

set.seed(1)
d1_simulated <- rep_simulate()
usethis::use_data(d1_simulated, overwrite = TRUE, compress = "xz")

# performed <- rep_perform(simulated)
# data_evaluated <- rep_evaluate(data_performed)
# data_measured <- rep_measure(data_evaluated)
#
# usethis::use_data(DATASET, overwrite = TRUE)
