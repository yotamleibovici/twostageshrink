nobs <- seq(from = 80, by = 80, length.out = 5)
nexper <- 300
nhyp <- 80
base_thrl_adj_method <- function(pvals) { pvals / sum(!is.na(pvals)) }



cases_param <- list(
  tibble::tribble(
    ~prop, ~altr, ~g_func,             ~b_func,
    0.65,  FALSE, \(n)              0, \(n) 0,
    0.30,  FALSE, \(n)     3*n^(-1/2), \(n) 0,
    0.05,  TRUE,  \(n)     3*n^(-1/2), \(n) 3*n^(-1/2),
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
    simulate(
      nobs = nobs,
      nexper = nexper,
      nhyp = nhyp,
      config = cases_param[[1]]
    ),
    simulate(
      nobs = nobs,
      nexper = nexper,
      nhyp = nhyp,
      config = cases_param[[2]]
    ),
    simulate(
      nobs = nobs,
      nexper = nexper,
      nhyp = nhyp,
      config = cases_param[[3]]
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
