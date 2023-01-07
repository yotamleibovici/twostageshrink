navy_adenoma <- NavyAdenoma %>% tibble::as_tibble()

sampled_navy_adenoma <-
  tibble::tibble(nobs = c(seq(from = 40, to = 258, by = 40), 258)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(sampled_data = list(
    navy_adenoma %>%
      tibble::rowid_to_column(var = "person") %>%
      dplyr::slice_sample(n = nobs)
  )) %>%
  tidyr::unnest(sampled_data)

# prevalence of adenoma in a population of interest
prev <- 0.228
p <- navy_adenoma %$% mean(Adenoma)
p


navy_adenoma_for_modelling <- sampled_navy_adenoma %>%
  dplyr::mutate(weight = dplyr::case_when( #TODO ?
    Adenoma == 1 ~ prev / p,
    Adenoma == 0 ~ (1 - prev) / (1 - p)
  )) %>%
  tidyr::pivot_longer(
    glycine:erythritol,
    names_to = "metabolite",
    values_to = "level"
  ) %>%
  dplyr::nest_by(nobs, metabolite, .key = "data")


modelled_navy_adenoma <- navy_adenoma_for_modelling %>%
  dplyr::mutate(
    mediator_model = list(stats::lm(
        formula = level ~ Fish + BMI + Female + Age + Smoking,
        data = data,
        weights = data$weight
    )),

    outcome_model = list(stats::glm(
        formula = Adenoma ~ Fish + BMI + Female + Age + Smoking + level,
        family = stats::binomial,
        data = data
    ))
  ) %>%

  mutate(
    tidy_mediator_model = mediator_model %>%
      broom::tidy() %>%
      dplyr::filter(term == "Fish") %>%
      dplyr::select(!term) %>%
      dplyr::rename_with(function(col_name) glue::glue("mediator_{col_name}")),

    tidy_outcome_model = outcome_model %>%
      broom::tidy() %>%
      dplyr::filter(term == "level") %>%
      dplyr::select(!term) %>%
      dplyr::rename_with(function(col_name) glue::glue("outcome_{col_name}"))
  ) %>%

  dplyr::select(!c(data, mediator_model, outcome_model)) %>%
  tidyr::unnest(c(tidy_mediator_model, tidy_outcome_model))

#------------------------------------------------------------------------------#


rep_perform_navy <- function(data) {
  perform_spec <- function(filt_test) {
    perform(
      data = data,
      filt_test = !!rlang::enexpr(filt_test),
      base_pval = pmax(mediator_p.value, outcome_p.value),
      base_thrl = 0.05,
      base_thrl_adj_method = function(thrl) { thrl / sum(!is.na(thrl)) },
      nobs
    )
  }

  filter_names <- c(
    "nofilt", "l2norm", "screenmin",
    "product08", "product09", "product1"
  )

  filters <- list(
    rlang::expr(TRUE),
    rlang::expr(pmin(mediator_p.value, outcome_p.value) < 0.001),
    rlang::expr(pchisq(nobs*(mediator_estimate^2 + outcome_estimate^2), 2, lower.tail = FALSE) < 0.001),
    rlang::expr(abs(mediator_estimate*outcome_estimate) > 1.2*nobs^(-0.8)),
    rlang::expr(abs(mediator_estimate*outcome_estimate) > 2*nobs^(-0.9)),
    rlang::expr(abs(mediator_estimate*outcome_estimate) > 3*nobs^(-1))
  )

  names(filters) <- filter_names


  filters %>%
    purrr::map(perform_spec) %>%
    dplyr::bind_rows(.id = "method")
}
