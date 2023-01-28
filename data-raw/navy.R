#' @importFrom magrittr `%$%`
#' @importFrom magrittr `%>%`

library(MultiMed)
#library(twostageshrink)
data("NavyAdenoma")

navy_adenoma <- MultiMed::NavyAdenoma %>% tibble::as_tibble()

# prevalence of adenoma in a population of interest
prev <- 0.228
p <- mean(navy_adenoma$Adenoma)

nobs_vec <- 258 #c(seq(from = 40, to = 258, by = 40), 258)

set.seed(1)

navy_adenoma_for_modelling <- navy_adenoma %>%
  dplyr::mutate(weight = dplyr::case_when( #TODO ?
    Adenoma == 1 ~ prev / p,
    Adenoma == 0 ~ (1 - prev) / (1 - p)
  )) %>%
  tidyr::pivot_longer(
    glycine:erythritol,
    names_to = "metabolite",
    values_to = "level"
  ) %>%
  dplyr::nest_by(metabolite, .key = "data")


modelled_navy_adenoma <- navy_adenoma_for_modelling %>%
  dplyr::mutate(
    mediator_model = list(
      purrr::map(nobs_vec, function(nobs) stats::lm(
        formula = level ~ Fish + BMI + Female + Age + Smoking,
        data = data,
        subset = seq_len(nrow(data)) %>% sample(nobs),
        weights = data$weight
      )) %>%
        stats::setNames(nobs_vec)
    ),
    outcome_model = list(
      purrr::map(nobs_vec, function(nobs) stats::glm(
        formula = Adenoma ~ Fish + BMI + Female + Age + Smoking + level,
        family = stats::binomial,
        data = data,
        subset = seq_len(nrow(data)) %>% sample(nobs)
      )) %>%
        stats::setNames(nobs_vec)
    )
  ) %>%
  tidyr::unnest_longer(
    c(mediator_model, outcome_model),
    indices_to = "nobs_{col}"
  ) %>%
  dplyr::mutate(nobs = nobs_mediator_model %>% as.integer()) %>%
  dplyr::select(!c(nobs_mediator_model, nobs_outcome_model))


statistics_navy_adenoma <- modelled_navy_adenoma %>%
  dplyr::rowwise() %>%

  dplyr::mutate(
    mediator_model = mediator_model %>%
      broom::tidy() %>%
      dplyr::filter(term == "Fish") %>%
      dplyr::select(!term) %>%
      dplyr::rename_with(function(col) glue::glue("mediator_{col}")),

    outcome_model = outcome_model %>%
      broom::tidy() %>%
      dplyr::filter(term == "level") %>%
      dplyr::select(!term) %>%
      dplyr::rename_with(function(col) glue::glue("outcome_{col}"))
  ) %>%

  dplyr::select(!data) %>%
  tidyr::unnest(c(mediator_model, outcome_model))

#------------------------------------------------------------------------------#


rep_perform_navy <- function(data) {
  perform_spec <- function(filt_test) {
    perform(
      data = data,
      filt_test = !!rlang::enexpr(filt_test),
      base_pval = pmax(mediator_p.value, outcome_p.value) %>%
        dplyr::if_else(filt_test == TRUE, ., NA_real_) %>%
        stats::p.adjust(method = "BH"),
      base_thrl = 0.05,
      base_thrl_adj_method = function(thrl) thrl, # function(thrl) { thrl / sum(!is.na(thrl)) },
      nobs
    )
  }

  filter_names <- c(
    "nofilt", "screenmin", "l2norm",
    "product08", "product09", "product1"
  )

  filters <- list(
    rlang::expr(TRUE),
    rlang::expr(pmin(mediator_p.value, outcome_p.value) < 0.05/149),
    rlang::expr(pchisq(mediator_statistic^2 + outcome_statistic^2, 2, lower.tail = FALSE) < 0.05/149),
    rlang::expr(abs(mediator_estimate*outcome_estimate) > 1.2*nobs^(-0.8)),
    rlang::expr(abs(mediator_estimate*outcome_estimate) > 2*nobs^(-0.9)),
    rlang::expr(abs(mediator_estimate*outcome_estimate) > 3*nobs^(-1))
  )

  names(filters) <- filter_names


  filters %>%
    purrr::map(perform_spec) %>%
    dplyr::bind_rows(.id = "method")
}

#------------------------------------------------------------------------------#

performed_navy <- statistics_navy_adenoma %>% rep_perform_navy()




#==============================================================================#




medsFish <- medTest(
  E = navy_adenoma %>% dplyr::pull(Fish),
  M = navy_adenoma %>% dplyr::select(glycine:dplyr::last_col()),
  Y = navy_adenoma %>% dplyr::pull(Adenoma),
  Z = navy_adenoma %>% dplyr::select(BMI:Smoking),
  nperm = 1000,
  w = navy_adenoma %$% dplyr::case_when(
    Adenoma == 1 ~ prev / p,
    Adenoma == 0 ~ (1 - prev) / (1 - p)
  ),
  useWeightsZ = FALSE
)

medsFish[which.min(medsFish[,"p"]),,drop=FALSE]
