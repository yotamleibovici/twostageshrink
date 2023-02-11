library(tidyverse)
library(magrittr)
library(MultiMed)

data("NavyAdenoma")

navy_adenoma <- NavyAdenoma %>% tibble::as_tibble()

# prevalence of adenoma in a population of interest
prev <- 0.228
p <- mean(navy_adenoma$Adenoma)

set.seed(12)

navy_adenoma_for_modelling <- navy_adenoma %>%
  dplyr::mutate(weight = dplyr::case_when( # TODO ?
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
  )


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
  perform_spec <- function(data, filt_test, base_pval) {
    perform(
      data = data,
      filt_test = !!rlang::enexpr(filt_test),
      base_pval = !!rlang::enexpr(base_pval),
      base_thrl = 0.05,
      base_thrl_adj_method = function(thrl) thrl # function(thrl) { thrl / sum(!is.na(thrl)) },
    )
  }

  l <- list(
    "nofilt-maxp" = data %>%
      perform_spec(
        filt_test = TRUE,
        base_pval = pmax(mediator_p.value, outcome_p.value) %>%
          dplyr::if_else(filt_test == TRUE, ., NA_real_) %>%
          stats::p.adjust(method = "BH")
      ),

    "screenmin-maxp" = data %>%
      perform_spec(
        filt_test = pmin(mediator_p.value, outcome_p.value) < 0.05 / 149,
        base_pval = pmax(mediator_p.value, outcome_p.value) %>%
          dplyr::if_else(filt_test == TRUE, ., NA_real_) %>%
          stats::p.adjust(method = "BH")
      ),

    "l2norm-maxp" = data %>%
      perform_spec(
        filt_test = pchisq(mediator_statistic^2 + outcome_statistic^2, 2, lower.tail = FALSE) < 0.05 / 149,
        base_pval = pmax(mediator_p.value, outcome_p.value) %>%
          dplyr::if_else(filt_test == TRUE, ., NA_real_) %>%
          stats::p.adjust(method = "BH")
      ),

    "nofilt-sobel" = data %>%
      perform_spec(
        filt_test = TRUE,
        base_pval = 2*pnorm(
          abs(mediator_statistic * outcome_statistic) /
            sqrt(mediator_statistic^2 + outcome_statistic^2),
          lower.tail = FALSE
        ) %>%
          dplyr::if_else(filt_test == TRUE, ., NA_real_) %>%
          stats::p.adjust(method = "BH")
      ),

    "screenmin-sobel" = data %>%
      perform_spec(
        filt_test = pmin(mediator_p.value, outcome_p.value) < 0.05 / 149,
        base_pval = 2*pnorm(
          abs(mediator_statistic * outcome_statistic) /
            sqrt(mediator_statistic^2 + outcome_statistic^2),
          lower.tail = FALSE
        ) %>%
          dplyr::if_else(filt_test == TRUE, ., NA_real_) %>%
          stats::p.adjust(method = "BH")
      ),

    "l2norm-sobel" = data %>%
      perform_spec(
        filt_test = pchisq(mediator_statistic^2 + outcome_statistic^2, 2, lower.tail = FALSE) < 0.05 / 149,
        base_pval = 2 * pnorm(
          abs(mediator_statistic * outcome_statistic) /
            sqrt(mediator_statistic^2 + outcome_statistic^2),
          lower.tail = FALSE
        ) %>%
          dplyr::if_else(filt_test == TRUE, ., NA_real_) %>%
          stats::p.adjust(method = "BH")
      ),

    "dact" = data %>%
      perform_spec(
        filt_test = TRUE,
        base_pval = DACT::DACT(mediator_p.value, outcome_p.value, correction = "JC")
      )
  )

  l %>% dplyr::bind_rows(.id = "method") %>%
    mutate(method = method %>% forcats::as_factor())



}

#------------------------------------------------------------------------------#

performed_navy <- statistics_navy_adenoma %>% rep_perform_navy()




# ==============================================================================#




medsFish <- medTest(
  E = navy_adenoma %>% dplyr::pull(Fish),
  M = navy_adenoma %>% dplyr::select(glycine:dplyr::last_col(offset = 1)),
  Y = navy_adenoma %>% dplyr::pull(Adenoma),
  Z = navy_adenoma %>% dplyr::select(BMI:Smoking),
  nperm = 1000,
  w = navy_adenoma %$% dplyr::case_when(
    Adenoma == 1 ~ prev / p,
    Adenoma == 0 ~ (1 - prev) / (1 - p)
  ),
  useWeightsZ = FALSE
)
rownames(medsFish) <- colnames(NavyAdenoma[,-c(1:5, 154)])

medsFish[which.min(medsFish[, "p"]), , drop = FALSE]
