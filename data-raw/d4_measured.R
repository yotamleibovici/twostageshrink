## code to prepare `d4_measure` dataset goes here

rep_measure <- function(data) {
  data %>% measure(method, config, nobs)
}

d4_measured <- rep_measure(d3_evaluated)

usethis::use_data(d4_measured, overwrite = TRUE)
