## code to prepare `d3_evaluated` dataset goes here

rep_evaluate <- function(data) {
  data %>% evaluate(method, config, nobs, exper)
}

d3_evaluated <- rep_evaluate(d2_performed)

usethis::use_data(d3_evaluated, overwrite = TRUE)
