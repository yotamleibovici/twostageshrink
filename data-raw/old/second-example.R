library(tidyverse)

Sys.setenv(`_R_USE_PIPEBIND_` = TRUE)

set.seed(0)
n_exp <- 1e4

df <- expand_grid(
  tibble(experiment = 1:n_exp),
  tibble(
    mu_x = 0 |> rep(100),
    mu_y = 0 |> rep(100)
  )
) |>
  mutate(
    x = rnorm(length(mu_x), mean = mu_x),
    y = rnorm(length(mu_y), mean = mu_y),
    sobel = (x * y) / sqrt(x ^ 2 + y ^ 2)
  )

adjustment_method = "bonferroni"
base_threshold = 0.05

df2 <- df |>
  group_by(experiment) |>

  mutate(filtration_p_value = pchisq(x ^ 2 + y ^ 2, 2, lower.tail = FALSE)) |>
  mutate(adjusted_filtration_p_value = filtration_p_value) |>
  mutate(rejected_filtration = adjusted_filtration_p_value <= 0.005) |>

  mutate(base_p_value = pnorm(-abs(sobel))) |>
  mutate(filtered_base_p_value =
           base_p_value |> . => if_else(rejected_filtration, ., NA)) |>
  mutate(adjusted_base_p_value =
           p.adjust(base_p_value, method = adjustment_method)) |>
  mutate(adjusted_filtered_base_p_value =
           p.adjust(filtered_base_p_value, method = adjustment_method)) |>

  summarise(unfiltered_false_alarms =
              sum(adjusted_base_p_value <= base_threshold & mu_y == 0),
            filtered_false_alarms =
              sum(adjusted_filtered_base_p_value |> replace_na(1) <= base_threshold & mu_y == 0)) |>
  summarise(unfiltered_fwer = mean(unfiltered_false_alarms >= 1),
            filtered_fwer = mean(filtered_false_alarms >= 1))



# df |>
#   summarise(mean(2 * tsobel <= 0.05 &
#                    pchisq(x ^ 2 + y ^ 2, 2, lower.tail = FALSE) <= 0.05))
