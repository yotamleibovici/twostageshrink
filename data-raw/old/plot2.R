library(ggplot2)
library(magrittr)
library(dplyr)
library(latex2exp)


labels <- c(
  sobel = r"(Sobel: ${|X*Y|}/{\sqrt{X^2 + Y^2}}$)",
  maxp = "MaxP",
  product = "$X*Y$",

  screenmin = "ScreenMin",
  l2 = "$X^2 + Y^2$",

  a = 'd'
) %>% latex2exp::TeX()


grid <- expand.grid(
  x = seq(-7, 7, length.out = 1000),
  y = seq(-7, 7, length.out = 1000)
) %>%
  mutate(z = abs(x * y) / sqrt(x^2 + y^2))

grid %>%
  ggplot(aes(
    x = x,
    y = y
  )) +
  geom_tile(aes(fill = factor(
    (abs(x * y) / sqrt(x^2 + y^2) >= 1) +
    (pmin(abs(x), abs(y)) >= 1) +
    (abs(x*y) >= 1) +
    (pmax(abs(x), abs(y)) >= 1) +
    (x^2 + y^2 >= 1)
  ))) +
  scale_fill_hue(labels = labels)
