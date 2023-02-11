colours <- c(
  sobel = "10PB 5/10" %>% munsell::mnsl(),
  maxp = "10PB 5/10" %>% munsell::rygbp(2) %>% munsell::mnsl(),
  product = "10PB 5/10" %>% munsell::rygbp(4) %>% munsell::mnsl(),

  screenmin = "7.5YR 5/10" %>% munsell::pbgyr(2) %>% munsell::mnsl(),
  l2 = "7.5YR 5/10" %>% munsell::mnsl()
)
labels <- c(
  sobel = "Sobel: ${|X*Y|}/{\\sqrt{X^2 + Y^2}}$",
  maxp = "MaxP",
  product = "$X*Y$",

  screenmin = "ScreenMin",
  l2 = "$X^2 + Y^2$"
) %>% latex2exp::TeX()

line_types <- c("Base" = "solid", "Filtration" = "dashed")




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
  geom_contour_filled(
    aes(
      z = z >= 1,
      fill = z >= 1,
      colour = "sobel",
      linetype = "Base"
    ),
    fill = "10PB 5/10" %>% munsell::mnsl(),
    alpha = 0.4
  ) +
  geom_contour_filled(
    aes(
      z = pmin(abs(x), abs(y)) >= 1,
      fill = pmin(abs(x), abs(y)) >= 1,
      colour = "maxp",
      linetype = "Base",
    ),
    fill = "10PB 5/10" %>% munsell::rygbp(2) %>% munsell::mnsl(),
    alpha = 0.4
  ) +
  geom_contour_filled(
    aes(
      z = abs(x*y) >= 1,
      fill = abs(x*y) >= 1,
      linetype = "Base",
      colour = "product"
    ),

    fill = "10PB 5/10" %>% munsell::rygbp(4) %>% munsell::mnsl(),
    alpha = 0.4
  ) +
  geom_contour_filled(
    aes(
      z = pmax(abs(x), abs(y)) >= 1,
      fill = pmax(abs(x), abs(y)) >= 1,
      linetype = "Filtration",
      colour = "screenmin"
    ),
    fill = "7.5YR 5/10" %>% munsell::pbgyr(2) %>% munsell::mnsl(),
    alpha = 0.4
  ) +
  geom_contour_filled(
    aes(
      z = x^2 + y^2 >= 1,
      fill = x^2 + y^2 >= 1,
      linetype = "Filtration",
      colour = "l2"
    ),
    fill = "7.5YR 5/10" %>% munsell::mnsl(),
    alpha = 0.4
  ) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0), limits = c(-5,5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-5,5)) +
  scale_linetype_manual(name = "Type", values = line_types) +
  scale_color_manual(name = "Statistic", values = colours, labels = labels)

  ggsave(
    paste0("theoretical-plot", ".pdf"),
    path = "C:\\Users\\yotam\\Desktop",
    width = 10, height = 8
  )

