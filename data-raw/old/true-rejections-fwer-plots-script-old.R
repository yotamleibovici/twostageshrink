library(tidyverse)
library(twostageshrink)
library(ggpubr)

#setwd(r"(C:\Users\yotam\My Drive\Studies\MSc\Yair-Yotam-SuperEfficiency\twostageshrink)")

method_names <- c(
  "nofilt_maxp" = "No Filtration / Max p-value",
  "screenmin_maxp" = "ScreenMin / Max p-value",
  "l2norm_maxp" = "L2-norm / Max p-value",
  "nofilt_sobel" = "No Filtration / Sobel",
  "screenmin_sobel" = "ScreenMin / Sobel",
  "l2norm_sobel" = "L2 norm / Sobel"
)

true_rejections_plots <- twostageshrink::d3_evaluated %>%
  group_by(nobs, method, config) %>%
  summarise(mean_trej = mean(trej)) %>%
  ungroup() %>%
  ggplot2::ggplot(ggplot2::aes(
    x = nobs %>% forcats::as_factor(),
    y = mean_trej,
    colour = method %>% forcats::fct_relevel(
      "nofilt_maxp",
      "nofilt_sobel",
      "screenmin_maxp",
      "screenmin_sobel",
      "l2norm_maxp",
      "l2norm_sobel"
    )
  )) +
  ggplot2::scale_colour_discrete(labels = method_names %>% latex2exp::TeX()) +
  ggplot2::labs(
    x = "Number of Observations",
    y = "True Rejections",
    colour = "Method"
  ) +
  ggplot2::geom_point() +
  ggplot2::geom_line(aes(group = method)) +
  facet_wrap(vars(config), nrow = 3, labeller =
               as_labeller(function(string) paste("Configuration", string)))




fwer_plots <- twostageshrink::d4_measured %>%
  ggplot2::ggplot(ggplot2::aes(
    x = nobs %>% forcats::as_factor(),
    y = fwer,
    colour = method %>% forcats::fct_relevel(
      "nofilt_maxp",
      "nofilt_sobel",
      "screenmin_maxp",
      "screenmin_sobel",
      "l2norm_maxp",
      "l2norm_sobel"
    )
  )) +
  ggplot2::scale_colour_discrete(labels = method_names %>% latex2exp::TeX()) +
  ggplot2::labs(
    x = "Number of Observations",
    y = "FWER",
    colour = "Method"
  ) +
  ggplot2::geom_point() +
  ggplot2::geom_line(aes(group = method)) +
  ggplot2::facet_wrap(vars(config), nrow = 3, labeller =
               as_labeller(function(string) paste("Configuration", string)))




ggarrange(true_rejections_plots, fwer_plots, ncol = 2, common.legend = TRUE)

ggsave(
  paste0("pvals-all-new-3", ".pdf"),
  path = r"(C:\Users\yotam\OneDrive\Desktop)",
  width = 8, height = 6
)
