library(tidyverse)
library(twostageshrink)
library(ggpubr)

#setwd("C:\\Users\\OMRI\\Dropbox\\Studies\\MSc\\Yair-Yotam-SuperEfficiency\\msc-thesis-paper\\general")

method_names <- c(
  "nofilt-maxp" = "No Filtration / Max p-value",
  "screenmin-maxp" = "ScreenMin / Max p-value",
  "l2norm-maxp" = "L2-norm / Max p-value",
  "nofilt-sobel" = "No Filtration / Sobel",
  "screenmin-sobel" = "ScreenMin / Sobel",
  "l2norm-sobel" = "L2 norm / Sobel",
  "dact" = "DACT"
)

true_rejections_plots <- twostageshrink::d3_evaluated %>%
  # dplyr::filter(config == configid) %>%
  ggplot2::ggplot(ggplot2::aes(
    x = nobs %>% forcats::as_factor(),
    y = trej,
    fill = method %>% forcats::fct_relevel(
      "nofilt-maxp",
      "screenmin-maxp",
      "l2norm-maxp",
      "nofilt-sobel",
      "screenmin-sobel",
      "l2norm-sobel",
      "dact"
    )
  )) +
  ggplot2::scale_fill_discrete(labels = method_names %>% latex2exp::TeX()) +
  ggplot2::labs(
    x = "Number of Observations",
    y = "True Rejections",
    fill = "Method"
  ) +
  ggplot2::geom_boxplot() +
  facet_wrap(vars(config), nrow = 3, labeller =
               as_labeller(function(string) paste("Configuration", string)))




fwer_plots <- twostageshrink::d4_measured %>%
  dplyr::filter(method != "dact") %>%
  ggplot2::ggplot(ggplot2::aes(
    x = nobs %>% forcats::as_factor(),
    y = fwer,
    colour = method
  )) +
  ggplot2::scale_colour_discrete(labels = method_names %>% latex2exp::TeX()) +
  ggplot2::labs(
    x = "Number of Observations",
    y = "FWER",
    colour = "Method"
  ) +
  ggplot2::geom_point() +
  facet_wrap(vars(config), nrow = 3, labeller =
               as_labeller(function(string) paste("Configuration", string)))




ggarrange(true_rejections_plots, fwer_plots, ncol = 2, common.legend = TRUE)

ggsave(
  paste0("pvals-all-new", ".pdf"),
  path = "C:\\Users\\yotam\\Desktop",
  width = 8, height = 6
)
