library(tidyverse)
library(twostageshrink)
library(ggpubr)

#setwd(r"(C:\Users\yotam\My Drive\Studies\MSc\Yair-Yotam-SuperEfficiency\twostageshrink)")

method_names <- c(
  "nofilt-maxp" = "No Filtration / Max p-value",
  "screenmin-maxp" = "ScreenMin / Max p-value",
  "l2norm-maxp" = "L2-norm / Max p-value",
  "nofilt-sobel" = "No Filtration / Sobel",
  "screenmin-sobel" = "ScreenMin / Sobel",
  "l2norm-sobel" = "L2 norm / Sobel"
)

true_rejections_plots <- twostageshrink::d3_evaluated %>%
  group_by(nobs, method, config) %>%
  summarise(mean_trej = mean(trej)) %>%
  ungroup() %>%
  ggplot2::ggplot(ggplot2::aes(
    x = nobs %>% forcats::as_factor(),
    y = mean_trej,
    colour = method %>% forcats::fct_relevel(
      "nofilt-maxp",
      "screenmin-maxp",
      "l2norm-maxp",
      "nofilt-sobel",
      "screenmin-sobel",
      "l2norm-sobel"
    )
  )) +
  ggplot2::scale_colour_discrete(labels = method_names %>% latex2exp::TeX()) +
  ggplot2::labs(
    x = "Number of Observations",
    y = "True Rejections",
    colour = "Method"
  ) +
  ggplot2::geom_point() +
  facet_wrap(vars(config), nrow = 3, labeller =
               as_labeller(function(string) paste("Configuration", string)))




fwer_plots <- twostageshrink::d4_measured %>%
  ggplot2::ggplot(ggplot2::aes(
    x = nobs %>% forcats::as_factor(),
    y = fwer,
    colour = method %>% forcats::fct_relevel(
      "nofilt-maxp",
      "screenmin-maxp",
      "l2norm-maxp",
      "nofilt-sobel",
      "screenmin-sobel",
      "l2norm-sobel"
    )
  )) +
  ggplot2::scale_colour_discrete(labels = method_names %>% latex2exp::TeX()) +
  ggplot2::labs(
    x = "Number of Observations",
    y = "FWER",
    colour = "Method"
  ) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  facet_wrap(vars(config), nrow = 3, labeller =
               as_labeller(function(string) paste("Configuration", string)))




ggarrange(true_rejections_plots, fwer_plots, ncol = 2, common.legend = TRUE)

ggsave(
  paste0("pvals-all-new-3", ".pdf"),
  path = r"(C:\Users\yotam\OneDrive\Desktop)",
  width = 8, height = 6
)
