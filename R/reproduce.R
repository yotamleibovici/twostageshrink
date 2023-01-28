method_names <- setNames(
  c(
    c("Without filtration", "$L^2$-norm", "Screen-min"),
    paste0("Product, $\\delta = ", c(0.8, 0.9, 1), "$")
  ),
  c(
    "nofilt", "l2norm", "screenmin",
    "product08", "product09", "product1"
  )
)


#' Reproduce the figures as they appear in the thesis.
#'
#' @param configid serial number of configuration, one of {1, 2, 3}.
#' @param output the desired plot, either "trej" for number of true rejections,
#'   or "fwer" for the FWER.
#'
#' @return the plot of the \code{output}, under the \code{configid}-th
#'   configuration
#' @export
#'
#' @examples
#' reproduce(1, "trej")
#' reproduce(2, "fwer")
#'
#' @importFrom magrittr `%>%`
#' @importFrom magrittr `%$%`
reproduce <- function(configid, output = c("trej", "fwer")) {
  assertthat::assert_that(
    assertthat::is.count(configid),
    configid >= 1 && configid <= 3,
    output == "trej" || output == "fwer"
  )

  switch (output,
    "trej" = reproduce_trej(data_evaluated = d3_evaluated, configid = configid),
    "fwer" = reproduce_fwer(data_measured = d4_measured, configid = configid)
  )
}


reproduce_trej <- function(data_evaluated, configid) {
  data_evaluated %>%
    dplyr::filter(config == configid) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = nobs %>% forcats::as_factor(),
      y = trej,
      fill = method %>% forcats::fct_reorder2(nobs, trej)
    )) +
    ggplot2::scale_fill_discrete(labels = method_names %>% latex2exp::TeX()) +
    ggplot2::labs(
      x = "Number of Observations",
      y = "True Rejections",
      fill = "Method"
    ) +
    ggplot2::geom_boxplot()
}


reproduce_fwer <- function(data_measured, configid) {
  data_measured %>%
    dplyr::filter(config == configid) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = nobs %>% forcats::as_factor(),
      y = fwer,
      colour = method %>% forcats::fct_reorder2(nobs, fwer)
    )) +
    ggplot2::scale_colour_discrete(labels = method_names %>% latex2exp::TeX()) +
    ggplot2::labs(
      x = "Number of Observations",
      y = "FWER",
      colour = "Method"
    ) +
    ggplot2::geom_point()
}




