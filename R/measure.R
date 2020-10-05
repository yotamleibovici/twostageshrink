#' Measure a testing procedure by the FWER score
#'
#' @param data A tibble, consists a \code{numeric} column, \code{frej}, which is
#'   the number of the false rejections in each group.
#' @inheritParams perform
#'
#' @return A tibble, consists a \code{numeric} column, \code{fwer}, which is
#'   the empirical probability of rejecting a true null.
#' @export
#'
#' @examples
measure <- function(data, ...) {
  group <- rlang::enexprs(...)

  data %>%
    dplyr::group_by(!!!group) %>%
    dplyr::summarise(
      fwer = mean(frej >= 1)
    )
}
