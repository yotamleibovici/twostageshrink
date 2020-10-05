#' Evaluate a testing procedure
#'
#' @param data A tibble, consists the \code{logical} columns \code{altr},
#'   indicating whether the hypothesis is a true alternative, and
#'   \code{final_result}, indicating whether the null hypothesis rejected
#'   (considered as an alternative).
#' @inheritParams perform
#'
#' @return A tibble consists two columns, \code{frej} and \code{trej}, which are
#'   the number of the false (true) rejections in each group.
#' @export
#'
#' @examples
evaluate <- function(data, ...) {
  group <- rlang::enexprs(...)

  data %>%
    dplyr::group_by(!!!group) %>%
    dplyr::summarise(
      frej = sum(!altr & final_result),
      trej = sum( altr & final_result)
    )
}
