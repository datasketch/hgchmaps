#' Sum, mean. mediad o count of initial data
#'
#' \code{function_agg} returns the operation of all the values present in its arguments.
#'
#' This is a generic function: methods can be defined for it directly or via the
#' \code{\link{across}} group generic. For this to work properly, the arguments
#' \code{...} should be unnamed, and dispatch is on the first argument.
#' @export
function_agg <- function (df, agg, to_agg, ...) {
  group_var <- rlang::enquos(...)

  if (is.null(to_agg)) {
    dd <- df %>%
      dplyr::group_by(!!!group_var) %>%
      dplyr::summarise(..count = dplyr::n())
  } else {
    dd <- df %>%
      dplyr::group_by(!!!group_var) %>%
      dplyr::summarise(dplyr::across(to_agg, ~ dsvizopts::agg(agg, .x)), ..count = dplyr::n())
  }
  dd

}
