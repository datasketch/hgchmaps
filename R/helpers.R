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

#' @export
simple_summary <- function(df, agg, to_agg, ...) {
  group_var <- rlang::enquos(...)
  dd <- df %>%
    dplyr::group_by(!!!group_var) %>%
    dplyr::summarise(dplyr::across(to_agg, ~ dsvizopts::agg(agg, .x)))
  dd
}

#' @export
percentage_data <- function (data, agg_var, by_col = NULL) {

  if (is.null(agg_var)) stop("You must have a numeric column")

  data[[agg_var]] <- as.numeric(data[[agg_var]])
  agg_var_t <- rlang::sym(agg_var)

  if (is.null(by_col)) {
    df <- data %>%
      mutate(..percentage = (!!agg_var_t/sum(!!agg_var_t, na.rm = TRUE))*100)
  } else {
    df <- data %>%
      dplyr::group_by_(by_col) %>%
      dplyr::mutate(..percentage = (!!agg_var_t/sum(!!agg_var_t, na.rm = TRUE))*100)
  }
  df
}


#' @export
collapse_data <- function (data, ...) {
  group_var <- rlang::enquos(...)
  print(group_var)
  func_paste <- function(x) paste(unique(x), collapse = '. ')
  df <- data %>%
    dplyr::group_by(!!!group_var) %>%
    dplyr::summarise_each(dplyr::funs(func_paste))
  df
}

#' @export
hgch_basic_choropleth <- function(shape_json, data) {
  opts <- dsvizopts::dsviz_defaults()

  highchart(type = "map") %>%
    hc_add_series(mapData =  shape_json,
                  data = data,
                  joinBy =  c('name', 'a'),
                  showInLegend = FALSE) %>%
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.labels;}")))
}

