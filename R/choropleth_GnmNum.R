#' choropleth chart Gnm Num
#'
#' @description
#' `hgch_choropleth_GnmNum()` Create a Highcharter choropleth map based on a particular data type.
#' In this case, you can load data with only two columns, where the firts it's a **geoname column**,
#' and the second is a **numeric class column**, or make sure that the first two columns of
#' your data meet this condition
#'
#' @export
#' @family Gnm-Num plots
#' @section Ftype:
#' Gnm-Num
#' @examples
#' data <- sample_data("Gnm-Num", n = 30)
#' hgch_choropleth_GnmNum(data)
#'
#'
#' # if you want to calculate the average instead of the sum, you can use agg inside a function
#' hgch_choropleth_GnmNum(data,
#'                        agg = "mean")
#'
#'
hgch_choropleth_GnmNum <- function(data = NULL, ...) {

  opts <- dsvizopts::merge_dsviz_options(...)
  if (!is.null(data)) data[[1]] <- as_Gnm(data[[1]])
  l <- hgchmaps_prep(data = data, opts = opts, ftype="Gnm-Num")

}
