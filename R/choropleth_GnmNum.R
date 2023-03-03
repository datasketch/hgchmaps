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

  highchart(type = "map") %>%
    hc_add_series(
      mapData = l$geoInfo,
      data = l$data,
      joinBy = l$by_col,
      borderColor = opts$theme$border_color,
      nullColor = opts$theme$na_color,
      showInLegend = FALSE,
      dataLabels = list (
        enabled = l$datalabel$data_labels_show,
        format = l$datalabel$data_labels_format_sample %||% "{point.value}",
        style = list(
          fontSize = paste0(l$datalabel$data_labels_size %||% 11, "px"),
          color = l$datalabel$data_labels_color %||% "#222222",
          textShadow = "none",
          textOutline = ifelse(l$datalabel$data_labels_text_outline,
                               "1px contrast", "none")
        )
      ),
      events = list(click = l$shiny$clickFunction),
      cursor= l$shiny$cursor
    ) %>%
    hc_colorAxis(
      stops = color_stops(colors = l$palette_colors)
    ) %>%
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.labels;}"))) %>%
    hc_add_theme(hgch_theme(opts = l$theme))


}



#' choropleth chart Gnm
#'
#' @description
#' `hgch_choropleth_Gnm()` Create a Highcharter choropleth map based on a particular data type.
#' In this case, you can load data with only one column, where it's a **geoname column**,
#' or make sure that the first column of your data meet this condition
#'
#' @export
#' @family Gnm plots
#' @section Ftype:
#' Gnm
#' @examples
#' data <- sample_data("Gnm", n = 30)
#' hgch_choropleth_Gnm(data)
#'
#'
#' # if you want to calculate the average instead of the sum, you can use agg inside a function
#' hgch_choropleth_Gnm(data,
#'                     agg = "mean")
#'
#'
hgch_choropleth_Gnm <- function(data = NULL, ...) {

  opts <- dsvizopts::merge_dsviz_options(...)
  if (!is.null(data)) data[[1]] <- as_Gnm(data[[1]])
  l <- hgchmaps_prep(data = data, opts = opts, ftype="Gnm")

  highchart(type = "map") %>%
    hc_add_series(
      mapData = l$geoInfo,
      data = l$data,
      joinBy = l$by_col,
      borderColor = opts$theme$border_color,
      nullColor = opts$theme$na_color,
      showInLegend = FALSE,
      dataLabels = list (
        enabled = l$datalabel$dataLabels_show,
        format = l$datalabel$dataLabels_format_sample %||% "{point.value}",
        style = list(
          fontSize = paste0(l$datalabel$dataLabels_size %||% 11, "px"),
          color = l$datalabel$dataLabels_color %||% "#222222",
          textShadow = "none",
          textOutline = ifelse(l$datalabel$dataLabels_text_outline,
                               "1px contrast", "none")
        )
      ),
      events = list(click = l$shiny$clickFunction),
      cursor= l$shiny$cursor
    ) %>%
    hc_colorAxis(
      stops = color_stops(colors = l$palette_colors)
    ) %>%
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.labels;}"))) %>%
    hc_add_theme(hgch_theme(opts = l$theme))


}

