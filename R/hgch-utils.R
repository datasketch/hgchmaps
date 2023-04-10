#' @import dplyr
#' @import highcharter

hc_titles <- function(hc, opts) {
  hc |>
    hc_title(text = opts$title) |>
    hc_subtitle(text = opts$subtitle)  |>
    hc_credits(enabled = opts$caption_show, text = opts$caption)
}

hc_choropleth <- function(hc, data, opts = NULL) {

  hc |>
    hc_add_series(
      mapData = data$map_data,
      data = data$data,
      joinBy = data$var_geo
    ) |>
    hc_colorAxis(
      stops = color_stops(colors = opts$palette_colors)
    ) |>
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}")))

}
