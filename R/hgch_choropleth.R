#' @export
hgch_choropleth <- function(data, var, ...) {

  opts <-  dsopts::dsopts_merge(...)
  titles <- list(title = opts$title)
  theme <- list(background_color = opts$background_color,
                title_align = opts$title_align,
                title_size = opts$title_size,
                title_family = opts$title_family,
                title_color = opts$title_color,
                title_weight = opts$title_weight)
  opts$color_palette_sequential <- trimws(
    strsplit(opts$color_palette_sequential, split = ",")
    |> unlist())
  hc_data <- data_hc_map(data, var, opts)

  highchart(type = "map") |>
    hc_titles(opts = titles) |>
    hc_add_series(
      mapData = hc_data$tj,
      data = hc_data$data,
      joinBy = "name"
    ) |>
    hcgh_color(data = hc_data$data,
               var = var,
               palette_colors = opts$color_palette_sequential) |>
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}"))) |>
    hc_add_theme(hgch_theme(opts = theme))

}
