gd_tj_transform <- function(tj) {
  tj <- tj |> sf::st_set_crs(3857)
  shape_transform <- sf::st_transform(tj,
                                      "+proj=mill +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +R_A +datum=WGS84 +units=m +no_defs")
  geojsonio::geojson_json(shape_transform)
}

hc_titles <- function (hc, opts) {
  hc |>
    hc_title(text = opts$title)
}

data_tooltip_opts <- function(opts = NULL) {

  if (is.na(opts$tooltip_template)) opts$tooltip_template <- NULL

  list(
    map_name = opts$map_name,
    tooltip_template = opts$tooltip_template,
    format_sample_cat = NULL, #opts$format_sample_cat,
    format_sample_num = opts$format_sample_num,
    format_sample_dat = NULL,#opts$format_sample_dat,
    na_label = opts$na_label,
    legend_cat_order = opts$legend_cat_order
  )

}

discret_color <- function(data, var, color) {
  l <- NULL
  if (class(data[[var]]) %in% c("character", "factor")) {
    data <- data[,c(var, "value", "color")]
    l <- data |>
      tidyr::drop_na() |>
      group_by(!!sym(var)) |>
      summarise(value = unique(value), color = unique(color)) |>
      arrange(value) |>
      rename(name = !!sym(var), from = value) |>
      mutate(to = from + 1) |>
      list_parse()
  }
  l
}

#' @export
hcgh_color <- function(hc, data, var, palette_colors) {
  data <- data |> as_tibble()
  data_classes <- discret_color(data, var, palette_colors)
  if (is.null(data_classes)) {
    hc |>
    hc_colorAxis(
        stops = color_stops(colors = palette_colors)
      )
  } else {
    hc |>
    hc_colorAxis(
      dataClassColor = "category",
      dataClasses = data_classes
    )
  }
}



#' @export
data_hc_map <- function(data, var, opts) {

  dgeo <- dsdataprep::data_map_draw(data = data,
                                    var = var,
                                    opts = data_tooltip_opts(opts))

  if ("..var" %in% names(dgeo)) dgeo$value <- dgeo$..var
  dgeo <- dgeo |> rename(label = ..labels)

  tj <- gd_tj_transform(dgeo)

  list(
    data = dgeo,
    tj = tj
  )
}


