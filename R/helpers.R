
#' @export
hgch_basic_choropleth <- function(shape_json, data, by) {

  if (is.null(data)) {
    h <-   highchart(type = "map") %>%
      hc_add_series(
        mapData = shape_json,
        showInLegend = FALSE
      )
  } else {
    h <- highchart(type = "map") %>%
      hc_add_series(mapData =  shape_json,
                    data = data,
                    joinBy =  c(by, 'a'),
                    showInLegend = FALSE) %>%
      hc_tooltip(useHTML = TRUE,
                 formatter = JS(paste0("function () {return this.point.labels;}")))
  }
  h
}

data_to_json <- function(data) {
  l <- map(1:nrow(data), function(r) {
    list(
      "labels"= data$labels[r],
      "lat"= data$b[r],
      "lon"= data$a[r],
      "z"= data$value[r]
    )
  })
  l
}

#' @export
hgch_basic_points <- function(shape_json, data) {

  h <-  highchart(type = "map") %>%
    hc_add_series(
      mapData = shape_json,
      showInLegend = FALSE
    )

  if (is.null(data)) {
    h <- h
  } else {

    data <- data_to_json(data)
    data <- geojson_list(data, lat = "lat", lon = "lon")

    h <- h %>%
      hc_add_series(
        data = data,
        type = 'mappoint',
        showInLegend = FALSE,
        tooltip = list(pointFormat = "{point.properties.labels}",
                       headerFormat = NULL),
        geojson = TRUE
      )
  }
  h
}

agg_tooltip <- function(data, label_by, nms, label_ftype, tooltip) {
  print(label_by)
  if (is.null(data)) stop("There is not a data")
  data_format <- data %>%
    dplyr::mutate(labels = ifelse(is.na(a),
                                  glue::glue(paste0("{",label_by, "}")) %>% lapply(htmltools::HTML),
                                  glue::glue(
                                    dsvizprep::tooltip_map(nms = nms,
                                                           label_ftype = label_ftype,
                                                           tooltip = tooltip)) %>%
                                    lapply(htmltools::HTML))
    )
  data_format
}
