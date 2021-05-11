#' @export
hgchmaps_prep <- function(data = NULL, opts = NULL, by_col = "name", ftype="Gnm-Num", ...) {

  map_name <- opts$extra$map_name
  label_by <- opts$extra$map_label_by


  shape <- shape_info(map_name = map_name, ftype = ftype, by_col = by_col)
  shape_transform <- st_transform(shape,
                                  "+proj=mill +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +R_A +datum=WGS84 +units=m +no_defs")
  shape_json <- geojson_json(shape_transform)


  # highchart(type = "map") %>%
  #   hc_add_series(mapData =  shape_json)
  list_d <- data_prep(data = data,
                      ftype = ftype,
                      agg =  opts$summarize$agg,
                      ptage_col = opts$postprocess$percentage_col)

}
