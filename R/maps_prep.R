#' @export
hgchmaps_prep <- function(data = NULL, opts = NULL, by_col = "name", ftype="Gnm-Num", ...) {

  # call geopgraphical info
  shape <- shape_info(map_name = opts$extra$map_name, ftype = ftype, by_col = by_col)
  # data preparation by type
  list_d <- data_prep(data = data,
                      ftype = ftype,
                      agg =  opts$summarize$agg,
                      ptage_col = opts$postprocess$percentage_col)
  # format setting of data being displayed
  data_format <- format_prep(data = list_d$data,
                             dic = list_d$dic,
                             formats = list(sample_num = opts$style$format_sample_num,
                                            sample_cat = opts$style$format_sample_cat))


  # coordinate transformation
  shape_transform <- st_transform(shape ,
                                  "+proj=mill +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +R_A +datum=WGS84 +units=m +no_defs")
  shape_json <- geojson_json(shape_transform)



  l <- list (
    geoInfo <- shape_json,
    data <- list_d$data
  )

}
