data_draw <- function(data,
                      dic = NULL,
                      map_name,
                      var_geo,
                      var_num = NULL,
                      opts = NULL) {

  if(is.null(map_name))
    stop("No map name provided, see available_maps()")

  col <- geodato::parse_col(data, var)
  data$..var <- data[[col]]

  d <- geodato::gd_match(data, map_name)
  tj <- geodato::gd_tj(map_name)


  if(!is.null(data)){
    dgeo <- tj |>
      dplyr::left_join(d, by = c(id = "..gd_id", name = "..gd_name"))
  } else{
    dgeo <- d
  }

  dgeo <- dgeo %>% sf::st_set_crs(3857)
  shape_transform <- sf::st_transform(dgeo,
                                      "+proj=mill +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +R_A +datum=WGS84 +units=m +no_defs")
  shape_json <- geojsonio::geojson_json(shape_transform)


}
