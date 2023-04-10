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

  data <- geodato::gd_match(data, map_name)
  tj <- geodato::gd_tj(map_name)


  if(!is.null(data)){
    if (!"..labels" %in% names(data)) {
      data$..labels <- dsdataprep::prep_tooltip(data = data,
                                                tooltip = opts$tooltip_template,
                                                new_labels = NULL,
                                                engine = "html",
                                                as_df = FALSE,
                                                na_row_default_column = NULL,
                                                na_row_default_value = NULL,
                                                na_label = opts$na_label,
                                                format_num = opts$format_sample_num,
                                                opts_format_num = list(prefix = opts$prefix_num,
                                                                       suffix = opts$suffix_num,
                                                                       si_prefix = opts$si_prefix),
                                                format_cat = opts$format_sample_cat,
                                                format_date = opts$format_sample_dat)
    }
    dgeo <- tj |>
      dplyr::left_join(data, by = c(id = "..gd_id", name = "..gd_name"))

  } else{
    dgeo <- tj
  }

  dgeo <- dgeo %>% sf::st_set_crs(3857)
  shape_transform <- sf::st_transform(dgeo,
                                      "+proj=mill +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +R_A +datum=WGS84 +units=m +no_defs")
  shape_json <- geojsonio::geojson_json(shape_transform)
  shape_json


}
