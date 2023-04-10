#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr select mutate summarise
#' @importFrom dstools %||%
## usethis namespace: end
NULL


#' @keywords internal
frtype_viz <- function(var_gnm = NULL,
                       var_gcd = NULL,
                       var_gln = NULL,
                       var_glt = NULL,
                       var_num = NULL) {

  frtype <- NULL
  if (!is.null(var_gnm)) frtype <- paste0(rep("Gnm", length(var_gnm)), collapse = "")
  if (!is.null(var_gcd)) frtype <- paste0(frtype, paste0(rep("Gcd", length(var_gcd)), collapse = ""), collapse = "")
  if (!is.null(var_gln)) frtype <- paste0(frtype, paste0(rep("Gln", length(var_gln)), collapse = ""), collapse = "")
  if (!is.null(var_glt)) frtype <- paste0(frtype, paste0(rep("Glt", length(var_glt)), collapse = ""), collapse = "")
  if (!is.null(var_num)) frtype <- paste0(frtype, paste0(rep("Num", length(var_num)), collapse = ""), collapse = "")

  frtype
}

data_draw <- function(data = NULL,
                      dic = NULL,
                      var_geo = NULL,
                      var_num = NULL,
                      opts = NULL) {


  map_name <- opts$map_name

  if(is.null(map_name))
    stop("No map name provided, see available_maps()")

  tj <- geodato::gd_tj(map_name)


  if(!is.null(data)){
    if (length(var_geo) == 1) {
      col <- geodato::parse_col(data, var_geo)
      data$..var <- data[[col]]
    }

    if (!"value" %in% names(data)) {
      data$value <- data[[var_num]]
    }
    data <- geodato::gd_match(data, map_name)

    if (!"..labels" %in% names(data)) {
      data$label <- dsdataprep::prep_tooltip(data = data,
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

  list(
    var_geo = var_geo,
    data = data,
    map_data = shape_json
  )


}
