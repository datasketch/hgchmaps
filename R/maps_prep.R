#' @export
hgchmaps_prep <- function(data = NULL, opts = NULL, by_col = "name", ftype="Gnm-Num", ...) {


  # call geographical info
  shape <- dsvizprep::shape_info(map_name = opts$map$map_name,
                                 ftype = ftype,
                                 by_col = by_col,
                                 addRds = FALSE)
  #topoData <- shape$rdsInfo
  topoInfo <- shape$topoInfo
  topoInfo$labels <- topoInfo[[by_col]]

  # data preparation by type
  data_format <- NULL
  if (!is.null(data)) {
    list_d <- dsvizprep::data_map_prep(data = data,
                                       ftype = ftype,
                                       agg =  opts$prep$agg,
                                       color_by = opts$style$color_by,
                                       more_levels = shape$more_levels,
                                       ptage_col = opts$prep$percentage_col)
    # format setting of data being displayed
    data_format <- dsvizprep::format_prep(data = list_d$data,
                                          dic = list_d$dic,
                                          formats = list(sample_num = opts$prep$format_sample_num,
                                                         sample_cat = opts$prep$format_sample_cat,
                                                         prefix = opts$prep$prefix_num,
                                                         suffix = opts$prep$suffix_num))


    if (grepl("Gnm|Gcd", ftype)) {
      data_format$name_alt <- iconv(tolower(data_format$a), to = "ASCII//TRANSLIT")
      all_data <- topoInfo %>% as.data.frame() %>% dplyr::select(id, name, name_alt, name_label)
      data_format <- all_data %>% dplyr::left_join(data_format, by = "name_alt")
      # add info tooltip in data
      data_format$labels <- dsdataprep::prep_tooltip(data_format, tooltip = opts$chart$tooltip_template)
    } else {
      topoInfo <- list(topoInfo = topoInfo, data = data_format)
      # add info tooltip in data
      topoInfo$data$labels <- dsdataprep::prep_tooltip(topoInfo$data, tooltip = opts$chart$tooltip_template)
    }


  }

  # coordinate transformation
  topoInfo <- topoInfo %>% sf::st_set_crs(3857)
  shape_transform <- sf::st_transform(topoInfo,
                                  "+proj=mill +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +R_A +datum=WGS84 +units=m +no_defs")
  shape_json <- geojsonio::geojson_json(shape_transform)

  palette_type <-  opts$theme$palette_type %||% "sequential"
  palette_colors <-  opts$theme$palette_colors %||% opts$theme[[paste0("palette_colors_", palette_type)]]

  l <- list (
    geoInfo = shape_json,
    data = data_format,
    by_col = by_col,
    palette_colors = palette_colors,
    theme = opts$theme,
    datalabel = opts$data_labels,
    shiny = opts$shiny
  )

}
