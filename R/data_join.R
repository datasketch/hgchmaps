#' @export
shape_info <- function (map_name, ftype, by_col) {

  if (is.null(map_name)) stop("You must type a map name")
  if (!map_name %in% availableGeodata()) stop("You map name isn't available, view availableGeodata()")

  geoInfo <- geodata::geoinfo(mapName = map_name)
  centroides <- geoInfo$centroids
  nms_centroides <- names(centroides)
  aditional_name <- setdiff(nms_centroides, c("id", "name", "lat", "lon"))
  centroides_join <- centroides[c("id", "lat", "lon")]
  topoInfo <- geoInfo$geo_sf


  topoInfo <- topoInfo %>%
    dplyr::left_join(centroides_join, by =  "id") %>%
    mutate(id = as.character(id))

  topoInfo_names <- names(topoInfo)
  topoInfo_names


  if (grepl("Gnm", ftype) & !identical(aditional_name, character())) {
    topoInfo$name_alt <- paste0(topoInfo$name, " - ", topoInfo[[aditional_name]])
  } else {
    topoInfo$name_alt <- as.character(topoInfo[[by_col]])
  }

  topoInfo$name_alt <- iconv(tolower(topoInfo$name_alt), to = "ASCII//TRANSLIT")
  topoInfo <- topoInfo %>% st_set_crs(3857)

  topoInfo
}
