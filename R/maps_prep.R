#' @export
hgchmaps_prep <- function(data = NULL, opts = NULL, by_col = "name", ftype="Gnm-Num", ...) {

  map_name <- opts$extra$map_name
  label_by <- opts$extra$map_label_by

  geoInfo <- geodata::geoinfo(mapName = map_name)
  centroides <- geoInfo$centroids
  nms_centroides <- names(centroides)
  aditional_name <- setdiff(nms_centroides, c("id", "name", "lat", "lon"))
  centroides_join <- centroides[c("id", "lat", "lon")]
  topoInfo <- geoInfo$geo_sf

  shape <- topoInfo %>% st_set_crs(4326)
  shape_transform <- st_transform(shape,
                                  "+proj=lcc +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
  shape_json <- geojson_json(shape_transform)



}
