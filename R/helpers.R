#' Sum, mean. mediad o count of initial data
#'
#' \code{function_agg} returns the operation of all the values present in its arguments.
#'
#' This is a generic function: methods can be defined for it directly or via the
#' \code{\link{across}} group generic. For this to work properly, the arguments
#' \code{...} should be unnamed, and dispatch is on the first argument.
#' @export
function_agg <- function (df, agg, to_agg, ...) {
  group_var <- rlang::enquos(...)

  if (is.null(to_agg)) {
    dd <- df %>%
      dplyr::group_by(!!!group_var) %>%
      dplyr::summarise(..count = dplyr::n())
  } else {
    dd <- df %>%
      dplyr::group_by(!!!group_var) %>%
      dplyr::summarise(dplyr::across(to_agg, ~ dsvizopts::agg(agg, .x)), ..count = dplyr::n())
  }
  dd

}

#' @export
simple_summary <- function(df, agg, to_agg, ...) {
  group_var <- rlang::enquos(...)
  dd <- df %>%
    dplyr::group_by(!!!group_var) %>%
    dplyr::summarise(dplyr::across(to_agg, ~ dsvizopts::agg(agg, .x)))
  dd
}

#' @export
percentage_data <- function (data, agg_var, by_col = NULL) {

  if (is.null(agg_var)) stop("You must have a numeric column")

  data[[agg_var]] <- as.numeric(data[[agg_var]])
  agg_var_t <- rlang::sym(agg_var)

  if (is.null(by_col)) {
    df <- data %>%
      mutate(..percentage = (!!agg_var_t/sum(!!agg_var_t, na.rm = TRUE))*100)
  } else {
    df <- data %>%
      dplyr::group_by_(by_col) %>%
      dplyr::mutate(..percentage = (!!agg_var_t/sum(!!agg_var_t, na.rm = TRUE))*100)
  }
  df
}


#' @export
collapse_data <- function (data, ...) {
  group_var <- rlang::enquos(...)
  print(group_var)
  func_paste <- function(x) paste(unique(x), collapse = '. ')
  df <- data %>%
    dplyr::group_by(!!!group_var) %>%
    dplyr::summarise_each(dplyr::funs(func_paste))
  df
}

#' @export
hgch_basic_choropleth <- function(shape_json, data, by) {
  highchart(type = "map") %>%
    hc_add_series(mapData =  shape_json,
                  data = data,
                  joinBy =  c(by, 'a'),
                  showInLegend = FALSE) %>%
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.labels;}")))
}


#' @export
hgch_basic_bubbles <- function(shape_json, data) {
  opts <- dsvizopts::dsviz_defaults()
  data <- l$data
  data <- data %>% rename(c("lat" = "b", "lon" = "a", "z" = "value"))
  data$name <- "aaaa"
  data <- data %>% select(lat, lon, val)
  #highchart() %>%
  #hcmap( map= 'countries/gb/gb-all') %>%
  getContent <- function(url) {
    library(httr)
    content(GET(url))
  }

  volcano <- getContent("http://cedeusdata.geosteiniger.cl/geoserver/wfs?srsName=EPSG%3A4326&typename=geonode%3Amundo_volcanes&outputFormat=json&version=1.0.0&service=WFS&request=GetFeature")

    highchart(type = "map") %>%
    hc_chart(backgroundColor = "#161C20") %>%
    hc_add_series(
      mapData = shape_json,
      showInLegend = FALSE,
      nullColor = "#424242",
      borderWidth = 0
    ) %>%
      hc_add_series(
        data = l,
        type = "mappoint",
        color = hex_to_rgba("#f1c40f", 0.4),
        geojson = TRUE,
        name = "Volcanos"#,
        #tooltip = list(pointFormat = "{point.properties.NOMBRE}"),
        #marker = list(lineWidth = 0, radius = 2)
      )


    # hc_tooltip(useHTML = TRUE,
    #            formatter = JS(paste0("function () {return this.point.labels;}")))
}

cities <- data.frame(
  lat = c(51.507222, 52.483056, 55.858, 53.4),
  lon = c(-0.1275, -1.893611, -4.259, -3),
  z = c(1, 2, 3, 2)
)

hcmap("custom/world", showInLegend = FALSE) %>%
  hc_add_series(
    data = data,
    type = "mapbubble",
    name = "Cities",
    minSize = "1%",
    maxSize = "5%"
  )

l <- list(
  type = "FeatureCollection",
  totalFeatures = nrow(cities),
  features = map(1:nrow(cities), function(r){
    list(
      type = "Feature",
      id = paste0("row_", r),
      geometry = list(
        type = "Point",
        coordinates = list(cities$lon[r], cities$lat[r])
      ),
      geometry_name = "the_geom",
      properties = list(
        NOMBRE ="Mesa Nevada deHerveo",
        ALTURA =5590,
        ESTADO = "solfatara")
    )
}),
crs = list(
  type = "EPSG",
  properties = list(code="4326"))
)


