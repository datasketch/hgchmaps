hgch_theme <- function(opts = NULL) {
  message("in theme_datasketch")
  highcharter::hc_theme(
    useHTML = TRUE,
    styledMode = TRUE,
    chart = list(
      backgroundColor = opts$background_color
    ),
    title = list(
      useHTML = TRUE,
      align = opts$title_align,
      style = list(
      fontSize = paste0(opts$title_size, 'px'),
      fontFamily = opts$title_family,
      color = opts$title_color,
      fontWeight = opts$title_weight
      )
    )
  )
}
