#' Default theme
#'
#' @param opts A list with parameters to modify the theme.
#'
#' @examples
#'
#' highcharts_demo() %>%
#'   hc_add_theme(hc_theme_alone())
#'
#' @export
#' @export
hgch_theme <- function(opts = NULL){
  message("in theme_datasketch")

  highcharter::hc_theme(
    colors = opts$palette_colors_sequential,

    chart = list(
      reflow = TRUE,
      renderTo = 'container',
      backgroundColor = opts$background_color,

      marginBottom = opts$plot_margin_bottom,
      marginLeft = opts$plot_margin_left,
      marginRight = opts$plot_margin_right,
      marginTop = opts$plot_margin_top,

      plotBackgroundColor = opts$plot_background_color,
      plotBorderColor = opts$plot_border_color,
      plotBorderWidth = opts$plot_border_width,
      style = list (
        fontFamily = opts$text_family,
        fontSize = paste0(opts$text_size, 'px')
      )),
    title = list(
      useHTML = TRUE,
      align = opts$title_align,
      style = list(
        fontFamily = opts$title_family,
        fontSize = paste0(opts$title_size, 'px'),
        color = opts$title_color,
        fontWeight = opts$title_weight
      )
    ),
    series = list(
      colorByPoint = TRUE,
      animation = list(
        duration = opts$animation_duration
      )
    ),
    legend = list(enabled = opts$legend_show),
    tooltip = list(style = list(width = "350px", whiteSpace = "normal",
                                fontFamily = opts$tooltip_family %||% opts$text_family,
                                fontSize = paste0(opts$text_size, "px")))
  )
}
