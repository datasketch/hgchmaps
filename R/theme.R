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
  print(opts)
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
      series = list(
        colorByPoint = TRUE,
        animation = list(
          duration = opts$animation_duration
        ),
        dataLabels = list (
          enabled = opts$dataLabels_show,
          format = paste0(opts$cats, opts$format_dataLabels)
        )
      ),
    map = list(
      minColor = "#FACFEA",
      maxColor = "#000000"
    )
  )
}
