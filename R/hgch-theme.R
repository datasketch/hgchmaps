#' @keywords internal
hgch_theme <- function(opts = NULL) {
  message("in theme_datasketch")

  highcharter::hc_theme(
    useHTML = TRUE,
    styledMode = TRUE,
    chart = list(
      backgroundColor = opts$background_color,
      marginBottom = opts$plot_margin_bottom,
      marginLeft = opts$plot_margin_left,
      marginRight = opts$plot_margin_right,
      marginTop = opts$plot_margin_top,
      plotBackgroundColor = opts$plot_background_color,
      borderColor = opts$plot_border_color,
      borderWidth = opts$plot_border_size,
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
    subtitle = list(
      useHTML = TRUE,
      align = opts$subtitle_align,
      style = list(
        fontFamily = opts$title_family,
        fontSize = paste0(opts$subtitle_size, 'px'),
        color = opts$subtitle_color,
        fontWeight = opts$subtitle_weight
      )
    ),
    credits = list(
      useHTML = TRUE,
      href = opts$caption_link,
      margin = opts$caption_margin,
      position = list(
        align = opts$caption_align,
        x = ifelse(opts$caption_align == "right",-20, 20),
        y = opts$y_credits
      ),
      style = list(
        fontFamily = opts$title_family,
        fontSize = paste0(opts$caption_size, 'px'),
        color = opts$caption_color
      )
    ),
    plotOptions = list (
      series = list(
        connectNulls = opts$connect_lines_nulls,
        colorByPoint = opts$color_by_point,
        animation = list(
          duration = opts$animation_duration
        ),
        cursor =  opts$cursor,
        events = list(
          click = JS(opts$click_function)
        ),
        marker = list(
          enabled = opts$marker_enabled,
          symbol = "circle",
          radius = opts$marker_radius
        )
      )
    ),
    legend = list(
      backgroundColor = opts$legend_background,
      borderColor = opts$legend_backgroundBorderColor,
      borderWidth = opts$legend_backgroundWidth,
      maxHeight = opts$legend_maxHeight,
      title = list(
        text = opts$legend_title),
      layout = opts$legend_layout,
      align = opts$legend_align,
      verticalAlign = opts$legend_verticalAlign,
      itemMarginTop = opts$legend_itemMarginTop,
      itemMarginBottom = opts$legend_itemMarginBottom,
      reversed = opts$legend_reversed,
      itemStyle = list(
        fontFamily = opts$legend_family,
        fontSize = paste0(opts$legend_size %||% opts$text_size, 'px'),
        color = opts$legend_color %||% opts$text_color
      )
    ),
    tooltip = list(
      useHTML = TRUE,
      style = list(
        width = opts$tooltip_width %||% '350px',
        whiteSpace = 'normal',
        fontFamily = opts$tooltip_family %||% opts$text_family,
        fontSize = paste0(opts$theme$tooltip_size, 'px')
      )
    )
  )
}
