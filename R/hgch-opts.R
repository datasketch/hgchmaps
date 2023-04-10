#' Prepare data for visualization
#'
#' This function prepares data for visualization by performing operations such as aggregation and data formatting.
#' @param ... additional arguments to be passed to \code{\link[dsvizopts]{dsviz_default_opts}}
#'
#' @return list with options to change properties of plot
#' @import dsvizopts
#'
#' @keywords internal
plot_opts <- function(viz = NULL, frType = NULL, ...) {
  if (is.null(viz)) return()
  opts <- dsvizopts::merge_dsviz_options(...)
  plot_type <- viz
  extra_opts <- list()

  if (opts$shiny$shiny_clickable) {
    input_name <- opts$shiny$shiny_input_name
    if (!is.null(frType)) {
      opts$theme$click_function <- click_functions(viz = viz,
                                                   frtype = frType,
                                                   id_click = input_name)
    }
    opts$theme$cursor <- opts$shiny$shiny_cursor
  }

  titles <- list(
    title = opts$titles$title,
    subtitle = opts$titles$subtitle,
    caption = opts$titles$caption,
    caption_show = !is.null(opts$titles$caption)
  )

  data_opts <- list(
    map_name = opts$map$map_name,
    tooltip_template = opts$chart$tooltip_template,
    na_label = opts$prep$na_label,
    format_sample_num = opts$prep$format_sample_num,
    prefix_num = opts$prep$prefix_num,
    suffix_num = opts$prep$suffix_num,
    si_prefix = opts$prep$si_prefix %||% FALSE,
    format_sample_cat = opts$prep$format_sample_cat,
    format_sample_dat = opts$prep$format_sample_dat,
    color_by = opts$prep$color_by,
    palette_type = NULL,
    palette = NULL,
    order = opts$prep$order,
    order_legend = opts$prep$order_legend,
    label_wrap = opts$prep$label_wrap,
    new_line = "<br/>",
    sort = opts$prep$sort,
    slice_n = opts$prep$slice_n
  )

  general_opts <- list(
    plot_type = plot_type,
    legend_show = opts$theme$legend_show,
    legend_title = opts$titles$legend_title,
    palette_colors = opts$theme$palette_colors %||% opts$theme$palette_colors_sequential
  )

  general_opts <- modifyList(general_opts, extra_opts)

  list(titles = titles,
       data_opts = data_opts,
       general_opts = general_opts,
       theme = opts$theme)

}



