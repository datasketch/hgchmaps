#' @export
hgch_choropleth <- function (data = NULL,
                             dic = NULL,
                             var_gnm = NULL,
                             var_num = NULL, ...) {

  if (!is.null(data)) {
    frType <- frtype_viz(var_gnm = var_gnm, var_num = var_num)
  }
  opts <- plot_opts(viz = "choropleth", frType = frType, ...)

  data_draw <- data_map_draw(data = data,
                             dic = dic,
                             var_geo = var_gnm,
                             var_num = var_num,
                             opts = opts$data_opts)

  choropleth <- highchart(type = "map") |>
    hc_titles(opts = opts$titles) |>
    hc_choropleth(data = data_draw, opts = opts$general_opts) |>
    hc_add_theme(hgch_theme(opts = opts$theme))

  choropleth
}

#' @export
hgch_choropleth_GnmNum <- function(data, ...) {
  var_gnm <- names(data)[1]
  var_num <- names(data)[2]
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% var_num

  if (!is.null(data)) {
    data <- dsdataprep::aggregation_data(data = data,
                                         agg = opts_prep$agg,
                                         agg_name = var_num_name,
                                         group_var = var_gnm,
                                         to_agg = var_num,
                                         percentage = opts_prep$percentage,
                                         percentage_name = opts_prep$percentage_name,
                                         extra_col = opts_prep$extra_col,
                                         agg_extra = opts_prep$agg_extra)
  }
  hgch_choropleth(data = data, var_gnm = var_gnm, var_num = var_num_name, ...)
}


