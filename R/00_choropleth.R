#' @export
hgch_choropleth <- function (data = NULL,
                             dic = NULL,
                             var_gnm = NULL,
                             var_num = NULL, ...) {

  if (is.null(var_gnm)) stop("You must enter at least one geeographic variable")

  frType <- frtype_viz(var_gnm = var_gnm, var_num = var_num)
  opts <- plot_opts(viz = "choropleth", frType = frType, ...)

  data_draw <- data_draw(data = data,
                         dic = dic,
                         var_geo = var_gnm,
                         var_num = var_num,
                         opts = opts$data_opts)

  choropleth <- highchart(type = "map") |>
    hc_titles(opts = opts$titles) |>
  hc_choropleth(data = data_draw, opts = opts$general_opts) #|>

  #hc_add_theme(hgch_theme(opts = opts$theme))
  choropleth
}
