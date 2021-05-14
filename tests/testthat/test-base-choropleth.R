# function basic choropleth -----------------------------------------------------------------------------


test_that("hgch_basic_choropleth() works", {
  opts <- dsvizopts::dsviz_defaults()
  data <- sample_data("Gnm-Num")
  l <- hgchmaps_prep(data = data, opts = opts)
  hgch_basic_choropleth(shape_json = l$geoInfo, data = l$data)
})
