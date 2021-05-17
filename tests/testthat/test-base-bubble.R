# function basic bubbles ------------------------------------------------------------------------------


test_that("hgch_basic_choropleth() works", {
  opts <- dsvizopts::dsviz_defaults()
  data <- sample_data("Gln-Glt-Num")
  l <- hgchmaps_prep(data = data, opts = opts, ftype = "Gln-Glt-Num")
  hgch_basic_bubbles(shape_json = l$geoInfo, data = l$data)
})
