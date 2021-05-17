# function basic choropleth -----------------------------------------------------------------------------


test_that("hgch_basic_choropleth() works", {
  opts <- dsvizopts::dsviz_defaults()
  data <- sample_data("Gnm-Num")
  l <- hgchmaps_prep(data = data, opts = opts)
  out <- hgch_basic_choropleth(shape_json = l$geoInfo, data = l$data, by = l$by_col)
  expect_true(all(class(out) %in% c("highchart","htmlwidget")))
  data <- sample_data("Gcd-Num")
  l <- hgchmaps_prep(data = data, opts = opts, by_col = "id")
  out <- hgch_basic_choropleth(shape_json = l$geoInfo, data = l$data, by = l$by_col)
  expect_true(all(class(out) %in% c("highchart","htmlwidget")))
})
