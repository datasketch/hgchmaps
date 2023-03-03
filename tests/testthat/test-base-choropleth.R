# function basic choropleth -----------------------------------------------------------------------------


test_that("hgch_basic_choropleth() works", {
  opts <- dsvizopts::dsviz_defaults()
  data <- data.frame(pais = c("Colombia", "Argentina", "Russia"), value = c(10, 20, 30))
  l <- hgchmaps_prep(data = data, opts = opts)
  out <- hgch_basic_choropleth(shape_json = l$geoInfo, data = l$data, by = l$by_col)
  expect_true(all(class(out) %in% c("highchart","htmlwidget")))
  data <- sample_data("Gcd-Num")
  l <- hgchmaps_prep(data = data, opts = opts, by_col = "id", ftype = "Gcd-Num")
  out <- hgch_basic_choropleth(shape_json = l$geoInfo, data = l$data, by = l$by_col)
  expect_true(all(class(out) %in% c("highchart","htmlwidget")))
})
