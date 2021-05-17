# function basic points -----------------------------------------------------------------------------


test_that("hgch_basic_points() works", {
  opts <- dsvizopts::dsviz_defaults()
  data <- sample_data("Gln-Glt")
  l <- hgchmaps_prep(data = data, opts = opts, ftype =  "Gln-Glt")
  out <- hgch_basic_points(shape_json = l$geoInfo, data = l$data)
  expect_true(all(class(out) %in% c("highchart","htmlwidget")))
  data <- sample_data("Gln-Glt-Num")
  l <- hgchmaps_prep(data = data, opts = opts, by_col = "id", ftype = "Gln-Glt-Num")
  out <- hgch_basic_points(shape_json = l$geoInfo, data = l$data)
  expect_true(all(class(out) %in% c("highchart","htmlwidget")))
})
