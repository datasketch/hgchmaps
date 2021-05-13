# function tooltip -------------------------------------------------------------------------------------


test_that("hgch_tooltip() works", {

  data <- sample_data("Gcd-Num")
  names (data) <- c("Countries", "Value")
  f <- homodatum::fringe(data)
  nms <- homodatum::fringe_labels(f)
  out <- hgch_tooltip(nms = nms, label_ftype = names(data), tooltip = "Country: {Countries}")
  expect_equal(out, "Country: {a_label}")
  expect_error(out <- hgch_tooltip(nms = nms, label_ftype = names(data) , tooltip = "Country: {Countries} {missing}"))
  out <- hgch_tooltip(nms = nms, label_ftype = names(data), tooltip = "Country: {Countries} <br/> {Value}")
  expect_equal(out, "Country: {a_label} <br/> {b_label}")
})
