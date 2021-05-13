# function format_prep ----------------------------------------------------------------------------------


test_that("format_prep() works changing style of categories", {

  data <- sample_data("Gnm")
  f <- homodatum::fringe(data)
  dic <- f$dic
  names(data) <- dic$id
  out <- format_prep(data, dic, formats = list(sample_cat = "SAMPLE"))
  expect_equal(names(out), c(names(data), paste0(names(data), "_label")))
  expect_identical(out$quis_gnm_label, as.character(toupper(data$quis_gnm)))
})
