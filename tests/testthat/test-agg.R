# function agg ------------------------------------------------------------------------------------------


test_that("function_agg() works without numeric column in data.frame", {

  df <- sample_data("Gcd", n = 300)
  names(df) <- "Countries"
  out <- function_agg(df, "sum", to_agg = NULL, Countries)
  expect_equal(names(out), c("Countries", "..count"))

  out <- function_agg(df, "sum", to_agg = NULL)
  expect_equal(out$..count, dim(df)[1])

})

test_that("function_agg() works with one or more numeric columns in data.frame", {

  df <- sample_data("Gcd-Num", n = 300)
  names(df) <- c("Countries", "Value")
  out <- function_agg(df, "sum", to_agg = "Value", Countries)
  expect_equal(names(out), c(names(df), "..count"))

  out <- function_agg(df, "sum", to_agg = "Value")
  expect_equal(out$Value, sum(df$Value, na.rm = T))


  df <- sample_data("Gcd-Num-Num", n = 300)
  names(df) <- c("Countries", "Value_x", "Value_y")
  out <- function_agg(df, "sum", to_agg = c("Value_x", "Value_y"), Countries)
  expect_equal(names(out), c(names(df), "..count"))

  out <- function_agg(df, "sum", to_agg = c("Value_x", "Value_y"))
  expect_equal(c(out$Value_x, out$Value_y), c(sum(df$Value_x, na.rm = T), sum(df$Value_y, na.rm = T)))

})
