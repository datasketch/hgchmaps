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


test_that("function_agg() works with one or more categorical columns in data.frame", {

  df <- sample_data("Gcd-Cat-Num", n = 300)
  names(df) <- c("Countries", "Catvar", "Value")
  out <- function_agg(df, "sum", to_agg = "Value", Countries, Catvar)
  expect_equal(names(out), c(names(df), "..count"))

  out <- function_agg(df, "sum", to_agg = "Value")
  expect_equal(out$Value, sum(df$Value, na.rm = T))


  df <- sample_data("Gcd-Cat-Cat-Num-Num", n = 300)
  names(df) <- c("Countries", "Cat_a", "Cat_b", "Value_x", "Value_y")
  out <- function_agg(df, "sum", to_agg = c("Value_x", "Value_y"), Countries)
  expect_equal(names(out), c(names(df)[c(-2,-3)], "..count"))

  out <- function_agg(df, "sum", to_agg = c("Value_x", "Value_y"), Countries, Cat_a, Cat_b)
  df_p <- df %>% group_by(Countries, Cat_a, Cat_b) %>% summarise(Value_x = sum(Value_x, na.rm = TRUE), Value_y = sum(Value_y, na.rm = TRUE))
  expect_identical(out[,-6], df_p)

})
