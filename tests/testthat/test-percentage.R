# Function percentage ---------------------------------------------------------------------------------

test_that("percentage_data() works with one categorical column in data.frame", {

  df <- sample_data("Gcd", n = 300)
  names(df) <- c("Countries")
  pre_out <- function_agg(df, "sum", to_agg = NULL, Countries)
  out <- percentage_data(pre_out, "..count",NULL)
  expect_equal(sum(out$..percentage), 100)


  df <- sample_data("Gcd-Num", n = 300)
  names(df) <- c("Countries", "Value")
  pre_out <- function_agg(df, "sum", to_agg = "Value", Countries)

  out <- percentage_data(pre_out, "Value",NULL)
  expect_equal(sum(out$..percentage), 100)


})



test_that("percentage_data() works with more than one categorical column in data.frame", {

  df <- sample_data("Gcd-Cat", n = 300, addNA = F)
  names(df) <- c("Countries", "Category")
  pre_out <- function_agg(df, "sum", to_agg = NULL, Countries, Category)
  out <- percentage_data(pre_out, "..count", NULL)
  out_test <- round(as.numeric(out %>% group_by(Countries) %>% summarise(total = sum(..percentage)) %>% .$total))
  expect_true(all(out_test == 100))

  df <- sample_data("Gcd-Cat-Num", n = 300, addNA = F)
  names(df) <- c("Countries", "Category" , "Value")
  pre_out <- function_agg(df, "sum", to_agg = "Value", Countries, Category)
  out <- percentage_data(pre_out, "Value", "Countries")
  out_test <- round(as.numeric(out %>% group_by(Countries) %>% summarise(total = sum(..percentage)) %>% .$total))
  expect_true(all(out_test == 100))

  out <- percentage_data(pre_out, "Value", "Category")
  out_test <- round(as.numeric(out %>% group_by(Category) %>% summarise(total = sum(..percentage)) %>% .$total))
  expect_true(all(out_test == 100))

})
