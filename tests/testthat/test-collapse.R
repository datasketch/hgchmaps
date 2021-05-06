# Function collapse ---------------------------------------------------------------------------------

test_that("collapse_data() works with categorical and numerica column", {

  df <- sample_data("Gcd-Cat-Num")
  out <- collapse_data(df)
  expect_identical(out[[1]], paste0(unique(df[[1]]), collapse = ". "))

  names(df) <- c("Code", "Categorie", "Num")
  out <- collapse_data(df, Categorie)
  df_exp <- df %>% group_by(Categorie) %>% summarise(Code_c = paste0(Code, collapse = ". "))
  expect_identical(out$Code, df_exp$Code_c)
})


