test_that("Choropleth", {
  data <- data.frame(depto = c("Antioquia", "Quindio", "Boyaca", "Bogota"),
                     valor = runif(4, 10, 100))
  opts <- dsvizopts::dsviz_default_opts()
  opts$map$map_name <- "col_departments"
  hgch_choropleth(data, var_gnm = "depto", var_num = "valor", opts = opts)
})
