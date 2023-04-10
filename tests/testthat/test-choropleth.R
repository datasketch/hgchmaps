test_that("Choropleth", {
  data <- data.frame(depto = c("Antioquia", "Quindio", "Boyaca", "Bogota"),
                     valor = runif(4, 10, 100))
  opts <- dsvizopts::dsviz_default_opts()
  opts$map$map_name <- "col_departments"
  hgch_choropleth(data, var_gnm = "depto", var_num = "valor", opts = opts)
  hgch_choropleth(data,
                  var_gnm = "depto",
                  var_num = "valor",
                  map_name = "col_departments",
                  background_color = "black")
  hgch_choropleth_GnmNum(data, map_name = "col_departments")

  data <- data.frame(pais = c("Colombia", "Argentina", "Brazil", "Mexico"),
                     Mean = runif(4, 10, 100))
  hgch_choropleth(data = data, var_gnm = "pais", var_num = "Mean",
                  map_name = "world_countries_latin_america_caribbean")


})

