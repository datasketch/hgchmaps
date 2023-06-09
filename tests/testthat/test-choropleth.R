test_that("Choropleth", {
  data <- data.frame(pais = c("Colombia", "Argentina", "Russia"), value = c(10, 20, 30))
  hgch_choropleth(data, "value")

  data <- data.frame(pais = c("Colombia", "Argentina", "Russia"),
                     riesgo = c("bajo", "medio", "alto"),
                     color = c("green", "yellow", "orange"))
  hgch_choropleth(data, "riesgo")

})
