library(shiny)
library(dplyr)
library(hgchmaps)

ui <- fluidPage(
  highchartOutput("hgch_viz"),
  verbatimTextOutput("click")
)

# Server logic
server <- function(input, output) {

  output$hgch_viz <- renderHighchart({
    data <- data.frame(depto = c("Antioquia", "Quindio", "Boyaca", "Bogota"),
                       valor = runif(4, 10, 100))
    opts <- dsvizopts::dsviz_default_opts()
    opts$map$map_name <- "col_departments"
    hgch_choropleth(data, var_gnm = "depto", var_num = "valor", opts = opts)
    hgch_choropleth(data,
                    var_gnm = "depto",
                    var_num = "valor",
                    map_name = "col_departments",
                    shiny_cursor = "pointer",
                    shiny_clickable = TRUE)

  })

  output$click <- renderPrint({
    input$hcClicked
  })

}

# Complete app with UI and server components
shinyApp(ui, server)


