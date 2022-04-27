#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    column(2,
           h4("Select IVs:"),
           checkboxInput("iv1", "IV 1", value = FALSE),
           checkboxInput("iv2", "IV 2", value = FALSE)
    ),
    column(5,
           plotlyOutput('plot')
    ),
    column(5,
           htmlOutput("lm1")
    )
)
)

server <- function(input, output) {
  # Loading the data in the server
  df <- rio::import("data/graph_reg.Rds")

  # Create a plotly output!
  #  Warning! Make sure you load the plotly library in the begining of the code!
  output$plot <- renderPlotly({
    iv1 <- input$iv1
    iv2 <- input$iv2

    # If IV 1 checkbox is checked, and not IV 2
    if (iv1 & !iv2) {
      plot_ly(x = df$iv,
        y = df$dv)
      # If IV 2 checkbox is checked, and not IV 1
    } else if (!iv1 & iv2) {
      plot_ly(x = df$iv_2,
        y = df$dv)
      # If both are checked
    } else if (iv1 & iv2) {
      plot_ly(x = df$iv,
        y = df$iv_2,
        z = df$dv)
      # If none are checked
    } else { plot_ly() }
  })

  output$lm1 <- renderText({

    iv1 <- input$iv1
    iv2 <- input$iv2

    # If IV 1 checkbox is checked, and not IV 2
    if (iv1 & !iv2) {
      model_1 <- lm(dv ~ iv, data = df)
      # If IV 2 checkbox is checked, and not IV 1
    } else if (!iv1 & iv2) {
      model_1 <- lm(dv ~ iv_2, data = df)
      # If both are checked
    } else if (iv1 & iv2) {
      model_1 <- lm(dv ~ iv + iv_2, data = df)
      # If none are checked
    } else { model_1 <- "" }

    stargazer::stargazer(model_1, type = "html")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
