library(shiny)

#2
ui <- fluidPage(
  plotOutput("plot", width = "700px", height = "300px")
)

server <- function(input, output, session) {
  output$plot <- renderPlot(plot(1:5), res = 96, alt = "scatterplot of five random numbers")
}

shinyApp(ui, server)

#3
ui <- fluidPage(
  dataTableOutput("table")
)

server <- function(input, output, session) {
  output$table <- renderDataTable(mtcars, 
                                  options = list(searching = F, ordering = F))
}

shinyApp(ui, server)

#4
library(reactable)

ui <- fluidPage(
  reactableOutput("table")
)

server <- function(input, output, session) {
  output$table <- renderReactable(reactable(mtcars))
}

shinyApp(ui, server)
