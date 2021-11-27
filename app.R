library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  setBackgroundColor(
    color = c("#ffffcc", "#003399"),
    gradient = "linear",
    direction = "bottom"
  ),
  titlePanel("Student Dropout Rates"),
  
  fluidRow(
    navlistPanel(
      "Menu",
      tabPanel(
        title = "Component 1"
      ),
      tabPanel(
        title = "Component 2"
      )
    )
      
      
      
  )
)

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)