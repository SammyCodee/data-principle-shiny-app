library(shiny)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(sf)


data_lower_secondary <- read.csv("Lower_secondary.csv")
filter_countryList <- data_lower_secondary[1:110, ]

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

default_Latitude <- 33.939110
default_Longitude <- 67.709953
default_zoom <- 2

ui <- fluidPage(
  tags$h1("Student Dropout Rate"),
  
  selectInput(inputId = "countryList",
              label = "Select 1 or more Country: ",
              multiple = TRUE,
              choices = sort(filter_countryList$Countries.and.Areas),
              selected = "Afghanistan"
  ),
  
  leafletOutput(outputId = "leafletMap")
  
)

server <- function(input, output) {
  data <- reactive({
    filter_countryList %>%
      filter(Countries.and.Areas %in% input$countryList) %>%
      mutate(INFO = paste0(Countries.and.Areas, " | ", 
                           Development, " | ", 
                           "Total: ", Total,"%"," | ",
                           "Female: ", Gender.Female,"%"," | ", 
                           "Male: ", Gender.Male, "%", " | ",
                           "School Age: ", School.Age, " | ",
                           "Rural Area: ", Rural.Residence,"%", " | ",
                           "Urban Area: ", Urban.Residence,"%"
                            ))
  })
  
  output$leafletMap <- renderLeaflet({
    leaflet(data = data()) %>%
      setView(lat = default_Latitude, lng = default_Longitude, zoom = default_zoom) %>%
      addTiles() %>%
      addMarkers(~Longitude, ~Latitude, popup = ~INFO, label = ~INFO) %>%
      addProviderTiles(providers$Esri.WorldStreetMap)
  })
  
}

shinyApp(ui = ui, server = server)