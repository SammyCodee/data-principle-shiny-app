library(shiny)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(sf)
library(shinythemes)
library(shinydashboard)

data_lower_secondary <- read.csv("Lower_secondary.csv")
filter_countryList <- data_lower_secondary[1:110, ]

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

default_Latitude <- 33.939110
default_Longitude <- 67.709953
default_zoom <- 2

ui <- navbarPage("Student Dropout Rate", theme = shinytheme("flatly"),
             tabPanel("Map",
                      selectInput(inputId = "countryList",
                                  label = "Select one or multiple country: ",
                                  multiple = TRUE,
                                  choices = sort(filter_countryList$Countries.and.Areas),
                                  selected = "Afghanistan"
                      ),
                      
                      h5("Tips: Press the marker on the map to see the info."),
                      
                      leafletOutput(outputId = "leafletMap")
              ),
             tabPanel("About",
                      h2("Introduction"),
                      h4("This analysis is to provide an overview on dropout rate of 
                      students in secondary level education from multiple countries."),
                      h4("Few things were taken into consideration while carrying out 
                      this analysis such as development status of a country,  
                      type of the residence and gender of students."),
                      
                      h2("Beneficiary"),
                      h4("Ministry of Education"),
                      
                      h2("Goals"),
                      h4("To draw critical insights"),
                      h4("To provide a decision support on planning the 
                        redemption scheme the student dropout rate")
             )
             
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