library(shiny)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(sf)
library(shinythemes)
library(shinydashboard)
library(tidyr)
library(ggplot2)
library(GGally)

data_lower_secondary <- read.csv("Lower_secondary.csv")
filter_countryList <- data_lower_secondary[1:110, ]

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

default_Latitude <- 33.939110
default_Longitude <- 67.709953
default_zoom <- 2

#Get country with highest dropout rate
higest_dropout_rate_country <- select(filter_countryList, c("Region", "Countries.and.Areas", "Development",
                                                            "Total"))


ui <- navbarPage("Student Dropout Rate", theme = shinytheme("flatly"),
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
             ),
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
             tabPanel("Insights",
                      
                      h2("1. The comparison of less developed and more develop country"),
                      
                      plotOutput("developed_plot"),
                      
                      h4("From the bar chart it shows the least developed country has the highest dropout rate."),
                      
                      h2("2. The gender dropout rate in less developed regions"),
                      
                      plotOutput("gender_plot"),
                      
                      h4("Comparison of location - Boxplot shows that the median of female is greater than male in less develop countries."),
                      h4("Comparison of dispersion - Overall range of the data set is greater for the female (as shown by the distances between 
                         the ends of the two whiskers for each boxplot."),
                      
                      h2("3. The relationship of student dropout rate and place of residence"),
                      
                      plotOutput("residence_plot"),
                      
                      h4("The plot above combines correlation coefficients, correlation tests 
                         (via the asterisks next to the coefficients2) and scatterplots for all possible pairs of variables present in a dataset."),
                      h4("From the analysis, we can see Rural Residence contributes to higher correlation to Student Dropout Rate
                         compared to Urban Residence with 0.9903334%."),
                      h4("We conclude the students from Rural Residence are more likely to dropout from school compared to Urban Residences."),
                      
                      h2("4. The country with highest dropout rate in each region"),
                      
                      plotOutput("country_plot"),
                      
                      h4("The country of Niger has the highest student dropout rate with total 70%")
             ),
             
             
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
  
  #Insights Question 1
  output$developed_plot <- renderPlot({
    dp <- select(filter_countryList, Countries.and.Areas:Urban.Residence,
                               -c(Latitude, Longitude, School.Age, Region, UNICEF.Subregion))
    
    dp[!complete.cases(dp), ]

    dp1 <- na.omit(dp)

    dp2 <- spread(dp1, Development, Total)

    #replace the NA
    dp3 <- dp2
    dp3[is.na(dp3)] = 0
    dp3 <- dp3 %>% rename(Least.Developed = 'Least Developed', Less.Developed = 'Less Developed',
                                        More.Developed = 'More Developed')

    #select only "developed" columns
    dp4 <- select(dp3, Least.Developed: More.Developed)
    dp4[,1] = as.integer(dp4[,1])
    dp4[,2] = as.integer(dp4[,2])
    dp4[,3] = as.integer(dp4[,3])
    average <- apply(dp4, 2, mean)
    dp5 <- average
    dp6 <- as.data.frame(dp5)
    dp6 <- t(dp6)

    barplot(dp6, main = "Average Dropout Rate by Development of Country",
            xlab = "Development Status",
            ylab = "Dropout Rate in %",
            col = "#aee05a",
            )
  })
  
  #Insight Question 2
  output$gender_plot <- renderPlot(({
    dp1 <- select(filter_countryList, Countries.and.Areas, Development, X, X.1, Population.data)
    
    colnames(dp1)<- c("Country","Development","Pop.female","Pop.male", "Pop.total")
    
    # stacked bar chart - Count of Country based on Development Type
    ggplot(dp1, 
           aes(x = Development,
               fill = Pop.total)) + 
      geom_bar(position = "stack")
    
    dp2 <- filter (dp1,Development=="Less Developed")
    
    ## Boxplot Compare Gender in Less Develop Countries
    ###(a) Before normalization
    boxplot(dp2$Pop.female, dp2$Pop.male,
            main ="Multiple Boxplots Comparison Gender 
        in Less Developed Countries",
            at = c(1,2),
            names = c("Female", "Male"),
            las = 1,
            col = c("orange","red"),
            border = "brown",
            horizontal = TRUE,
            notch = TRUE
    )
    
    ###(b) After Normalization
    female <- dp2$Pop.female
    male <- dp2$Pop.male
    female_norm <- rnorm(200,mean=mean(female, na.rm=TRUE), sd=sd(female, na.rm=TRUE))
    male_norm <- rnorm(200,mean=mean(male, na.rm=TRUE), sd=sd(male, na.rm=TRUE))
    boxplot(female, female_norm, male, male_norm,
            main = "  Multiple Boxplots for Comparison Gender in Less Developed Countries - Before and After Normalization",
            at = c(1,2,4,5),
            names = c("Female", "Female_N", "Male", "Male_N"),
            las = 1,
            col = c("orange","red"),
            border = "brown",
            horizontal = TRUE,
            notch = TRUE
    )
  }))
  
  #Insights Question 3
  output$residence_plot <- renderPlot({
    dp <- select(filter_countryList, Countries.and.Areas:Urban.Residence, 
                 -c(Latitude, Longitude, School.Age, Region, UNICEF.Subregion))
    
    dp[!complete.cases(dp), ]
    
    dp1 <- na.omit(dp)
    
    dp2 <- dp1
    dp2[is.na(dp2)]=0
    
    dp3 <- dp2 %>% rename("Number Of Dropouts" = Total, 
                          "Rural Residence" = Rural.Residence, 
                          "Urban Residence" = Urban.Residence)
    
    dp4 <- select(dp3, "Number Of Dropouts", "Rural Residence": "Urban Residence")
    dp4[,1] = as.integer(dp4[,1])
    dp4[,2] = as.integer(dp4[,2])
    dp4[,3] = as.integer(dp4[,3])
    
    Urban.Res.Corr <- cor.test(dp4$"Number Of Dropouts", dp4$"Urban Residence")
    
    Rural.Res.Corr <- cor.test(dp4$"Number Of Dropouts", dp4$"Rural Residence")
    
    ggpairs(dp4[ , c("Number Of Dropouts", "Urban Residence", "Rural Residence")])
  })
  
  #Insights Question 4
  output$country_plot <- renderPlot({
    filter_highest_rate <- higest_dropout_rate_country %>%
                            group_by(Region) %>%
                            slice(which.max(Total))
    
    g <- ggplot(filter_highest_rate, aes(y = Total, x = Countries.and.Areas, col="#24ffff"))
    g + geom_bar(stat = "sum") + xlab("Country and Region") + ylab("Total % of dropout rate")
    
  })
  
}

shinyApp(ui = ui, server = server)