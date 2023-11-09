#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("utils.R")

library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(stringr)

library(spData) # For getting spatial data
library(sf) # For preserving spatial data

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("World Happiness Index"),

    # Sidebar with a slider input for number of bins 
    #sidebarLayout(
    #    sidebarPanel(
    #        
    #    ),

        # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Map", leafletOutput("heatmap")),
                  tabPanel("Plots", plotOutput("pie_chart"), 
                           selectInput("input_year", "Select Year:",
                              c(2006:2022)))
      )
      
      #leafletOutput(outputId = "heatmap"),
      #fluidRow(
        #splitLayout(cellWidths = c("50%", "50%"), plotOutput("line_plot"), plotOutput("line_plot2"))
      #)
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    #load dataset
    data <- read.csv(file = 'WHI_geo.csv',header = TRUE, sep = ';', colClasses = c('character','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
    
    #Categorize Life Ladder
    data$happiness_cat <- ifelse(data$Life.Ladder <= 4.5, "Unhappy", ifelse(data$Life.Ladder <= 6 & data$Life.Ladder >4.5, "Neutral", ifelse(data$Life.Ladder > 6, "Happy", "Other")))
      
    #define the color of for the depth of the earquakes
    color_pal <- colorFactor(
      palette = c('green', 'yellow', 'red'),
      domain = data$happiness_cat
    )
    
    #Create Pie chart (Question no.2a)
    output$pie_chart <- renderPlot({
      
      selected_data <- data[data$year == input$input_year, ]
      
      pie_count <- data.frame("cat" = c("Unhappy","Neutral","Happy"),
                              "count" = c(nrow(selected_data[selected_data$happiness_cat == 'Unhappy', ]),nrow(selected_data[selected_data$happiness_cat == 'Neutral', ]),nrow(selected_data[selected_data$happiness_cat == 'Happy', ])))
      
      pie_count <- pie_count %>%
        arrange(desc(cat)) %>%
        mutate(label_pos = cumsum(count) - 0.5*count)
      
      ggplot(pie_count, aes(x = 2, y = count, fill = cat)) +
        geom_bar(stat = "identity", color = "white") +
        coord_polar(theta = "y", start = 0)+
        geom_text(aes(y = label_pos, label = count), color = "white")+
        theme_void()+
        xlim(0.5, 2.5)
    })
    
    
    #create the map
    output$heatmap <- renderLeaflet({
      
      #spatial data from spData library
      mapData <- world[c(2,11)]
      
      print(mapData)
      
      #join data with spatial data
      countries <- left_join(data, mapData, c("Country.name" = "name_long"))
      
      #Color pallete for the map  
      pal <- colorNumeric(palette = "YlOrRd", domain = countries$Life.Ladder)  
      
      map <- leaflet(data) %>% 
        setView(lng = -99, lat = 45, zoom = 2)  %>% #setting the view over ~ center of North America
        addTiles()
        #addCircles(opacity = 70, data = data, lat = data$Latitude, lng = data$Longitude, weight = 1, radius = 100000, popup = ~as.character(Country.name), label = ~as.character(paste0("Happines of the country: ", sep = " ", happiness_cat)), color = ~color_pal(happiness_cat), fillOpacity = 0.5)
      
      #add polygons to map
      map %>% addPolygons(data = countries$geom,
                          fillColor = pal(countries$Life.Ladder),
                          fillOpacity = .7,
                          color = "grey",
                          weight = 1)
    })
    
    
    output$line_plot <- renderPlot({
        # generate bins based on input$bins from ui.R
        ggplot(data %>% filter(Country.name == 'Slovakia'), 
             aes(year, `Life.Ladder`,  group = Country.name)) + geom_line()
    })
   
}




# Run the application 
shinyApp(ui = ui, server = server)
