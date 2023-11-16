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
library(shinyWidgets)
#library(gganimate)

library(spData) # For getting spatial data
library(sf) # For preserving spatial data

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    theme = "styles.css",
    
    setBackgroundImage(
      src = "https://img.freepik.com/free-photo/peachy-blur-gradient-background-soft-vintage-style_53876-108717.jpg?w=2000&t=st=1699966170~exp=1699966770~hmac=5601c1c7ff8a723798c8c1823c3724703cb80eea5b176c9d698adf8461753bc9"
    ),
  
    # Application title
    headerPanel(h1("World Happiness Index", align='center')),
    

    # Sidebar with a slider input for number of bins 
    #sidebarLayout(
    #    sidebarPanel(
    #        
    #    ),

        # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Map", sliderInput("slider_year", "Select year:",
                                              min = 2006, max = 2022, value = 2014, step=1, sep=" ", ticks=FALSE, width = "100%"),
                  leafletOutput("map_graph",  width = "1000px", height = "600px")),
                  
                  tabPanel("Plots", selectInput("input_year", "Select Year:", c(2006:2022)), plotOutput("pie_chart",  width = "1000px", height = "600px")),
                  
                  # add more panels here
                 )
          ,class="flex-center"
      )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    print(world[c(2,11)]$name_long)
    
    #load dataset
    data <- read.csv(file = 'WHI_geo_new.csv',header = TRUE, sep = ';', colClasses = c('character','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
    
    #Categorize Life Ladder
    data$happiness_cat <- ifelse(data$Life.Ladder <= 4.5, "Unhappy", ifelse(data$Life.Ladder <= 6 & data$Life.Ladder >4.5, "Neutral", ifelse(data$Life.Ladder > 6, "Happy", "Other")))
      
 
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
        geom_text(aes(y = label_pos, label = count), color = "black")+
        theme_void()+
        xlim(0.5, 2.5)+
        scale_fill_manual(values = c("#98FB98", "#FFFF99", "#FF9999"))
    })
    
    
    
    
    #create the map
    output$map_graph <- renderLeaflet({
      
      #selected data
      selected_data <- data[data$year == input$slider_year, ]
      
      #spatial data from spData library
      mapData <- world[c(2,11)]
      
      #join data with spatial data
      countries <- left_join(selected_data, mapData, c("Country.name" = "name_long"))
      
      #Color pallete for the map  
      pal <- colorNumeric(palette = c("#FF9999", "#FFFF99", "#98FB98"), domain = countries$Life.Ladder)  
      
      map <- leaflet(data) %>% 
        setView(lng = 0, lat = 45, zoom = 2)  %>% #setting the view over ~ center of North America
        addTiles()
    
      #add polygons to map
      map %>% addPolygons(data = countries$geom,
                          fillColor = pal(countries$Life.Ladder),
                          fillOpacity = .7,
                          color = "grey",
                          weight = 1,
                          popup = paste("Country: ", countries$Country.name, "<br>",
                                        "Happiness Index: ", countries$Life.Ladder, "<br>"))
    })
    
   
}




# Run the application 
shinyApp(ui = ui, server = server)
