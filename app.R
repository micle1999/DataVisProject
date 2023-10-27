#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(leaflet.extras)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("World Happiness"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
        ),

        # Show a plot of the generated distribution
    mainPanel(
      leafletOutput(outputId = "heatmap"),
      #fluidRow(
        #splitLayout(cellWidths = c("50%", "50%"), plotOutput("line_plot"), plotOutput("line_plot2"))
      #)
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    #load dataset
    data <- read.csv(file = 'WHI.csv',header = TRUE, sep = ';')
    
    #Categorize Life Ladder
    data$happiness_cat <- ifelse(data$Life.Ladder <= 4.5, "Unhappy", ifelse(data$Life.Ladder <= 6 | data$Life.Ladder >4.5, "Neutral", ifelse(data$Life.Ladder > 6, "Happy", "Other")))
      
    #define the color of for the depth of the earquakes
    color_pal <- colorFactor(
      palette = c('red', 'yellow', 'green'),
      domain = data$happiness_cat
    )
    
    #create the map
    output$heatmap <- renderLeaflet({
      leaflet(data) %>% 
        setView(lng = -99, lat = 45, zoom = 2)  %>% #setting the view over ~ center of North America
        addTiles() %>% 
        addCircles(opacity = 50, data = data, lat = 42.546245, lng = 1.601554, weight = 1, radius = 25000, popup = ~as.character(Country.name), label = ~as.character(paste0("Happines of the country: ", sep = " ", happiness_cat)), color = ~color_pal(happiness_cat), fillOpacity = 0.5)
    })
    
    output$heatmap2 <- renderLeaflet({
      leaflet(data) %>%
        addProviderTiles(providers$CartoDB.DarkMatter) %>%
        setView( 178, -20, 5 ) %>%
        addHeatmap(
          lng = 1.601554, lat = 42.546245, intensity = 3,
          blur = 20, max = 0.05, radius = 15
        )  
    })
    
    
    output$line_plot <- renderPlot({
        # generate bins based on input$bins from ui.R
        ggplot(data %>% filter(Country.name == 'Slovakia'), 
             aes(year, `Life.Ladder`,  group = Country.name)) + geom_line()
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
