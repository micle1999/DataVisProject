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
           plotOutput("line_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    #load dataset
    happiness_data <- read.csv(file = 'WHI.csv',header = TRUE, sep = ';')
  
      
    output$line_plot <- renderPlot({
        # generate bins based on input$bins from ui.R
        ggplot(happiness_data %>% filter(Country.name == 'Slovakia'), 
             aes(year, `Life.Ladder`,  group = Country.name)) + geom_line()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
