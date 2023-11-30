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
library(RColorBrewer)
library(plotly)
library(viridis)
#library(gganimate)

library(spData) # For getting spatial data
library(sf) # For preserving spatial data


#load dataset
data <- read.csv(file = 'WHI_geo_new.csv',header = TRUE, sep = ';', colClasses = c('character','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    theme = "styles.css",
    
    #setBackgroundImage(
    #  src = "https://img.freepik.com/free-photo/peachy-blur-gradient-background-soft-vintage-style_53876-108717.jpg?w=2000&t=st=1699966170~exp=1699966770~hmac=5601c1c7ff8a723798c8c1823c3724703cb80eea5b176c9d698adf8461753bc9"
    #),
  
    # Application title
    headerPanel(h1("World Happiness Index", align='center')),
    

        
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  #MICHAL'S GRAPHS
                  tabPanel(h4("Map"), 
                           selectInput("input_mode", "Select Mode:", c("Normal","Colorblind"), selected = "Normal"),
                           sliderInput("slider_year", "Select year:",
                                              min = 2006, max = 2022, value = 2014, step=1, sep=" ", ticks=FALSE, width = "100%"),
                           leafletOutput("map_graph",  width = "1100px", height = "600px")),
                  
                  tabPanel(
                    h4("Charts"),
                    
                      h4("Line Chart"),plotOutput("line_chart", width ="1100px"),
                      h4("Pie Chart") , selectInput("input_year", "Select Year:", c(2006:2022)), plotOutput("pie_chart",  width = "100%")
                    
                  ),
        
                  
                  #KARITA'S GRAPHS
                  tabPanel(
                    h4("Happiness vs."),
                    selectInput("variable", "Select Variable:",
                                choices = c("GDP" = "Log.GDP.per.capita", "Life Expectancy" = "Healthy.life.expectancy.at.birth", "Social support" = "Social.support", "Freedom to make life choices" = "Freedom.to.make.life.choices", "Generosity" = "Generosity", "Perceptions of corruption" = "Perceptions.of.corruption"),
                                ),plotlyOutput("scatterplot")
                  ),
                  tabPanel(
                    h4("Time Series"),
                    selectInput("selected_country", "Select Country:",
                                choices = unique(data$Country.name),
                                selected = "Denmark"),
                    selectInput("variable_time_series", "Select Variable:",
                                choices = c("GDP" = "Log.GDP.per.capita", "Life Expectancy" = "Healthy.life.expectancy.at.birth", "Social support" = "Social.support", "Freedom to make life choices" = "Freedom.to.make.life.choices", "Generosity" = "Generosity", "Perceptions of corruption" = "Perceptions.of.corruption")),
                    plotlyOutput("time_series")
                  ),
                  
                  #OPHELIE'S GRAPHS
                  tabPanel(
                    h4("Ophelie"),
                    selectInput(inputId = "selected_region_line_plot", 
                                label = "Select a region", 
                                choices = c("Western and northern Europe" = 1,
                                            "Central and Easten Europe" = 10,
                                            "Middle east" = 2,
                                            "Southeast Asia" = 3,
                                            "South Asia" = 4,
                                            "North America and ANZ" = 5,
                                            "Africa" = 6,
                                            "Latin America and Caribbean" = 7,
                                            "East Asia" = 8,
                                            "Commonwealth of Independent States" = 9)),
        
  
                    
                    plotlyOutput(outputId = "line_plot", height = "500px", width = "800px"),
                    #HTML("<p>Double-click on a legend to isolate one trace<br>Click once on a legend to make this trace disappear<br><br><br></p>"),
                    plotOutput(outputId = "line_plot1"),  # Nouvelle sortie pour le deuxième graphique
                    #HTML("<p><br><br><br></p>"),
                    selectInput(inputId = "selected_region_line_plot2", 
                                label = "Select a region", 
                                choices = c("Western and northern Europe" = 1,
                                            "Central and Easten Europe" = 10,
                                            "Middle east" = 2,
                                            "Southeast Asia" = 3,
                                            "South Asia" = 4,
                                            "North America and ANZ" = 5,
                                            "Africa" = 6,
                                            "Latin America and Caribbean" = 7,
                                            "East Asia" = 8,
                                            "Commonwealth of Independent States" = 9)),
                    plotOutput(outputId = "line_plot2"),
                    #HTML("<p><br><br><br></p>"),
                    plotOutput(outputId = "summary_plot")
                    
                  ),
                  
                  #CASSANDRA'S GRAPHS
                  tabPanel(
                    h4("Q6,Q7"),
                    selectInput("selected_year", "Select Year", choices = unique(data$year)),
                    selectInput("selected_factor", "Select Factor", choices = c("Life.Ladder", "Social.support", "Healthy.life.expectancy.at.birth", "Freedom.to.make.life.choices", "Generosity", "Perceptions.of.corruption", "Positive.affect", "Negative.affect")),
                    radioButtons("happiness_type", "Select Happiness Type", choices = c("Happy", "Unhappy"), selected = "Happy"),
                    plotlyOutput(outputId = "factor_plot"),
                    textOutput("selected_value")  # Display selected value
                  )
                  # add more panels here
                 )
          ,class="flex-center"
      )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #MICHAL'S SERVER CODE
    
    #Categorize Life Ladder
    data$happiness_cat <- ifelse(data$Life.Ladder <= 4.5, "Unhappy", ifelse(data$Life.Ladder <= 6 & data$Life.Ladder >4.5, "Neutral", ifelse(data$Life.Ladder > 6, "Happy", "Other")))
      
    
    #Create Line chart (Question no. 2a)
    output$line_chart <- renderPlot({
      
      line_count <- as.data.frame(table(data$year, data$happiness_cat))
      colnames(line_count) <- c("year", "cat", "count")
      
      ggplot(line_count, aes(x = year, y = count, color = cat, group = cat)) +
        geom_line() +
        geom_point() +
        labs(title = "Line Chart of Happiness Categories Over Years",
             x = "Year",
             y = "Count",
             color = "Category") +
        theme_dark() +
        scale_color_manual(values = c("#98FB98", "#FFFF99", "#FF9999"))
    })
    
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
      
      if(input$input_mode == "Normal")
        pal <- colorNumeric(palette = c("#FF9999", "#FFFF99", "#98FB98"), domain = countries$Life.Ladder)  
      else
        pal <- colorNumeric(palette = c("#f1a340", "#f7f7f7", "#998ec3"), domain = countries$Life.Ladder)  
      
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
                                        "Happiness Index: ", countries$Life.Ladder, "<br>")) %>%
        addLegend("bottomright", pal = pal, values = c(8 , 2),
                  title = "Happiness Index",
                  labFormat = labelFormat(prefix = " "),
                  opacity = 1
        )
    })
    
    
    #KARITA'S SERVER CODE
    
    # ********************************************* 
    #question 8 and 9 Happiness score vs. another variable
    # Calculate average Happiness Score per country
    # Create an interactive scatterplot
    output$scatterplot <- renderPlotly({
      variable_selected <- input$variable
      
      avg_data <- data %>%
        group_by(Country.name) %>%
        summarize(Avg_variable = mean(!!sym(variable_selected)),
                  Avg_Happiness = mean(Life.Ladder))
      
      plot_ly(avg_data, text = ~Country.name, x = ~Avg_variable, y = ~Avg_Happiness, type = "scatter", mode = "markers") %>%
        layout(title = paste(variable_selected, "vs. Happiness Score"), 
               xaxis = list(title = paste("Average", variable_selected)),
               yaxis = list(title = "Average Happiness Score"))
    })
    
    #Q 8 and 9 continues: Happiness on y, year on X, mapping each country individually
    #development of happiness vs variable through time
    # Development of happiness vs variable through time
    output$time_series <- renderPlotly({
      variable_selected <- input$variable_time_series
      selected_country <- input$selected_country
      
      # Filter data based on selected country
      selected_data <- data[data$Country.name == selected_country, ]
      
      #categorising happiness levels based on LifeLadder
      selected_data$happiness_score <- cut(selected_data$Life.Ladder,
                                           breaks = c(-Inf, 4.5, 6, Inf),
                                           labels = c("unhappy", "neutral", "happy"),
                                           include.lowest = TRUE)
      
      # Map happiness levels to colors
      # Map happiness levels to colors
      color_pal <- c("unhappy" = "red", "neutral" = "blue", "happy" = "black")
      colors <- factor(selected_data$happiness_score, levels = names(color_pal))
      
      plot_ly(selected_data, x = ~year, y = ~get(variable_selected), text = ~paste("Country: ", Country.name, "<br>GDP: ", get(variable_selected), "<br>Happiness: ", Life.Ladder), color = colors, type = "scatter", mode = "markers") %>%
        layout(title = paste(variable_selected, "vs. Happiness Score"),
               xaxis = list(title = "Year"),
               yaxis = list(title = paste("Average", variable_selected)),
               showlegend = TRUE,
               legend = list(title = "Happiness Level", labels = levels(colors), traceorder = "normal"),
               margin = list(t = 100)
        )
      
    })
    
    #OPHELIE'S CODE
    output$line_plot <- renderPlotly({
      selected_data <- data[data$region == input$selected_region_line_plot, ]
      
      # Remove rows with NAs in the Life.Ladder column
      selected_data <- selected_data[complete.cases(selected_data$Life.Ladder), ]
      
      region_name <- switch(as.numeric(input$selected_region_line_plot),
                            "Western and northern Europe",
                            "Central and Easten Europe",
                            "Middle east",
                            "Southeast Asia",
                            "South Asia",
                            "North America and ANZ",
                            "Africa",
                            "Latin America and Caribbean",
                            "East Asia",
                            "Commonwealth of Independent States")
      
      p <- ggplot(selected_data, aes(year, `Life.Ladder`, color = Country.name, 
                                     text = paste("Country : ", Country.name, "<br>Year : ", year, "<br>Level of Happiness : ", `Life.Ladder`),
                                     group = Country.name)) +
        geom_line() +
        geom_point() +
        xlab("Year") + ylab("Value of Happiness") +
        ggtitle(paste("Evolution of Happiness in", region_name)) +
        theme(legend.position = "bottom")
      
      
      # Create plotly plot
      p <- ggplotly(p, tooltip = "text")
      
      # Customize legend
      p <- p %>% layout(legend = list(title = "Country",
                                      orientation = "h",
                                      y = -0.2,  # Adjust the vertical position
                                      x = 0.5,
                                      xanchor = "center"),
                        margin = list(l = 50, r = 50, b = 50, t = 50))  # Adjust margin values as needed
      
      p
    })
    
    output$line_plot1 <- renderPlot({
      # Convertir year en numérique si nécessaire
      data$year <- as.numeric(data$year)
      
      # Convertir region en caractère si nécessaire
      data$region <- as.character(data$region)
      
      # Agréger les données par région et année pour obtenir la moyenne du bonheur
      summary_data <- data %>%
        group_by(region, year) %>%
        summarize(Average_Life_Ladder = mean(`Life.Ladder`), .groups = "drop")
      
      # Sélectionner les 10 régions avec les moyennes les plus élevées
      top_regions <- summary_data %>%
        group_by(region) %>%
        summarize(Max_Average_Life_Ladder = max(Average_Life_Ladder)) %>%
        top_n(10, Max_Average_Life_Ladder) %>%
        pull(region)
      
      # Filtrer les données uniquement pour les 10 meilleures régions et exclure "N/A"
      filtered_data <- summary_data %>%
        filter(region %in% top_regions, region != "N/A")
      
      # Définir une palette de couleurs pour les régions
      my_colors <- c("red", "blue", "green", "purple", "orange", "pink", "brown", "gray", "cyan", "yellow")
      
      # Définir les étiquettes personnalisées pour la légende
      my_labels <- c("Western and northern Europe", "Central and Easten Europe", "Middle east", "Southeast Asia", "South Asia", "North America and ANZ", "Africa", "Latin America and Caribbean", "East Asia", "Commonwealth of Independent States")
      
      # Convertir la variable region en un facteur avec un ordre spécifique
      filtered_data$region <- factor(filtered_data$region, levels = unique(filtered_data$region))
      
      # Tracer le graphique avec la palette de couleurs personnalisée et les étiquettes de légende
      ggplot(filtered_data, aes(x = year, y = Average_Life_Ladder, color = region, group = region)) +
        geom_line() +
        geom_point() +
        xlab("Year") + ylab("Value of Happiness") +
        ggtitle(paste("Evolution of Happiness per regions")) +
        scale_color_manual(values = my_colors, labels = my_labels) +
        theme(legend.position = "bottom")  # Déplacer la légende en dessous 
    })
    
    
    output$line_plot2 <- renderPlot({
      selected_data <- data[data$region == input$selected_region_line_plot2, ]
      
      region_names <- c("Western and northern Europe",
                        "Middle east",
                        "Southeast Asia",
                        "South Asia",
                        "North America and ANZ",
                        "Africa",
                        "Latin America and Caribbean",
                        "East Asia",
                        "Commonwealth of Independent States",
                        "Central and Easten Europe")
      
      region_name <- region_names[as.numeric(input$selected_region_line_plot2)]
      
      ggplot(selected_data, aes(year, `Life.Ladder`)) +
        geom_line(stat = "summary", fun.y = "mean") +
        geom_point(stat = "summary", fun.y = "mean") +
        ylim(0, 10) +
        xlab("Year") + ylab("Average Value of Happiness") +
        ggtitle(paste("Average Evolution of Happiness in", region_name)) +
        annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 4.5, fill = "red", alpha = 0.3) +
        annotate("rect", xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 6, fill = "orange", alpha = 0.3) +
        annotate("rect", xmin = -Inf, xmax = Inf, ymin = 6, ymax = 10, fill = "green", alpha = 0.3)
    })
    
    
    
    
    
    output$summary_plot <- renderPlot({
      summary_data <- data %>%
        group_by(year) %>%
        summarize(Average_Life_Ladder = mean(`Life.Ladder`))
      
      ggplot(summary_data, aes(year, Average_Life_Ladder)) +
        geom_line() +
        geom_point() +  # Ajouter un point à chaque endroit
        xlim(2006, max(summary_data$year)) +  # Définir l'échelle de l'axe x
        ylim(4, 6.5) +  # Définir l'échelle de l'axe y
        annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 4.5, fill = "red", alpha = 0.3) +
        annotate("rect", xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 6, fill = "orange", alpha = 0.3) +
        annotate("rect", xmin = -Inf, xmax = Inf, ymin = 6, ymax = Inf, fill = "green", alpha = 0.3) +
        xlab("Year") + ylab("Value of Happiness") +
        ggtitle(paste("Evolution of Happiness in the world")) 
    })
    
    
    #CASSANDRA'S CODE
    # Create a plot for selected factor values for the top 5 countries in the selected year
    output$factor_plot <- renderPlotly({
      selected_year <- input$selected_year
      selected_factor <- input$selected_factor
      happiness_type <- input$happiness_type
      
      # Filter data for the selected year and happiness type
      selected_data <- data %>%
        filter(year == selected_year, happiness_cat == happiness_type) %>%
        arrange(desc(Life.Ladder)) %>%
        slice(1:5) %>%
        mutate(Country.name = factor(Country.name, levels = rev(Country.name)))
      
      # Use viridis color palette for each country
      colors <- viridis(length(unique(selected_data$Country.name)), option = "C")
      
      # Map country names to unique colors
      color_mapping <- setNames(colors, unique(selected_data$Country.name))
      country_colors <- color_mapping[selected_data$Country.name]
      
      # Plot the selected factor values for the top 5 countries
      p <- plot_ly(selected_data, x = ~Country.name, y = ~get(selected_factor), type = 'bar', marker = list(color = country_colors))
      
      # Adjust y-axis scale only when "Life.Ladder" is selected
      if (selected_factor == "Life.Ladder") {
        p <- p %>% layout(yaxis = list(title = selected_factor, range = c(0, 10), tick0 = 0, dtick = 0.5),
                          yaxis2 = list(title = selected_factor, overlaying = "y", side = "right", range = c(0, 10), tick0 = 0, dtick = 0.5))
      }
      
      # Adjust y-axis scale only when "Social.support" is selected
      if (selected_factor == "Social.support") {
        p <- p %>% layout(yaxis = list(title = selected_factor, range = c(0, 1), tick0 = 0, dtick = 0.1),
                          yaxis2 = list(title = selected_factor, overlaying = "y", side = "right", range = c(0, 1), tick0 = 0, dtick = 0.1))
      }
      
      # Adjust y-axis scale only when "Healthy.life.expectancy.at.birth" is selected
      if (selected_factor == "Healthy.life.expectancy.at.birth") {
        p <- p %>% layout(yaxis = list(title = selected_factor, range = c(0, 100), tick0 = 0, dtick = 10),
                          yaxis2 = list(title = selected_factor, overlaying = "y", side = "right", range = c(0, 100), tick0 = 0, dtick = 10))
      }
      
      # Adjust y-axis scale only when "Freedom.to.make.life.choices" is selected
      if (selected_factor == "Freedom.to.make.life.choices") {
        p <- p %>% layout(yaxis = list(title = selected_factor, range = c(0, 1), tick0 = 0, dtick = 0.1),
                          yaxis2 = list(title = selected_factor, overlaying = "y", side = "right", range = c(0, 1), tick0 = 0, dtick = 0.1))
      }
      
      # Adjust y-axis scale only when "Generosity" is selected
      if (selected_factor == "Generosity") {
        p <- p %>% layout(yaxis = list(title = selected_factor, range = c(-0.4, 0.4), tick0 = -0.4, dtick = 0.1),
                          yaxis2 = list(title = selected_factor, overlaying = "y", side = "right", range = c(-0.4, 0.4), tick0 = -0.4, dtick = 0.1))
      }
      
      # Adjust y-axis scale only when "Positive.affect" is selected
      if (selected_factor == "Positive.affect") {
        p <- p %>% layout(yaxis = list(title = selected_factor, range = c(0, 1), tick0 = 0, dtick = 0.1),
                          yaxis2 = list(title = selected_factor, overlaying = "y", side = "right", range = c(0, 1), tick0 = 0, dtick = 0.1))
      }
      
      # Adjust y-axis scale only when "Negative.affect" is selected
      if (selected_factor == "Negative.affect") {
        p <- p %>% layout(yaxis = list(title = selected_factor, range = c(0, 1), tick0 = 0, dtick = 0.1),
                          yaxis2 = list(title = selected_factor, overlaying = "y", side = "right", range = c(0, 1), tick0 = 0, dtick = 0.1))
      }
      
      # Register the plotly_click event
      event_register(p, 'plotly_click')
      
      # Print the plot
      p
    })
    
    # React to clicks on the plot and display selected values
    observeEvent(event_data("plotly_click"), {
      event_data <- event_data("plotly_click")
      
      if (!is.null(event_data)) {
        output$selected_value <- renderText({
          paste("Selected Values: \n",
                input$selected_factor, ": ", event_data$y, "\n",
                "Country: ", event_data$x)
        })
      }
    })
}




# Run the application 
shinyApp(ui = ui, server = server)
