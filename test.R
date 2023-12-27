library(gapminder)
library(ggplot2)
library(shiny)
library(gganimate)
library(png)
library(gifski)
theme_set(theme_bw())
ui <- basicPage(
  imageOutput("plot1"))
server <- function(input, output) {
  output$plot1 <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.gif')
    # now make the animation
    p = ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, 
                              color = continent)) + geom_point() + scale_x_log10() +
      transition_time(year) # New
    anim_save("outfile.gif", animate(p)) # New
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)}
shinyApp(ui, server)