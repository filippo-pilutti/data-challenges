#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(gapminder)
library(ggplot2)
library(dplyr)


ui <- fluidPage(
  
  # Application title
  titlePanel("Shiny Gapminder"),
  
  # Sidebar with a slider input for the year, selection for the
  # continent and radio buttons for X and Y axis.
  sidebarLayout(
    sidebarPanel(
      # Year slider
      sliderInput("year",
                  "Year",
                  min = 1952,
                  max = 2007,
                  value = 1952,
                  step = 5),
      # Continent selection
      selectInput("continent",
                  "Continent",
                  choices = list("Asia", "Europe", "Africa", "Americas", "Oceania", "All"),
                  selected = "All"),
      # X axis selection
      radioButtons("x",
                   "X",
                   choices = list("Life expectancy" = "lifeExp",
                                  "Population" = "pop",
                                  "GDP per capita" = "gdpPercap"),
                   selected = "gdpPercap"),
      # Y axis selection
      radioButtons("y",
                   "Y",
                   choices = list("Life expectancy" = "lifeExp",
                                  "Population" = "pop",
                                  "GDP per capita" = "gdpPercap"),
                   selected = "lifeExp")
    ),
    
    # Show the plot
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

# Server logic
server <- function(input, output) {
  
  # Everything that needs to be reactive goes inside here
  output$scatterPlot <- renderPlot({
    
    # We always filter by year; filter by continent only if "All" is NOT selected
    data <- filter(gapminder, year == input$year)
    if (input$continent != "All") {
      data <- filter(data, continent == input$continent)
    }
    
    # Render the plot
    ggplot(data, aes_string(x = input$x, y = input$y, size = "pop", color = "country")) +
      geom_point(show.legend = FALSE, alpha = 0.5, stroke = 0.8) +
      scale_color_manual(values = country_colors) +
      scale_size(range = c(1, 20)) +
      # On both axis, if lifeExp we want continuous scale, otherwise log
      (if (input$x == "lifeExp") scale_x_continuous() else scale_x_log10()) +
      (if (input$y == "lifeExp") scale_y_continuous() else scale_y_log10()) +
      labs(
        x = if (input$x == "lifeExp") {
          "Life expectancy"
        } else if (input$x == "pop") {
          "Population"
        } else {
          "GDP per capita"
        },
        y = if (input$y == "lifeExp") {
          "Life expectancy"
        } else if (input$y == "pop") {
          "Population"
        } else {
          "GDP per capita"
        }
      ) +
      theme_minimal()
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)