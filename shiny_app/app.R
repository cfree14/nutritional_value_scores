
# Clear
rm(list = ls())
options(dplyr.summarise.inform=F)

# Setup
################################################################################

# Packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(RColorBrewer)

# Directories
datadir <- "data" # for actual app
codedir <- "code"  # for actual app
# datadir <- "shiny_app/data" # when testing
# codedir <- "shiny_app/code" # when testing

# Read data
scores <- readRDS(file.path(datadir, "scores.Rds"))

# Read scripts
sapply(list.files(codedir), function(x) source(file.path(codedir, x)))

# Countries
countries <- scores$country %>% unique()



# User interface
################################################################################

# User interface
ui <- fluidPage(

  # Title
  titlePanel("Nutritional value score exploration tool"),

  # Select species
  selectInput(inputId = "country", label = "Select a country:",
              choices = countries,  multiple = F, selected=countries[1]),
  br(),


  # Plot historical comparison
  plotOutput(outputId = "plot_overall", width=450, height=700)


)


# Server
################################################################################

# Server
server <- function(input, output, session){

  # Plot data
  output$plot_overall <- renderPlot({
    g <- plot_overall(data = scores,
                      country=input$country)
    g
  })

}

shinyApp(ui = ui, server = server)
