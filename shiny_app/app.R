
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
datadir <- "shiny_app/data" # when testing
codedir <- "shiny_app/code" # when testing

# Read data
scores_orig <- readRDS(file.path(datadir, "scores.Rds"))

# Read scripts
sapply(list.files(codedir), function(x) source(file.path(codedir, x)))

# Countries
countries <- scores$country %>% unique()

# Format data
################################################################################

# Food groups
food_groups <- c("Fruits", "Vegetables", "Legumes, nuts, seeds", "Animal-source foods", "Starchy staples")

# Format data
scores <- scores_orig %>% 
  # Format food group
  mutate(food_group=recode(food_group,
                           "Pulses, nuts, and seeds"="Legumes, nuts, seeds"),
         food_group=factor(food_group, levels=food_groups))


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
  h3("Detailed results"),
  plotOutput(outputId = "plot_overall", width=650, height=950)


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
