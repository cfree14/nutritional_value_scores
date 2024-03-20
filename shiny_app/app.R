
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
scores_orig <- readRDS(file.path(datadir, "scores.Rds"))

# Read scripts
sapply(list.files(codedir), function(x) source(file.path(codedir, x)))


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

# Countries
countries <- scores$country %>% unique()


# Themes
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=12),
                    axis.title=element_text(size=13),
                    legend.text=element_text(size=12),
                    legend.title=element_text(size=13),
                    strip.text = element_text(size=13),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.background = element_rect(fill=alpha('blue', 0)))


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
  
  # Broad results
  h3("Broad results"),
  
  # Detailed results
  h3("Detailed results"),

  # Plot historical comparison
  HTML("<b>Figure 1</b>. Nutritional value score of foods. Foods are sorted in order of decreasing nutritional value score. Color indicates the broader food group."),
  plotOutput(outputId = "plot_overall", width=650, height=950),
  br(),
  br(),
  
  # Plot boxplot
  HTML("<b>Figure 2</b>. Distribution of overall nutritional value score and contributing subscores by food group. Food groups are sorted in order of descending median overall nutritional value score. In boxplots, the solid line indicates the median, the box indicates the interquartile range (IQR; 25th to 75th percentiles), the whiskers indicate 1.5 times the IQR, and the points beyond the whiskers indicate outliers."),
  plotOutput(outputId = "plot_boxplot", width=700, height=450)


)


# Server
################################################################################

# Server
server <- function(input, output, session){

  # Plot data
  output$plot_overall <- renderPlot({
    g <- plot_overall(data = scores,
                      country=input$country,
                      base_theme=base_theme)
    g
  })
  
  # Plot data
  output$plot_boxplot <- renderPlot({
    g <- plot_boxplot(data = scores,
                      country=input$country,
                      base_theme=base_theme)
    g
  })

}

shinyApp(ui = ui, server = server)
