
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
library(shinyjs) # javascript functionality for shiny
library(shinydashboard) # layout
library(shinyWidgets)
library(rsconnect)

# Directories
datadir <- "data" # for actual app
codedir <- "code"  # for actual app
# datadir <- "shiny_app/data" # when testing
# codedir <- "shiny_app/code" # when testing

# Read data
scores_orig <- readRDS(file.path(datadir, "scores.Rds"))

# Read scripts
sapply(list.files(codedir), function(x) source(file.path(codedir, x)))

# Read intro text
intro_text <- readr::read_file(file=file.path(datadir, "intro_text.txt"))


# Format data
################################################################################

# Food groups
food_groups <- c("Fruits", "Vegetables", "Legumes, nuts, seeds", "Animal-source foods", "Starchy staples")

# Scores
score_names <- c("Overall", "Vitamin", "Mineral", "EAA", "Omega-3", "Fiber", 
                 "Calorie density", "Nutrient ratio", "Nutrient density")

# Format data
scores <- scores_orig %>% 
  # Format food group
  mutate(food_group=recode(food_group,
                           "Pulses, nuts, and seeds"="Legumes, nuts, seeds"),
         food_group=factor(food_group, levels=food_groups)) %>% 
  # Format calorie density
  mutate(calorie_density=abs(calorie_density))

# Countries
countries <- scores$country %>% unique()


# Themes
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=12),
                    axis.title=element_text(size=13),
                    legend.text=element_text(size=12),
                    legend.title=element_text(size=13),
                    strip.text = element_text(size=12),
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
  titlePanel("Nutritional Value Score (NVS) exploration tool"),
  
  # Intro text
  h3("Background"),
  HTML(intro_text),
  br(),

  # Select species
  h3("Data explorer"),
  p("Select a country to begin data exploration."),
  selectInput(inputId = "country", label = "Select a country:",
              choices = countries,  multiple = F, selected=countries[1]),
  br(),
  
  # Broad results
  h4("Broad results"),
  
  # Plot boxplot
  p("The figure below illustrates the distribution of Nutritional Value Scores among foods within different food group. Food groups are sorted in order of descending median Nutritional Value Score. In boxplots, the solid line indicates the median, the box indicates the interquartile range (IQR; 25th to 75th percentiles), the whiskers indicate 1.5 times the IQR, and the points beyond the whiskers indicate outliers. Sub-scores 1-7 contribute to the overall Nutritional Value Score. Sub-scores 1-4 also contribute to the Nutrient Density Score."),
  
  # Score panels
  tabsetPanel(id= "tabs1",
              tabPanel("Overall"),
              tabPanel("1. Vitamin"),
              tabPanel("2. Mineral"),
              tabPanel("3. EAA"),
              tabPanel("4. Omega-3"),
              tabPanel("5. Fiber"),
              tabPanel("6. Calorie density"),
              tabPanel("7. Nutrient ratio"),
              tabPanel("Nutrient density")
  ),
  
  # Plot data
  plotOutput(outputId = "plot_boxplot", width=700, height=500),
  br(),
  
  # Detailed results
  h4("Detailed results"),
  p("The figure below illustrates Nutritional Value Scores of specific foods. Sub-scores 1-7 contribute to the overall Nutritional Value Score. Sub-scores 1-4 contribute to the Nutrient Density Score."),
  br(),
  
  # Group?
  shinyWidgets::radioGroupButtons(inputId="group_yn", label="Group results by food group?", choices=c("Yes", "No"), selected="Yes"), 
  
  # Score panels
  tabsetPanel(id= "tabs2",
              tabPanel("Overall"),
              tabPanel("1. Vitamin"),
              tabPanel("2. Mineral"),
              tabPanel("3. EAA"),
              tabPanel("4. Omega-3"),
              tabPanel("5. Fiber"),
              tabPanel("6. Calorie density"),
              tabPanel("7. Nutrient ratio"),
              tabPanel("Nutrient density")
  ),

  # Plot scores
  plotOutput(outputId = "plot_overall", width=650, height=1100),
  
  # Citation
  h3("Citation"),
  p("Please cite this Shiny app and its results using the following paper:"),
  HTML('<p><span style="font-weight: 400;">Beal T, Ortenzi F (</span><em><span style="font-weight: 400;">in review</span></em><span style="font-weight: 400;">) Nutritional Value Score rates foods based on global health priorities. Available at: </span><a href="https://www.researchsquare.com/article/rs-3443927/v1"><span style="font-weight: 400;">https://www.researchsquare.com/article/rs-3443927/v1</span> </a></p>'),
  br(),
  br(),
  br()
  


)


# Server
################################################################################

# Server
server <- function(input, output, session){
  
  # Plot data
  output$plot_boxplot <- renderPlot({
    g <- plot_boxplot(data = scores,
                      country=input$country,
                      score=input$tabs1,
                      base_theme=base_theme)
    g
  })
  
  # Plot data
  output$plot_overall <- renderPlot({
    g <- plot_overall(data = scores,
                      score = input$tabs2,
                      group = input$group_yn,
                      country=input$country,
                      base_theme=base_theme)
    g
  })

}

shinyApp(ui = ui, server = server)
