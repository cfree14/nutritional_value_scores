
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
lca_orig <- readRDS(file.path(datadir, "envi_impact_data.Rds"))

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

# Format scores
scores <- scores_orig %>% 
  # Format food group
  mutate(food_group=recode(food_group,
                           "Pulses, nuts, and seeds"="Legumes, nuts, seeds"),
         food_group=factor(food_group, levels=food_groups)) %>% 
  # Format calorie density
  mutate(calorie_density=abs(calorie_density))

# Format LCA
lca <- lca_orig %>% 
  # Format food group
  mutate(food_group=recode(food_group,
                           "Pulses, nuts, and seeds"="Legumes, nuts, seeds"),
         food_group=factor(food_group, levels=food_groups)) %>% 
  # Format factor
  mutate(factor=recode(factor,
                       "Water treatment"="Waste treatment"))

# Countries
countries1 <- scores$country %>% unique() %>% sort()
countries2 <- lca$country %>% unique() %>% sort()

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
ui <- navbarPage("Nutritional Value Score & Life Cycle Assessment Explorer",
                 
  # Nutritional value tab
  ##############################################################################
  
  # Nutritional value
  tabPanel("Nutritional value",
  
    # Intro text
    h3("Background"),
    HTML(intro_text),
    br(),
  
    # Section header
    h3("Data explorer"),
    
    # Section header
    h4("Scores by food group"),
    
    # Plot boxplot
    p("The figure below illustrates the distribution of Nutritional Value Scores and the seven sub-scores among foods within different recommended food groups in the Diet Quality Questionnaire. Food groups are sorted in order of descending mean scores. In boxplots, the solid line indicates the median, the box indicates the interquartile range (IQR; 25th to 75th percentiles), the whiskers indicate 1.5 times the IQR, and the points beyond the whiskers indicate outliers. The large central point indicates the mean value. Sub-scores 1-7 contribute to the overall Nutritional Value Score."),
    
    # Select country
    # selectInput(inputId = "country1", label = "Select a country:",
    #             choices = countries,  multiple = F, selected=countries[1]),
    shinyWidgets::pickerInput(inputId="country1", label="Select a country or countries:", 
                              choices=countries1, multiple = T, selected=countries1[1],
                              options = list(`actions-box` = TRUE)),

    br(),
    
    # Score panels
    tabsetPanel(id= "tabs1",
                tabPanel("NVS"),
                tabPanel("1. Vitamins"),
                tabPanel("2. Minerals"),
                tabPanel("3. Protein"),
                tabPanel("4. Omega-3"),
                tabPanel("5. Fiber"),
                tabPanel("6. Calories"),
                tabPanel("7. Nutrient ratios")
    ),
    
    # Plot data
    plotOutput(outputId = "plot_nvs_boxplots", width=700, height=500),
    br(),
    
    # Detailed results
    h4("Scores by individual foods"),
    p("The figure below illustrates Nutritional Value Scores of unprocessed, minimally and moderately processed foods included in country-adapted Diet Quality Questionnaires (DQQ). Sub-scores 1-7 contribute to the overall Nutritional Value Score."),
    p("The reason for selecting foods from Diet Quality Questionnaires is to ensure relevance to the local context, as DQQ foods are commonly consumed by a large proportion of households locally. In addition, we have chosen not to prioritize analyzing ultra-processed foods as these are usually not recommended in dietary guidelines globally, and the purpose of the NVS is to inform policy and programmatic decisions around nutritious foods to promote or invest in. However, future versions of the NVS may be expanded to allow for scoring ultra-processed foods and culinary ingredients."),
    br(),
    
    # Select country
    selectInput(inputId = "country2", label = "Select a country:",
                choices = countries1,  multiple = F, selected=countries1[1]),
    br(),
    
    # Group?
    shinyWidgets::radioGroupButtons(inputId="group_yn1", label="Group results by food group?", choices=c("Yes", "No"), selected="Yes"), 
    
    # Score panels
    tabsetPanel(id= "tabs2",
                tabPanel("NVS"),
                tabPanel("1. Vitamins"),
                tabPanel("2. Minerals"),
                tabPanel("3. Protein"),
                tabPanel("4. Omega-3"),
                tabPanel("5. Fiber"),
                tabPanel("6. Calories"),
                tabPanel("7. Nutrient ratios")
    ),
  
    # Plot scores
    plotOutput(outputId = "plot_nvs_barplots", width=650, height=1100),
    
    # Citation
    h3("Citation"),
    p("Please cite this Shiny app and its results using the following paper:"),
    HTML('<p><span style="font-weight: 400;">Beal T, Ortenzi F (</span><em><span style="font-weight: 400;">in review</span></em><span style="font-weight: 400;">) Nutritional Value Score rates foods based on global health priorities. Available at: </span><a href="https://www.researchsquare.com/article/rs-3443927/v1"><span style="font-weight: 400;">https://www.researchsquare.com/article/rs-3443927/v1</span> </a></p>'),
    br(),
    br(),
    br()
    
  ),
  
  # Environmental impact tab
  ##############################################################################
  
  # Environmental impact
  tabPanel("Environmental impact",
           
     # Intro text
     h3("Background"),
     p("Background text coming soon."),
     br(),
           
     # Select country
     h3("Data explorer"),
     selectInput(inputId = "country3", label = "Select a country:",
                 choices = countries2,  multiple = F, selected=countries2[1]),
     br(),
     
     # Life stage
     h4("Environmental impacts"),
     p("The figure below illustrates the environmental impacts associated with the production and consumption of different foods included in country-adapted Diet Quality Questionnaires (DQQ), by life cycle stage (right panel) and impact category (left panel). Foods are listed in order of increasing average environmental impact within each food group. Impacts are measured in both milliPoints per kilogram (mPT/kg) and per a Nutritional Value Score of 100 (mPT/100 NVS). Cell color indicates the intensity of environmental impacts and is illustrated using a log-scale. White cells indicate insignificant environmental impact."),
     shinyWidgets::radioGroupButtons(inputId="units_lca", label="Units?", choices=c("mPT/kg", "mPT/100 NVS"), selected="mPT/kg"), 
     plotOutput(outputId = "plot_lca_rasters", width=700, height=1100),
           
    # Life stage
    h4("Impact by category"),
    
    # Broad results figure
    p("The figure below illustrates the overall environmental impact and the impact by category of different food groups included in country-adapted Diet Quality Questionnaires (DQQ). Impacts are measured in both milliPoints per kilogram (mPT/kg) and per a Nutritional Value Score of 100 (mPT/100 NVS). Food groups are sorted in order of increasing mean environmental impact. The overall impact is the sum of the impacts of all individual categories. In boxplots, the solid line indicates the median, the box indicates the interquartile range (IQR; 25th to 75th percentiles), the whiskers indicate 1.5 times the IQR, and the points beyond the whiskers indicate outliers. The large central point indicates the mean value."),

    # Category panels
    tabsetPanel(id= "tabs3",
                tabPanel("Overall"),
                tabPanel("Climate change"),
                tabPanel("Acidification"),
                tabPanel("Particulate matter"),
                tabPanel("Eutrophication"),
                tabPanel("Land use"),
                tabPanel("Fossil fuel use"),
                tabPanel("Water use"),
                tabPanel("Other")
    ),
    
    # Plot LCA by category - broad
    plotOutput(outputId = "plot_lca_boxplots_catg", width=700, height=800),
    
    # Detailed results figure
    p("The figure below illustrates the overall environmental impact and the impact by category of different foods included in country-adapted Diet Quality Questionnaires (DQQ). Impacts are measured in both milliPoints per kilogram (mPT/kg) and per a Nutritional Value Score of 100 (mPT/100 NVS). Foods are sorted in order of decreasing environmental impacts. The overall impact is the sum of the impacts of all individual categories. Lines indicate the 95% confidence interval."),
    
    # Group results?
    shinyWidgets::radioGroupButtons(inputId="group_yn2", label="Group results by food group?", choices=c("Yes", "No"), selected="Yes"), 
    
    # Category panels
    tabsetPanel(id= "tabs4",
                tabPanel("Overall"),
                tabPanel("Climate change"),
                tabPanel("Acidification"),
                tabPanel("Particulate matter"),
                tabPanel("Eutrophication"),
                tabPanel("Land use"),
                tabPanel("Fossil fuel use"),
                tabPanel("Water use"),
                tabPanel("Other")
    ),
    
    # Plot LCA by category - detailed
    plotOutput(outputId = "plot_lca_barplots_catg", width=1100, height=1100),
           
    # Life stage
    h4("Impact by life stage"),
    
    # Broad figure
    p("The figure below illustrates the overall environmental impact and the impact by life cycle stage of different food groups included in country-adapted Diet Quality Questionnaires (DQQ). Impacts are measured in both milliPoints per kilogram (mPT/kg) and per a Nutritional Value Score of 100 (mPT/100 NVS). Food groups are sorted in order of increasing mean environmental impact. The overall impact is the sum of the impacts associated with all life cycle stages. In boxplots, the solid line indicates the median, the box indicates the interquartile range (IQR; 25th to 75th percentiles), the whiskers indicate 1.5 times the IQR, and the points beyond the whiskers indicate outliers. The large central point indicates the mean value."),
    
    # Life stage panels
    tabsetPanel(id= "tabs5",
                tabPanel("Overall"),
                tabPanel("Primary production"),
                tabPanel("Processing"),
                tabPanel("Packaging"),
                tabPanel("Distribution"),
                tabPanel("Retail"),
                tabPanel("User stage"),
                tabPanel("Waste treatment")
    ),
    
    # Plot LCA by life stage - broad
    plotOutput(outputId = "plot_lca_boxplots_stage", width=700, height=800),
    
    # Detailed results figure
    p("The figure below illustrates the overall environmental impact and the impact by life cycle stage of different foods included in country-adapted Diet Quality Questionnaires (DQQ). Impacts are measured in both milliPoints per kilogram (mPT/kg) and per a Nutritional Value Score of 100 (mPT/100 NVS). Foods are sorted in order of decreasing environmental impacts. The overall impact is the sum of the impacts associated with all life stages. Lines indicate the 95% confidence interval."),
    
    # Group results?
    shinyWidgets::radioGroupButtons(inputId="group_yn3", label="Group results by food group?", choices=c("Yes", "No"), selected="Yes"), 
    
    # Life stage panels
    tabsetPanel(id= "tabs6",
                tabPanel("Overall"),
                tabPanel("Primary production"),
                tabPanel("Processing"),
                tabPanel("Packaging"),
                tabPanel("Distribution"),
                tabPanel("Retail"),
                tabPanel("User stage"),
                tabPanel("Waste treatment")
    ),
    
    # Plot LCA by life stage- detailed
    plotOutput(outputId = "plot_lca_barplots_stage", width=1100, height=1100),
    
    # Citation
    h3("Citation"),
    p("Please cite this Shiny app and its results using the following paper:"),
    HTML('<p><span style="font-weight: 400;">Beal T, Ortenzi F (</span><em><span style="font-weight: 400;">in review</span></em><span style="font-weight: 400;">) Nutritional Value Score rates foods based on global health priorities. Available at: </span><a href="https://www.researchsquare.com/article/rs-3443927/v1"><span style="font-weight: 400;">https://www.researchsquare.com/article/rs-3443927/v1</span> </a></p>'),
    br(),
    br(),
    br()
    
  )
  
)


# Server
################################################################################

# Server
server <- function(input, output, session){
  
  # Nutritional value
  ###################################
  
  # Plot NVS - broad
  output$plot_nvs_boxplots <- renderPlot({
    g <- plot_nvs_boxplots(data = scores,
                      country=input$country1,
                      score=input$tabs1,
                      base_theme=base_theme)
    g
  })
  
  # Plot NVS - detailed
  output$plot_nvs_barplots <- renderPlot({
    g <- plot_nvs_barplots(data = scores,
                      score = input$tabs2,
                      group = input$group_yn1,
                      country=input$country2,
                      base_theme=base_theme)
    g
  })
  
  # Environmental impact
  ###################################
  
  # Plot LCA rasters
  output$plot_lca_rasters <- renderPlot({
    g <- plot_lca_rasters(data = lca,
                          country=input$country3,
                          unit=input$units_lca, 
                          base_theme=base_theme)
    g
  })
  
  # Plot LCA by category - broad
  output$plot_lca_boxplots_catg <- renderPlot({
    g <- plot_lca_boxplots(data = lca,
                          country=input$country3,
                          type="catg",
                          factor=input$tabs3,
                          base_theme=base_theme)
    g
  })
  
  # Plot LCA by category - detailed
  output$plot_lca_barplots_catg <- renderPlot({
    g <- plot_lca_barplots(data = lca,
                          country=input$country3,
                          type="catg",
                          factor=input$tabs4,
                          group = input$group_yn2,
                          base_theme=base_theme)
    g
  })
  
  # Plot LCA by life stage - broad
  output$plot_lca_boxplots_stage <- renderPlot({
    g <- plot_lca_boxplots(data = lca,
                          country=input$country3,
                          type="stage",
                          factor=input$tabs5,
                          base_theme=base_theme)
    g
  })
  
  # Plot LCA by life stage - detailed
  output$plot_lca_barplots_stage <- renderPlot({
    g <- plot_lca_barplots(data = lca,
                            country=input$country3,
                            type="stage",
                            factor=input$tabs6,
                            group = input$group_yn3,
                            base_theme=base_theme)
    g
  })

}

shinyApp(ui = ui, server = server)
