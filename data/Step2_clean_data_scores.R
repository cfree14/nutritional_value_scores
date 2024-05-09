
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)


# Directories
indir <- "data/raw"
outdir <- "data/processed"

# Read data
# data_orig <- readxl::read_excel(file.path(indir, "NVS_4Mar2024.xlsx"))
data_orig <- readxl::read_excel(file.path(indir, "NVS_Nigeria_12Apr2024.xlsx"))

# Read food key
food_key <- readRDS(file.path(outdir, "food_key.Rds"))
  

# Format data
################################################################################

# Format original data
data <- data_orig %>% 
  # Simplify
  select("Food",	"Country",	"Vitamin score",	"Mineral Score",	"Omega-3 fat score",	
         "EAA score",	"Fiber score",	"Nutrient ratio score",	"Calorie density score",	"Nutrient density score",	
         "Nutritional Value Score") %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(vitamin=vitamin_score,
         mineral=mineral_score,
         omega3=omega_3_fat_score,
         eaa=eaa_score,
         fiber=fiber_score,
         nutrient_ratio=nutrient_ratio_score,
         calorie_density=calorie_density_score,
         nutrient_density=nutrient_density_score,
         overall=nutritional_value_score) %>% 
  # Add food groups
  left_join(food_key %>% select(food, food_group, dqq_food_group), by="food") %>% 
  # Arrange
  select(country, food_group, dqq_food_group, food, everything()) %>% 
  arrange(country, food_group, dqq_food_group, food)

# Inspect
str(data)
freeR::complete(data)


# Export data
################################################################################

# Export
saveRDS(data, file.path(outdir, "scores.Rds"))

