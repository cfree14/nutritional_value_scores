
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
data_orig <- read.csv(file.path(indir, "FCD_4Mar2024.csv"), na.strings=c("", "N/A"))

# Food id issues to clean up

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  # Arrange
  select(country, food_group, dqq_food_group, food, food_long, usda_fdc_id, dqq_question, everything())

# Inspect
str(data)

# Food key
food_key <- data %>% 
  count(food_group, dqq_food_group, food, food_long, usda_fdc_id, dqq_question)
freeR::which_duplicated(food_key$food)
freeR::which_duplicated(food_key$usda_fdc_id)


# Export data
################################################################################

# Export
saveRDS(data, file.path(outdir, "food_composition_data.Rds"))
