
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
data_orig <- readxl::read_excel(file.path(indir, "NVS_4Mar2024.xlsx"))

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(vitmain=vitamin_score,
         mineral=mineral_score,
         eaa=eaa_score,
         omega3=omega_3_fat_score,
         fiber=fiber_score,
         nutrient_ratio=nutrient_ratio_score,
         calorie_density=calorie_density_score,
         nutrient_density=nutrient_density_score,
         overall=nutritional_value_score) %>% 
  # Arrange
  select(country, food, everything()) %>% 
  arrange(country, food)

# Inspect
str(data)


# Export data
################################################################################

# Export
saveRDS(data, file.path(outdir, "scores.Rds"))

