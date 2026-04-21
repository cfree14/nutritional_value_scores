
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)


# Directories
indir <- "data/raw/2026_04_21_data"
outdir <- "data/processed"

# Read data
# data_orig <- readxl::read_excel(file.path(indir, "NVS_4Mar2024.xlsx"))
# data_orig <- readxl::read_excel(file.path(indir, "NVS_Nigeria_12Apr2024.xlsx"))
data_orig <- readxl::read_excel(file.path(indir, "NVS_OutputData.xlsx"))


# Read food key
food_key <- readRDS(file.path(outdir, "food_key_new.Rds"))



# Format data
################################################################################

# Format original data
data <- data_orig %>% 
  # Simplify
  select(c("Food",	"Country",	"V",	"M",	"N3",	
         "Protein",	"F",	"NR",	"EMR",	"NDS",	
         "NVS")) %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(vitamin=v,
         mineral=m,
         omega3=n3,
         eaa=protein,
         fiber=f,
         nutrient_ratio=nr,
         calorie_density=emr,
         nutrient_density=nds,
         overall=nvs) %>% 
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
saveRDS(data, file.path(outdir, "scores_new.Rds"))

