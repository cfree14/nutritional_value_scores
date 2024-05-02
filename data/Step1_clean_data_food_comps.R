
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
nigeria_orig <- read.csv(file.path(indir, "FCD_Nigeria_12Apr2024.csv"), na.strings=c("", "N/A"))

# Inspect
str(data_orig)
str(nigeria_orig)
colnames(nigeria_orig)[!colnames(nigeria_orig) %in% colnames(data_orig)]


# Food id issues to clean up

# Format data
################################################################################

# Format data
data <- bind_rows(data_orig) %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  # Format long foods
  mutate(food_long=recode(food_long,
                          "Lean fish, average of various species consumed in Indonesia and of different cooking methods"="Lean fish, average of various species consumed and of different cooking methods",
                          "Lean fish, average of various species consumed in Bangladesh, and of different cooking methods"="Lean fish, average of various species consumed and of different cooking methods",
                          "Oily fish, average of various species consumed in Bangladesh and of different cooking methods"="Oily fish, average of various species consumed and of different cooking methods",
                          "Oily fish, average of various species consumed in Indonesia and of different cooking methods"="Oily fish, average of various species consumed and of different cooking methods")) %>% 
  # Format foods
  mutate(food=case_when(food_long=="Chayote, average of raw and cooked (boied)" ~ "Green pepper (chayote)",
                        T ~ food)) %>% 
  # Arrange
  select(country, food_group, dqq_food_group, food, food_long, usda_fdc_id, dqq_question, everything())

# Inspect
str(data)

# Food key
food_key <- data %>% 
  count(food_group, dqq_food_group, food, food_long, usda_fdc_id) #dqq_question)
freeR::which_duplicated(food_key$food)
freeR::which_duplicated(food_key$usda_fdc_id)

# Export food key
saveRDS(food_key, file=file.path(outdir, "food_key.Rds"))


# Export data
################################################################################

# Export
saveRDS(data, file.path(outdir, "food_composition_data.Rds"))
