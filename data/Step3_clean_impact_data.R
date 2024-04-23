
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
data_orig <- readxl::read_excel(file.path(indir, "LCA results per kg & NVS.xlsx"))
data_key <- readxl::read_excel(file.path(indir, "LCA results per kg & NVS.xlsx"), sheet=2) 

# Read food key
food_key <- readRDS(file.path(outdir, "food_key.Rds"))
  

# Format data
################################################################################

# Get med, hi, lo working
# Food probably needs to be formatted for this to work

# Format data
data <- data_orig %>% 
  # Gather
  gather(key="metric", value="value", 8:ncol(.)) %>% 
  # Add variable metrics
  left_join(data_key, by="metric") %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(food_lca=food_lc_aname,
         food_nvs=food_nv_sname) %>% 
  # Spread
  # select(-metric)
  # spread(key="quantile", value="value") %>% 
  # rename(value="50",
  #        value_lo="2.5",
  #        value_hi="97.5") %>% 
  # Arrange
  select(country,
         food_group, dqq_food_group, dqq_question,
         food_lca, food_nvs, food_long, 
         metric,
         category, factor, unit, 
         quantile, value,
         #value, value_lo, value_hi,
         everything()) %>% 
  arrange(country, food_group, dqq_food_group, food_lca)

# Inspect
table(data$country)

# Inspect food


# Inspect
str(data)
freeR::complete(data)


# Export data
################################################################################

# Export
saveRDS(data, file.path(outdir, "envi_impact_data.Rds"))
