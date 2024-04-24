
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
  # Must remove metric
  select(-metric) %>%
  spread(key="quantile", value="value") %>%
  rename(value="50",
         value_lo="2.5",
         value_hi="97.5") %>%
  # Arrange
  select(country,
         food_group, dqq_question, dqq_food_group, 
         food_lca, food_nvs, food_long, 
         category, factor, unit, 
         value, value_lo, value_hi,
         everything()) %>% 
  arrange(country, food_group, dqq_food_group, dqq_question, food_lca)

# Inspect
table(data$country)

# Inspect food
food_key <- data %>% 
  count(food_group, dqq_food_group, food_long, food_lca, food_nvs)
freeR::which_duplicated(food_key$food_long)

# Inspect
str(data)
freeR::complete(data)


# # Format foods
# mutate(food_long=case_when(country=="Bangladesh" & food_long=="Lean fish, average of various species consumed in Indonesia and of different cooking methods" ~ "Lean fish, average of various species consumed in Bangladesh and of different cooking methods",
#                            food_lca=="Peanuts (raw/oil-roasted)" ~ "Peanuts, average of raw, oil-roasted",
#                            food_lca=="Fatty fish (tuna)" ~ "Oily tuna, average of various species consumed in Indonesia and of different cooking methods",
#                            food_lca=="Egg, omelet" ~ "Egg, average of various cooking methods (omelet)",
#                            food_lca=="Whole goat milk" ~ "Milk, goat, fluid, whole, unenriched", 
#                            food_lca=="Whole sheep milk" ~ "Milk, sheep, fluid, whole, unenriched", 
#                              T ~ food_long)) %>% 
# mutate(dqq_food_group=ifelse(food_long=="Melons, cantaloupe, raw", "Vitamin A-rich fruits", dqq_food_group)) %>% 

# Export data
################################################################################

# Export
saveRDS(data, file.path(outdir, "envi_impact_data.Rds"))

