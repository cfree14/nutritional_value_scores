
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/raw"
outdir <- "data/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file.path(outdir, "envi_impact_data.Rds"))


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Filter
  filter(category!="Other" & !grepl("Total", factor)) %>% 
  # Order food groups
  mutate(food_group=recode(food_group,
                           "Animal-source foods"="Animal-source\nfoods",
                           "Starchy staples"="Starchy\nstaples",
                           "Pulses, nuts, and seeds"="Pulses,\nnuts, seeds")) %>% 
  # Order categories
  mutate(category=factor(category, levels=c("Life stage", "Impact category")))

# Build key
key <- data %>% 
  select(country, unit) %>% 
  unique()

# Loop through countries and metrics
################################################################################

# Loop
for(i in 1:nrow(key)){
  
  # Params
  cntry_do <- key$country[i]
  unit_do <- key$unit[i]
  
  
  # Subset data
  sdata <- data %>% 
    filter(country==cntry_do & unit==unit_do)
  
  # Food order
  stats_food <- sdata %>% 
    group_by(food_group, food_lca) %>% 
    summarize(value_sum=sum(value)) %>% 
    ungroup() %>% 
    arrange(food_group, desc(value_sum))
  
  # Factor order
  stats_factor <- sdata %>% 
    group_by(category, factor) %>% 
    summarize(value_sum=sum(value)) %>% 
    ungroup %>% 
    arrange(category, desc(value_sum))
  
  # Order data
  data_ordered <- sdata %>% 
    mutate(food_lca=factor(food_lca, levels=stats_food$food_lca)) %>% 
    mutate(factor=factor(factor, levels=stats_factor$factor))
  
  # Overwrite zeros/negatives
  data_ordered1 <- data_ordered %>% 
    mutate(value=ifelse(value<=0, NA, value))
  
  # Plot data
  ################################################################################
  
  # Setup theme
  my_theme <-  theme(axis.text=element_text(size=6),
                     axis.title=element_text(size=7),
                     legend.text=element_text(size=6),
                     legend.title=element_text(size=7),
                     strip.text=element_text(size=7),
                     plot.title=element_blank(),
                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key.size = unit(0.5, "cm"),
                     legend.background = element_rect(fill=alpha('blue', 0)))
  

  # Plopt data
  legend_title <- paste0("Environmental impact\n(", unit_do, ")")
  g <- ggplot(data_ordered1, 
              aes(y=food_lca, x=factor, fill=value)) +
    facet_grid(food_group~category, scales="free", space="free") +
    # Data
    geom_tile() +
    # Labels
    labs(x="", y="") +
    # Legend
    scale_fill_gradientn(name=legend_title,
                         colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                         na.value="white",
                         trans="log2",
                         breaks=c(0.0001, 0.001, 0.01, 0.1, 1),
                         labels=c("0.0001", "0.001", "0.01", "0.1", "1")) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() + my_theme
  g
  
  # Export figure
  plottitle <- paste0("FigX_lca_by_food_", tolower(cntry_do), "_", ifelse(unit_do=="mPT/kg", "kg", "nvs"), ".png")
  ggsave(g, filename=file.path(plotdir, plottitle), 
         width=4.5, height=7, units="in", dpi=600)
  
}

# # Plopt data
# g <- ggplot(data_ordered, 
#             aes(y=food_lca, x=factor, fill=value)) +
#   facet_grid(food_group~category, scales="free", space="free") +
#   # Data
#   geom_tile() +
#   # Labels
#   labs(x="", y="") +
#   # Legend
#   scale_fill_gradientn("Environmental impact\n(mPT/100 NVS)", 
#                        colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
#   guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
#   # Theme
#   theme_bw() + my_theme
# 
# 
# # Export figure
# ggsave(g, filename=file.path(plotdir, "FigX_lca_by_food_raw_indonesia.png"), 
#        width=4.5, height=7, units="in", dpi=600)

