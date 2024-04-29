
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

life_stages <- c("Primary production", "Processing", "Packaging", "Distribution", 
                 "Retail", "User stage", "Water treatment", "Climate change")

# Format data
data <- data_orig %>% 
  filter(category!="Other" & !grepl("Total", factor)) %>% 
  filter(unit=="mPT/100 NVS" & category=="Life stage") %>% 
  filter(country=="Indonesia") %>% 
  # Order food groups
  mutate(food_group=recode(food_group,
                           "Animal-source foods"="Animal-source\nfoods",
                           "Starchy staples"="Starchy\nstaples",
                           "Pulses, nuts, and seeds"="Pulses,\nnuts, seeds")) %>% 
  # Order life stages
  mutate(factor=factor(factor, levels=life_stages)) %>% 
  # Calculate prop
  group_by(food_lca) %>% 
  mutate(prop=value/sum(value)) %>% 
  ungroup()

# Order
stats <- data %>% 
  group_by(food_group, food_lca) %>% 
  summarise(value_sum=sum(value)) %>% 
  ungroup() %>% 
  arrange(food_group, desc(value_sum))

# ORder data
data_ordered <- data %>% 
  mutate(food_lca=factor(food_lca, levels=stats$food_lca))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=8),
                   plot.tag = element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.5, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(data_ordered, aes(y=food_lca, x=value, fill=factor)) +
  # Facet
  facet_grid(food_group~., scales="free_y", space="free_y") +
  # Data
  geom_bar(stat="identity", color="grey30", linewidth=0.2) +
  # Labels
  labs(x="Environmental impact (mPT/100 NVS)", y="", tag="A") +
  # Legend
  scale_fill_discrete(name="Life stage") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g1

# Plot data
g2 <- ggplot(data_ordered, aes(y=food_lca, x=prop, fill=factor)) +
  # Facet
  facet_grid(food_group~., scales="free_y", space="free_y") +
  # Data
  geom_bar(stat="identity", color="grey30", linewidth=0.2) +
  # Labels
  labs(x="% of overall environmental impact", y="", tag="B") +
  scale_x_continuous(labels=scales::percent) +
  # Legend
  scale_fill_discrete(name="Life stage") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank())
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, widths=c(0.46, 0.54), ncol=2)
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_lca_by_food_overall_props.png"), 
       width=6.5, height=6.5, units="in", dpi=600)



