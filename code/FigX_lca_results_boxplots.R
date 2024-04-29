
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

# Food groups
food_groups <- c("Fruits", "Vegetables", "Legumes, nuts, seeds", "Animal-source foods", "Starchy staples")

# Food groups
food_group_colors <- c("#5d5766", "#6c9a92", "#e7b123", "#b95547", "#c8875e")

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  filter(category=="Life stage" & factor=="Total life cycle") %>% 
  filter(country=="Indonesia") %>% 
  # Format food groups
  mutate(food_group=recode(food_group,
                           "Pulses, nuts, and seeds"="Legumes, nuts, seeds"),
         food_group=factor(food_group, levels=food_groups))

# Subset data
data1 <- data %>% 
  filter(unit=="mPT/kg")
data2 <- data %>% 
  filter(unit=="mPT/100 NVS")

# Order
stats1 <- data1 %>% 
  group_by(food_group, dqq_food_group) %>% 
  summarize(value=mean(value)) %>% 
  ungroup() %>% 
  arrange(desc(value))
stats2 <- data2 %>% 
  group_by(food_group, dqq_food_group) %>% 
  summarize(value=mean(value)) %>% 
  ungroup() %>% 
  arrange(desc(value))

# Order data
data1_ordered <- data1 %>% 
  mutate(dqq_food_group=factor(dqq_food_group, levels=stats1$dqq_food_group))
data2_ordered <- data2 %>% 
  mutate(dqq_food_group=factor(dqq_food_group, levels=stats2$dqq_food_group))


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
g1 <- ggplot(data1_ordered, aes(y=dqq_food_group, x=value, fill=food_group)) +
  geom_boxplot() +
  geom_point(data=stats1, mapping=aes(y=dqq_food_group, x=value, fill=food_group), pch=21, size=2) +
  # Labels
  labs(x="Overall impact (mPT/kg)", y="", tag="A") +
  # Legend
  scale_fill_manual(name="Food group", values=food_group_colors) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.8, 0.75))
g1 

# Plot data
g2 <- ggplot(data2_ordered, aes(y=dqq_food_group, x=value, fill=food_group)) +
  geom_boxplot() +
  geom_point(data=stats2, mapping=aes(y=dqq_food_group, x=value, fill=food_group), pch=21, size=2) +
  # Labels
  labs(x="Overall impact (mPT/100 NVS)", y="", tag="B") +
  # Legend
  scale_fill_manual(name="Food group", values=food_group_colors) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, ncol=1)
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_lca_results_botxplots.png"), 
       width=6.5, height=6.5, units="in", dpi=600)



