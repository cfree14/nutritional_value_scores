
# Plot data
# country <- "Indonesia"; scores <- data
plot_overall <- function(data, country){
  
  # Food groups
  food_group_colors <- c("#5d5766", "#6c9a92", "#e7b123", "#b95547", "#c8875e")

  # Format data
  country_do <- country
  sdata <- scores %>% 
    filter(country==country_do)
    
  
  # Base theme
  base_theme <- theme(axis.text=element_text(size=12),
                      axis.title=element_text(size=13),
                      legend.text=element_text(size=12),
                      legend.title=element_text(size=13),
                      # Gridlines
                      panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(), 
                      axis.line = element_line(colour = "black"),
                      # Legend
                      legend.background = element_rect(fill=alpha('blue', 0)))
  
  # Plot data
  ggplot(sdata, aes(y=reorder(food, overall), x=overall, fill=food_group)) +
    geom_bar(stat="identity") +
    # Labels
    labs(x="Nutritional value score", y="") +
    # Legend
    scale_fill_manual(name="Food group", values=food_group_colors) +
    # Theme
    theme_bw() + base_theme #+
    # theme(legend.position = c(0.7, 0.05),
    #       legend.key.size = unit(0.3, "cm"))

}


plot_boxplot <- function(data, country){
  
  # Format data
  country_do <- country
  sdata <- scores %>% 
    filter(country==country_do)
  sdata_long <- sdata %>% 
    gather(key="metric", value="score", 5:ncol(.)) %>% 
    mutate(metric=recode_factor(metric, 
                                "overall"="Overall",
                                "vitmain"="Vitamin",        
                                "mineral"="Mineral",         
                                "eaa"="EAA",              
                                "omega3"="Omega-3 fatty acid",          
                                "fiber"="Fiber",            
                                "nutrient_ratio"="Nutrient ratio",   
                                "calorie_density"="Calorie density",  
                                "nutrient_density"="Nutrient density"))
  
  # Calculate stats
  stats <- sdata_long %>% 
    filter(metric=="Overall") %>% 
    group_by(food_group) %>% 
    summarize(score=median(score)) %>% 
    ungroup() %>% 
    arrange(score)
  
  # Order data
  sdata_long_ordered <- sdata_long %>% 
    mutate(food_group=factor(food_group, levels=stats$food_group))
  
  # Plot gata 
  ggplot(sdata_long_ordered, aes(y=food_group, x=score, fill=food_group)) +
    facet_wrap(~metric, ncol=3, scale="free_x") +
    geom_boxplot() +
    # Labels
    labs(x="Score", y="") +
    # Theme
    theme_bw() +
    theme(legend.position="none")
  
}
  


