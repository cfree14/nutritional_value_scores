
# Plot overall
# country <- "Indonesia"; data <- scores; score_name <- "Overall"
plot_overall <- function(data, score_name, group_yn, country, base_theme){
  
  # Food groups
  food_group_colors <- c("#5d5766", "#6c9a92", "#e7b123", "#b95547", "#c8875e")

  # Format data
  country_do <- country
  sdata <- data %>% 
    # Reduce to country of interest
    filter(country==country_do) %>% 
    # Reshape to ultimaely reduce to variable of interest
    gather(key="metric", value="score", 5:ncol(.)) %>% 
    # Rename scores
    mutate(metric=gsub("_", " ", metric) %>% stringr::str_to_sentence(.),
           metric=recode(metric,
                         "Vitamin"="1. Vitamin",
                         "Mineral"="2. Mineral",
                         "Eaa"="3. EAA",
                         "Omega3"="4. Omega-3",
                         "Fiber"="5. Fiber",
                         "Calorie density"="6. Calorie density",
                         "Nutrient ratio"="7. Nutrient ratio",
                         "Nutrient density"="Nutrient density")) %>% 
    # Filter to variable of interest
    filter(metric==score_name)
  
  # X-axis title
  x_title <- paste(sub("^\\d+\\.\\s", "", score_name), "score")
  
  # Max x value
  xmax <- max(sdata$score) + 5
  
  # If grouping
  if(group_yn=="No"){
    
    # Plot data
    ggplot(sdata, aes(y=reorder(food, score), x=score, fill=food_group)) +
      geom_bar(stat="identity") +
      geom_text(mapping=aes(label=round(score,0), color=food_group), show.legend = F, hjust=-0.2) +
      # Labels
      labs(x=x_title, y="") +
      scale_x_continuous(sec.axis = dup_axis(), lim=c(NA, xmax)) +
      # Legend
      scale_fill_manual(name="Food group", values=food_group_colors) +
      scale_color_manual(name="Food group", values=food_group_colors) +
      # Theme
      theme_bw() + base_theme
    
  }else{
    
    # Plot data
    ggplot(sdata, aes(y=reorder(food, score), x=score, fill=food_group)) +
      facet_grid(food_group~., space="free_y", scales="free_y", labeller = label_wrap_gen(10) ) +
      geom_bar(stat="identity") +
      geom_text(mapping=aes(label=round(score,0), color=food_group), show.legend = F, hjust=-0.2) +
      # Labels
      labs(x=x_title, y="") +
      scale_x_continuous(sec.axis = dup_axis(), lim=c(NA, xmax)) +
      # Legend
      scale_fill_manual(name="Food group", values=food_group_colors) +
      scale_color_manual(name="Food group", values=food_group_colors) +
      # Theme
      theme_bw() + base_theme 
    
  }
  
}



# Plot boxplots
# country <- "Indonesia"; data <- scores; score_name <- "Overall"
plot_boxplot <- function(data, country, base_theme){
  
  # Food groups
  food_group_colors <- c("#5d5766", "#6c9a92", "#e7b123", "#b95547", "#c8875e")
  
  # Format data
  country_do <- country
  sdata <- data %>% 
    filter(country==country_do)
  sdata_long <- sdata %>% 
    gather(key="metric", value="score", 5:ncol(.)) %>% 
    mutate(metric=recode_factor(metric, 
                                "overall"="Overall",
                                "vitamin"="1. Vitamin",        
                                "mineral"="2. Mineral",         
                                "eaa"="3. EAA",              
                                "omega3"="4. Omega-3 fatty acid",          
                                "fiber"="5. Fiber",
                                "calorie_density"="6. Calorie density",
                                "nutrient_ratio"="7. Nutrient ratio",   
                                "nutrient_density"="Nutrient density"))
  
  # Calculate stats
  stats <- sdata_long %>% 
    filter(metric=="Overall") %>% 
    group_by(dqq_food_group) %>% 
    summarize(score=median(score)) %>% 
    ungroup() %>% 
    arrange(score)
  
  # Order data
  sdata_long_ordered <- sdata_long %>% 
    mutate(dqq_food_group=factor(dqq_food_group, levels=stats$dqq_food_group))
  
  # Plot gata 
  ggplot(sdata_long_ordered, aes(y=dqq_food_group, x=score, fill=food_group)) +
    facet_wrap(~metric, ncol=3, scale="free_x") +
    geom_boxplot() +
    # Labels
    labs(x="Nutritional value score", y="") +
    # Legend
    scale_fill_manual(name="Food group", values=food_group_colors) +
    # Theme
    theme_bw() + base_theme
  
}

