
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
                         "Overall"="NVS",
                         "Vitamin"="1. Vitamins",
                         "Mineral"="2. Minerals",
                         "Eaa"="3. Protein",
                         "Omega3"="4. Omega-3",
                         "Fiber"="5. Fiber",
                         "Calorie density"="6. Calories",
                         "Nutrient ratio"="7. Nutrient ratios",
                         "Nutrient density"="Nutrient density")) %>% 
    # Filter to variable of interest
    filter(metric==score_name)
  
  # X-axis title
  x_title <- recode(score_name, 
                    "NVS"="Nutritional Value Score",
                    "1. Vitamins"="Vitamin score",
                    "2. Minerals"="Mineral score",
                    "3. Protein"="Protein score",
                    "4. Omega-3"="Omega-3 score",
                    "5. Fiber"="Fiber score",
                    "6. Calories"="Calorie score",
                    "7. Nutrient ratios"="Nutrient ratio score",
                    "Nutrient density"="Nutrient Density Score")
  
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
# country <- "Indonesia"; data <- scores; score_name <- "NVS"
plot_boxplot <- function(data, country, score_name, base_theme){
  
  # Food groups
  food_group_colors <- c("#5d5766", "#6c9a92", "#e7b123", "#b95547", "#c8875e")
  
  # Format data
  country_do <- country
  sdata <- data %>% 
    filter(country==country_do)
  sdata_long <- sdata %>% 
    gather(key="metric", value="score", 5:ncol(.)) %>% 
    mutate(metric=recode_factor(metric, 
                                "overall"="NVS",
                                "vitamin"="1. Vitamins",        
                                "mineral"="2. Minerals",         
                                "eaa"="3. Protein",              
                                "omega3"="4. Omega-3",          
                                "fiber"="5. Fiber",
                                "calorie_density"="6. Calories",
                                "nutrient_ratio"="7. Nutrient ratios",   
                                "nutrient_density"="Nutrient density")) %>% 
    filter(metric==score_name)
  
  # Calculate stats
  stats <- sdata_long %>% 
    group_by(food_group, dqq_food_group) %>% 
    summarize(score=mean(score)) %>% 
    ungroup() %>% 
    arrange(score) %>% 
    mutate(dqq_food_group=factor(dqq_food_group, levels=dqq_food_group))
  
  # Order data
  sdata_long_ordered <- sdata_long %>% 
    mutate(dqq_food_group=factor(dqq_food_group, levels=stats$dqq_food_group))
  
  # X-axis title
  x_title <- recode(score_name, 
                    "NVS"="Nutritional Value Score",
                    "1. Vitamins"="Vitamin score",
                    "2. Minerals"="Mineral score",
                    "3. Protein"="Protein score",
                    "4. Omega-3"="Omega-3 score",
                    "5. Fiber"="Fiber score",
                    "6. Calories"="Calorie score",
                    "7. Nutrient ratios"="Nutrient ratio score",
                    "Nutrient density"="Nutrient Density Score")
  
  # Plot data 
  ggplot(sdata_long_ordered, aes(y=dqq_food_group, x=score, fill=food_group)) +
    stat_boxplot(geom = "errorbar", width = 0.4) + 
    geom_boxplot() +
    geom_point(data=stats, mapping=aes(y=dqq_food_group, x=score, fill=food_group), shape=21, size=3) +
    # Labels
    labs(x=x_title, y="") +
    scale_x_continuous(sec.axis = dup_axis()) +
    # Legend
    scale_fill_manual(name="Food group", values=food_group_colors) +
    # Theme
    theme_bw() + base_theme
  
}


# Plot LCA data
# data <- lca_orig; country <- "Indonesia"; unit <- "mPT/kg"; factor <- "Primary production"
plot_lca <- function(data, country, factor, unit, group_yn, base_theme){
  
  # Params
  unit_do <- unit
  country_do <- country
  factor_do <- factor
  
  # Format data
  sdata <- data %>% 
    filter(country==country_do & unit==unit_do & factor==factor_do)
  
  # Food groups
  food_group_colors <- c("#5d5766", "#6c9a92", "#e7b123", "#b95547", "#c8875e")
  
  # Plot data
  xtitle <- paste0("Impact of\n", tolower(factor_do))
  ggplot(sdata, aes(y=reorder(food_lca, value), x=value, fill=food_group)) +
    # facet_grid(food_group~., space="free_y", scale="free_y") +
    geom_bar(stat="identity") +
    # Labels
    labs(x=xtitle, y="") +
    # Legend
    scale_fill_manual(name="Food group", values=food_group_colors) +
    # Theme
    theme_bw() + base_theme
  
  
}














