
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
# data <- lca_orig; country <- "Indonesia"; type <- "stage"; unit <- "mPT/kg"; factor <- "Overall"; group_yn <- "Yes"
plot_lca <- function(data, country, type, factor, unit, group_yn, base_theme){
  
  # Params
  country_do <- country
  factor_do <- factor
  
  # Factor
  if(factor_do=="Overall"){
    factor_do <-ifelse(type=="catg", "Total category impact", "Total life cycle")
  }
  
  # Format data
  sdata_kg <- data %>% 
    filter(country==country_do & factor==factor_do & unit=="mPT/kg")
  sdata_nvs <- data %>% 
    filter(country==country_do & factor==factor_do & unit=="mPT/100 NVS")
  
  # Food groups
  food_group_colors <- c("#5d5766", "#6c9a92", "#e7b123", "#b95547", "#c8875e")
  
  # Plot data
  xtitle1 <- ifelse(factor=="Overall",
                    paste0("Overall impact (mPT/kg)"),
                    paste0("Impact of\n", tolower(factor_do), " (mPT/kg)"))
  g1 <- ggplot(sdata_kg, aes(y=reorder(food_lca, value), x=value, fill=food_group)) +
    {if(group_yn=="Yes"){facet_grid(food_group~., space="free_y", scale="free_y", labeller = label_wrap_gen(10))}} +
    geom_bar(stat="identity", alpha=0.7) +
    geom_errorbar(mapping=aes(y=food_lca, xmin=value_lo, xmax=value_hi, color=food_group), linewidth=1, width=0) +
    # Labels
    labs(x=xtitle1, y="") +
    scale_x_continuous(sec.axis = dup_axis()) +
    # Legend
    scale_fill_manual(name="Food group", values=food_group_colors) +
    scale_color_manual(name="Food group", values=food_group_colors) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position = "none")
  
  # Plot data
  xtitle2 <- ifelse(factor=="Overall",
                    paste0("Overall impact (mPT/100 NVS)"),
                    paste0("Impact of\n", tolower(factor_do), " (mPT/100 NVS)"))
  g2 <- ggplot(sdata_nvs, aes(y=reorder(food_lca, value), x=value, fill=food_group)) +
    {if(group_yn=="Yes"){facet_grid(food_group~., space="free_y", scale="free_y", labeller = label_wrap_gen(10))}} +
    geom_bar(stat="identity", alpha=0.7) +
    geom_errorbar(mapping=aes(y=food_lca, xmin=value_lo, xmax=value_hi, color=food_group), linewidth=1, width=0) +
    # Labels
    labs(x=xtitle2, y="") +
    scale_x_continuous(sec.axis = dup_axis()) +
    # Legend
    scale_fill_manual(name="Food group", values=food_group_colors) +
    scale_color_manual(name="Food group", values=food_group_colors) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position = "none")
  
  # Merge
  g <- gridExtra::grid.arrange(g1, g2, ncol=2)
  g
  
}

# Plot LCA barplots
plot_lca_overall <- function(data, country, type, factor, unit, group_yn, base_theme){
  
  # Params
  country_do <- country
  factor_do <- factor
  
  # Factor
  if(factor_do=="Overall"){
    factor_do <-ifelse(type=="catg", "Total category impact", "Total life cycle")
    catg_do <- ifelse(type=="catg", "Impact category", "Life stage")
  }
  
  # Format data
  sdata_kg <- data %>% 
    filter(country==country_do & category==catg_do & factor!=factor_do & unit=="mPT/kg")
  sdata_nvs <- data %>% 
    filter(country==country_do & category==catg_do & factor!=factor_do & unit=="mPT/100 NVS")
  
  # Plot data
  xtitle1 <- ifelse(factor=="Overall",
                    paste0("Overall impact (mPT/kg)"),
                    paste0("Impact of\n", tolower(factor_do), " (mPT/kg)"))
  g1 <- ggplot(sdata_kg, aes(y=food_lca, x=value, fill=factor)) +
    {if(group_yn=="Yes"){facet_grid(food_group~., space="free_y", scale="free_y", labeller = label_wrap_gen(10))}} +
    geom_bar(stat="identity", alpha=0.7) +
    # geom_errorbar(mapping=aes(y=food_lca, xmin=value_lo, xmax=value_hi, color=food_group), linewidth=1, width=0) +
    # Labels
    labs(x=xtitle1, y="") +
    scale_x_continuous(sec.axis = dup_axis()) +
    # Legend
    # scale_fill_manual(name="Food group", values=food_group_colors) +
    # scale_color_manual(name="Food group", values=food_group_colors) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position = "none")
  g1
  
  # Plot data
  xtitle2 <- ifelse(factor=="Overall",
                    paste0("Overall impact (mPT/100 NVS)"),
                    paste0("Impact of\n", tolower(factor_do), " (mPT/100 NVS)"))
  g2 <- ggplot(sdata_nvs, aes(y=reorder(food_lca, value), x=value, fill=food_group)) +
    {if(group_yn=="Yes"){facet_grid(food_group~., space="free_y", scale="free_y", labeller = label_wrap_gen(10))}} +
    geom_bar(stat="identity", alpha=0.7) +
    geom_errorbar(mapping=aes(y=food_lca, xmin=value_lo, xmax=value_hi, color=food_group), linewidth=1, width=0) +
    # Labels
    labs(x=xtitle2, y="") +
    scale_x_continuous(sec.axis = dup_axis()) +
    # Legend
    scale_fill_manual(name="Food group", values=food_group_colors) +
    scale_color_manual(name="Food group", values=food_group_colors) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position = "none")
  
  # Merge
  g <- gridExtra::grid.arrange(g1, g2, ncol=2)
  g
  
}

# Plot LCA rasters
# data <- lca; country <- "Indonesia"; unit <- "mPT/kg"; 
plot_lca_rasters <- function(data, country, unit, base_theme){
  
  # Subset data
  cntry_do <- country
  unit_do <- unit
  sdata <- data %>% 
    filter(country==cntry_do & unit==unit_do) %>% 
    mutate(food_group=recode(food_group,
                                    "Legumes, nuts, seeds"="Legumes,\nnuts,\nseeds"))
  
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
    theme_bw() + base_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g
  
}

#  data <- lca; country <- "Indonesia";  factor <- "Climate change"
plot_lca_boxplot <- function(data, country, type, factor, base_theme){
  
  # Food groups
  food_group_colors <- c("#5d5766", "#6c9a92", "#e7b123", "#b95547", "#c8875e")
  
  # Format data
  ################################################################################
  
  # Format data
  country_do <- country
  if(factor=="Overall"){
    factor_do <- ifelse(type=="stage", "Total life cycle", "Total category impact")
  }else{
    factor_do <- factor
    
  }
  sdata <- data %>% 
    # Subset
    filter(country==country_do & factor==factor_do)
  
  # Split data
  sdata1 <- sdata %>% 
    filter(unit=="mPT/kg")
  sdata2 <- sdata %>% 
    filter(unit=="mPT/100 NVS")
  
  # Order
  stats1 <- sdata1 %>% 
    group_by(food_group, dqq_food_group) %>% 
    summarize(value=mean(value)) %>% 
    ungroup() %>% 
    arrange(desc(value))
  stats2 <- sdata2 %>% 
    group_by(food_group, dqq_food_group) %>% 
    summarize(value=mean(value)) %>% 
    ungroup() %>% 
    arrange(desc(value))
  
  # Order data
  sdata1_ordered <- sdata1 %>% 
    mutate(dqq_food_group=factor(dqq_food_group, levels=stats1$dqq_food_group))
  sdata2_ordered <- sdata2 %>% 
    mutate(dqq_food_group=factor(dqq_food_group, levels=stats2$dqq_food_group))
  
  
  # Plot data
  ################################################################################
  
  # Plot data
  xtitle1 <- paste0(factor, " impact (mPT/kg)")
  g1 <- ggplot(sdata1_ordered, aes(y=dqq_food_group, x=value, fill=food_group)) +
    geom_boxplot() +
    geom_point(data=stats1, mapping=aes(y=dqq_food_group, x=value, fill=food_group), pch=21, size=2) +
    # Labels
    labs(x=xtitle1, y="", tag="A") +
    # Legend
    scale_fill_manual(name="Food group", values=food_group_colors) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position = c(0.8, 0.75))
  g1 
  
  # Plot data
  xtitle2 <- paste0(factor, " impact (mPT/kg)")
  g2 <- ggplot(sdata2_ordered, aes(y=dqq_food_group, x=value, fill=food_group)) +
    geom_boxplot() +
    geom_point(data=stats2, mapping=aes(y=dqq_food_group, x=value, fill=food_group), pch=21, size=2) +
    # Labels
    labs(x=xtitle2, y="", tag="B") +
    # Legend
    scale_fill_manual(name="Food group", values=food_group_colors) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position = "none")
  g2
  
  # Merge
  g <- gridExtra::grid.arrange(g1, g2, ncol=1)
  g
  
  
}











