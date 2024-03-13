
# Plot data
# country <- "Indonesia"
plot_overall <- function(data, country){

  # Format data
  country_do <- country
  sdata <- scores %>% 
    filter(country==country_do)
  
  # Plot data
  ggplot(sdata, aes(y=reorder(food, overall), x=overall)) +
    geom_bar(stat="identity") +
    # Labels
    labs(x="Nutritional value score", y="") +
    # Theme
    theme_bw() 

}




