

# # Plot overall
# # country <- "Indonesia"; data <- scores; food_group <- "Animal-source foods"
# plot_barplot <- function(data, country, food_group, base_theme){
#   
#   # Food groups
#   food_group_colors <- c("#5d5766", "#6c9a92", "#e7b123", "#b95547", "#c8875e")
#   
#   # Format data
#   country_do <- country
#   sdata <- data %>% 
#     filter(country==country_do)
#   
#   # If one food group
#   food_group_do <- food_group
#   if(food_group!="Overall"){
#     sdata <- sdata %>% 
#       filter(food_group==food_group_do)
#   }
#   
#   # Plot data
#   ggplot(sdata, aes(y=reorder(food, overall), x=overall, fill=food_group)) +
#     geom_bar(stat="identity") +
#     # Labels
#     labs(x="Nutritional value score", y="") +
#     # Legend
#     scale_fill_manual(name="Food group", values=food_group_colors) +
#     # Theme
#     theme_bw() + base_theme #+
#   # theme(legend.position = c(0.7, 0.05),
#   #       legend.key.size = unit(0.3, "cm"))
#   
# }




# plot_overall <- function(data, group_yn, country, base_theme){
#   
#   # Food groups
#   food_group_colors <- c("#5d5766", "#6c9a92", "#e7b123", "#b95547", "#c8875e")
#   
#   # Format data
#   country_do <- country
#   sdata <- data %>% 
#     filter(country==country_do)
#   
#   # If grouping
#   if(group_yn=="No"){
#     
#     # Plot data
#     ggplot(sdata, aes(y=reorder(food, overall), x=overall, fill=food_group)) +
#       geom_bar(stat="identity") +
#       # Labels
#       labs(x="Nutritional value score", y="") +
#       # Legend
#       scale_fill_manual(name="Food group", values=food_group_colors) +
#       # Theme
#       theme_bw() + base_theme #+
#     # theme(legend.position = c(0.7, 0.05),
#     #       legend.key.size = unit(0.3, "cm"))
#     
#   }else{
#     
#     # Plot data
#     ggplot(sdata, aes(y=reorder(food, overall), x=overall, fill=food_group)) +
#       facet_grid(food_group~., space="free_y", scales="free_y", labeller = label_wrap_gen(10) ) +
#       geom_bar(stat="identity") +
#       # Labels
#       labs(x="Nutritional value score", y="") +
#       # Legend
#       scale_fill_manual(name="Food group", values=food_group_colors) +
#       # Theme
#       theme_bw() + base_theme 
#     
#   }
#   
# }
