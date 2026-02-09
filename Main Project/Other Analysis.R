# CENTROID PLOT FOR MN CRIME/POP DATA
# ```{r crime_and_pop_mapping2}
# all_centroids<-data_2023 |>
#   st_centroid()
# 
# pop_scaled <- scales::rescale(all_centroids$total_population_acs5, to = c(1, 100))
# 
# 
# 
# # create packed-circle layout
# packing <- circleProgressiveLayout(
#   pop_scaled,
#   sizetype = "area"
# )
# 
# # row numbers for joining 
# all_centroids$id = row(all_centroids)[,1]
# 
# packing_df <- packing|>
#   drop_na()|>
#   circleLayoutVertices()|>
#   # join coordinates back
#   left_join(all_centroids, by = join_by(id == id))
# 
# # only keep centroids for reservation counties
# top_cities_centroids <- bind_cols(packing, all_centroids)|>
#   filter(top_cities == 1)
# 
# 
# ggplot() +
#   # circles
#   geom_polygon(
#     data = packing_df,
#     aes(x, y, group = id, fill = crime_rate),
#     color = NA,
#   ) +
#   # subset data to get borders for reservation counties
#   geom_polygon(
#     data = subset(packing_df, reservations == 1),
#     aes(x, y, group = id),
#     fill = NA,           # no fill
#     color = "black",     # border color
#     linewidth = .5
#   ) +
#   # top_cities centroids
#   geom_point(
#     data = top_cities_centroids,
#     aes(x, y),
#     color = "black",
#     size = 1.5
#   ) +
#   scale_fill_viridis_c(
#     option = "plasma",
#     name = "Crime Rates"
#   ) +
#   coord_equal() +
#   theme_void()
# ```


### homicide plots from before I was doing just crime rate
# scale_factor <- max(yearly_ts$homicides_pc, na.rm = TRUE) / max(yearly_ts$crime_rate, na.rm = TRUE)
# 
# ggplot(yearly_ts, aes(x = year)) +
#   # Homicides per capita (true scale, left axis)
#   geom_line(aes(y = homicides_pc, color = "Homicides per Capita"), size = 1) +
#   geom_point(aes(y = homicides_pc, color = "Homicides per Capita"), size = 2) +
#   
#   # General crime rate (scaled, right axis)
#   geom_line(aes(y = crime_rate * scale_factor, color = "General Crime Rate"), size = 1, linetype = "dashed") +
#   geom_point(aes(y = crime_rate * scale_factor, color = "General Crime Rate"), size = 2) +
#   
#   # Two axes
#   scale_y_continuous(
#     name = "Homicides per Capita",
#     sec.axis = sec_axis(~ . / scale_factor, name = "General Crime Rate")
#   ) +
#   
#   # Colors and labels
#   scale_color_manual(
#     values = c("Homicides per Capita" = "darkblue", 
#                "General Crime Rate" = "purple")
#   ) +
#   theme_minimal() +
#   labs(
#     title = "Homicides per Capita vs General Crime Rate (ACS1 Counties)",
#     x = "Year",
#     color = "",
#     caption = "Source: FBI Crime data and ACS1 estimates"
#   )
# 
# 
# 
# acs_1_yearly_data|>
#   ggplot(aes(x = homicides/total_population_acs1))+
#   geom_density()+
#   theme_minimal()+
#   labs(title = "Density of Minnesota Homicides by County by Year", 
#        x = "Homicides per Capita per Year", y = NULL, 
#        caption = "Source: FBI Crime data, and ACS1 estimates; using Decennial counts for 2020")