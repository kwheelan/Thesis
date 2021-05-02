#View data
ggplot() +
  xlim(-87.85, -87.525) + ylim(41.65, 42.01) +
  geom_sf(data = elem.df, aes(fill = prop.white, color = prop.white)) +
  scale_fill_viridis(direction=1) +
  scale_color_viridis(direction=1) + 
  geom_sf(data = elem_boundaries, size = 0.25, color = "black", alpha=0) +
  labs(title = "Percent White", 
       subtitle = "Demographics of elementary school districts",
       caption = "Data souce: 2010 US Census")

#====================================================================================================
# Viewing data

#====================================================================================================

# Creating a function to plot district-level data
plot.map <- function (var, title, limits=NA, colors=NA, option = NA, direction=1, begin=0, end=1) {
  if(is.na(colors[1])){
    fill_scale = scale_fill_viridis(direction=direction, option = option, begin=begin, end=end)
  } else {
    if(is.na(limits[1])){
      limits = c(min(var, na.rm = T), mean(var, na.rm=T), max(var, na.rm=T))
    }
    fill_scale = scale_fill_gradient2(high=colors[1], 
                                      low=colors[3], 
                                      mid=colors[2], 
                                      midpoint=limits[2],
                                      limits= c(limits[1],limits[3]))
  }
  ggplot(elem.df, aes(fill = var), color = "white") +
    geom_sf() +
    coord_sf() +  
    fill_scale +
    ggtitle(title) + 
    labs(subtitle ="Chicago Public Elementary School Districts (2018)",
         caption = "Data Sources: 2010 US Census; IL Board of Education")
} 


# Generating various plots
plot.map(elem.df$white.prop.difference, "White school/district absolute disparity", c(-65, 0, 15))
plot.map(elem.df$white.prop.difference.percent, "White school/district relative disparity", c(-1, 0, 1))



ggplot() +
  xlim(-87.85, -87.525) + ylim(41.65, 42.01) +
  geom_sf(data = elem.df, aes(fill = prop.ELA.proficient.black, color = prop.ELA.proficient.black)) +
  scale_fill_viridis(direction=1) +
  scale_color_viridis(direction=1) + 
  geom_sf(data = elem_boundaries, size = 0.25, color = "black", alpha=0) +
  labs(title = "Percent ELA Proficient", 
       subtitle = "Outcomes for elementary school districts",
       caption = "Data souce: 2010 US Census")

plot.map(elem.df$med.rent, "Median Gross Rent", option="inferno", direction=-1)

# Plot
plot.map(elem.df$home.owners, "Home Ownership Rate", option="magma", direction=-1)
ge


# Plot
plot.map(elem.df$gini, "Individual Income GINI index", option="magma", direction=-1)
plot.map(elem.df$sd, "Individual Income Standard Deviation", option="magma", direction=-1)
# Plot
plot.map(elem.df$ela.grade3.chisq , "Test score Chi-squared", option="magma", direction=-1)

ggplot() +
  xlim(-87.85, -87.525) + ylim(41.65, 42.01) +
  geom_sf(data = elem.df, aes(fill = most.common.group)) +
  labs(title = "Largest Racial/Ethnic Group", 
       subtitle="Chicago Public Elementary School Boundaries",
       caption = "Data souce: 2010 US Census")

