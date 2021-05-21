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

ggplot() + 
  #geom_sf(data = tracts19, color="red", alpha=0)  +
  geom_sf(data = elem_boundaries, color = "#691883", fill="#e79aff") + 
  xlim(-87.85, -87.525) + ylim(41.65, 42.01)

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


#==================================================
# weighting plot
#===================================================

library(ggnewscale)

#write.csv(block_to_elem, "~/Desktop/Thesis/geo_matrices/block_to_elem.txt")
#write.csv(block_to_tract, "~/Desktop/Thesis/geo_matrices/block_to_tract.txt")

x=104

tract_intersect_pop = st_within(st_transform(census2000[block_to_elem[x,],]$geometry, crs = 3488), 
                                st_buffer(st_transform(acs_data[block_to_tract[block_to_elem[x,],] %>% colSums() > 0,]$geometry, 
                                                       crs=3488), dist=50), F)

total.area.pops = data.frame(
  "pop" = t(tract_intersect_pop) %*% as.vector(census2000[block_to_elem[x,],]$population),
  "geometry" = c(st_intersection(elem.df.2009[x,]$geometry, acs_data[block_to_tract[block_to_elem[x,],] %>% colSums() > 0,]$geometry[1]),
                 st_intersection(elem.df.2009[x,]$geometry, acs_data[block_to_tract[block_to_elem[x,],] %>% colSums() > 0,]$geometry[2]),
                 st_intersection(elem.df.2009[x,]$geometry, acs_data[block_to_tract[block_to_elem[x,],] %>% colSums() > 0,]$geometry[3]),
                 st_intersection(elem.df.2009[x,]$geometry, acs_data[block_to_tract[block_to_elem[x,],] %>% colSums() > 0,]$geometry[4]))
)

st_geometry(total.area.pops) = total.area.pops$geometry

total.pop= sum(total.area.pops$pop)
total.area.pops$prop.pop = total.area.pops$pop / total.pop
for(i in 1:4){
  total.area.pops$lab[i] = concat(c("Weight = ", round(total.area.pops$prop.pop[i], 2)))
}

cols = c("darkgreen", "darkblue", "darkorange", "#CBC3E3")

library(gridExtra) # for nice side-by-side plots
library(lemon)

grid.arrange(
ggplot() +
  geom_sf(data = census2000[block_to_elem[x,],], aes(fill = population),
          show.legend = F, alpha=1) +
  theme(legend.position="top") +
  labs(fill = "Block Population") + 
  scale_fill_gradient(high="black", low="white") +
  new_scale("fill") +
  geom_sf(data = elem.df.2009[x,], color="red", alpha=1) + 
  xlim(c(-87.76566, -87.74566))+
  ylim(c(41.89483, 41.90946)) + 
  ggtitle("Panel A"),


ggplot() +
  geom_sf(data = census2000[block_to_elem[x,],], aes(fill = population),
          show.legend = F) +
  labs(fill = "Block Population") + 
  scale_fill_gradient(high="black", low="white") +
  new_scale("fill") +
  geom_sf(data = acs_data[block_to_tract[block_to_elem[x,],] %>% colSums() > 0,]$geometry, 
          alpha=0.25, color="black",
          aes(fill=acs_data[block_to_tract[block_to_elem[x,],] %>% colSums() > 0,]$median.household.income),
          show.legend = F) +
  scale_fill_viridis(direction=-1) +
  geom_sf(data = elem.df.2009[x,], color="red", alpha=0) +
  ggtitle("Panel B"), 

ggplot() +
  geom_sf(data = total.area.pops, aes(fill = 1), show.legend = F) +
  labs(fill = "Block Population") + 
  scale_fill_gradient(low="#7E7E7E", high="#464646") +
  new_scale("fill") +
  geom_sf(data = acs_data[block_to_tract[block_to_elem[x,],] %>% colSums() > 0,]$geometry, 
          alpha=0.35, color="black",
          aes(fill=acs_data[block_to_tract[block_to_elem[x,],] %>% colSums() > 0,]$median.household.income),
          show.legend = F) +
  scale_fill_viridis(direction=-1) +
  geom_sf(data = elem.df.2009[x,], color="red", alpha=0) +
  geom_sf_text(data = acs_data[block_to_tract[block_to_elem[x,],] %>% colSums() > 0,]$geometry,
               aes(label = round(total.area.pops$prop.pop,2)), color=cols) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  ggtitle("Panel C"),

ncol=1)





