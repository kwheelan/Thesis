#=========================================================================
# Initially weighting elementary school district demographics by 
# block-level census data. This is decennial data for now. May update
# to ACS block-group data, which is more up-to-date.

# Later: could use high level population distribution across blocks
# to weight block-group ACS data

# to-do : record matching
#         determine appropriate metrics 

# Use population distribrution weights from decennial to weight
# ACS (for now)
#=========================================================================

# Load packages

library(tidyr)
library(dplyr)
library(tidycensus)
library(sf)
library(ggplot2)
library(viridis)
library(lwgeom)


#==========================================================================
# Get data

# (A) Decennial census data for population weights by block
# (B) ACS data for block-group level demographic characteristics
# (C) Elementary school geographic boundaries

#==========================================================================

# Decennial census population data

census2010 <- get_decennial(year=2010, geography = "block", state="IL", county="Cook", geometry = TRUE,
                              variables = c(population = "P001001",
                                            all.races = "P003001",
                                            white = "P005003", #not hispanic or latino
                                            af.am = "P003003",
                                            hispanic = "P004003",
                                            am.ind = "P003004",
                                            asian = "P003005",
                                            nh.pi = "P003006",
                                            multiple = "P003008",
                                            other = "P003007"))

census2010 = census2010 %>% data.frame() %>%
  pivot_wider(
  names_from = "variable",
  names_prefix = "",
  values_from = "value",
  id_cols = c("NAME", "geometry", "GEOID") ) %>% 
  st_as_sf()


# ACS block-group data

chi_acs_data <- get_acs(year=2019, geography = "block group", state="IL", county="Cook", geometry = TRUE,
                        variables = c(popululation = "B02001_001",
                                      median.gross.rent = "B25064_001",
                                      median.household.income = "B19013_001",
                                      rent.burden = "B25071_001",
                                      total = "B03002_001",
                                      white = "B03002_003", 
                                      af.am = "B03002_004",
                                      hispanic = "B03002_012",
                                      am.ind = "B03002_005",
                                      asian = "B03002_006",
                                      nh.pi = "B03002_007",
                                      multiple = "B03002_009",
                                      other = "B03002_008"))

# Elementary school boundaries

elem_boundaries = st_read("C:/Users/katri/Desktop/Thesis/SY19-20_elem_boundaries/19-20_boundaries.shx")
elem_boundaries = elem_boundaries %>% st_set_crs("NAD83")

# Elementary school general characteristics

general <- read.csv("C:/Users/katri/Desktop/Thesis/general_ILboard.csv") %>% 
  filter(City == 'Chicago') %>% filter(School.Type == 'ELEMENTARY')

#========================================================================================
# Set up new dataframe for data of interest

# Find weighted demographics

# Process:
# For each block:
#   Find area overlap with elem district / total block area
#   Use this to weight demographic characteristics
#   Add together for each elem district


#========================================================================================

elem.df = elem_boundaries %>% select(c(school_add, geometry, school_nm))

# Use intersects to make binary matrix of overlapping blocks and boundaries
intersect_matrix = st_intersects(elem_boundaries$geometry, census2010$geometry, F)

# Find district-level demographics
for (col in colnames(census2010)[-(1:3)]) {
  elem.df[col] = intersect_matrix %*% as.matrix(data.frame(census2010)[col])
  elem.df[paste("prop.",col, sep="")] = as.matrix(data.frame(elem.df)[col]) / elem.df$population
}


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


#===================================================================================
# Matching school names across records
#===================================================================================

names.IL = general$School.Name
names.IL.fixed = sub( "((Middle|Elementary|Elem|Magnet) (School|Schl|Sch|Academy|Acad)| (Academy|Acad|Magnet|Elem|ES|School)$)", "", names.IL) %>% toupper
names.IL.fixed = sub(" $", "", names.IL.fixed)

#===================================================================================
# Weights -- unnecesssary right now but could be useful for block-groups
#===================================================================================



# Make a matrix of block weights
block_weights = matrix(0, dim(elem_boundaries)[1], dim(dec_pop)[1])

for (school in 1:1){#dim(elem_boundaries)[1]){
  for (block in 1:dim(dec_pop)[1]) {
    if (intersect_matrix[school,block]){
          # get overlap btwn block and district
          block.wt = suppressMessages(
                        st_intersection(
                          elem_boundaries$geometry[school],
                          dec_pop$geometry[block]) %>%
                      st_area() %>% 
                      as.numeric() )
          # divide by total block area for weight
          block.wt = block.wt / 
                    (as.numeric( 
                      st_area(
                        dec_pop$geometry[block]
                      )))
    } else{
      block.wt = 0
    }
  block_weights[school, block] = block.wt
  }
}
    
  
  
  







