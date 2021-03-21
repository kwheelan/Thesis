#=========================================================================
# Initially weighting elementary school district demographics by 
# block-level census data. This is decennial data for now. May update
# to ACS block-group data, which is more up-to-date.

# Later: could use high level population distribution across blocks
# to weight block-group ACS data

# to-do : income data
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

ELA = read.csv("C:/Users/katri/Desktop/Thesis/School performance data/general_ILboard_mathELA.csv")
ELA = ELA %>% filter(City == 'Chicago') %>% filter(School.Type == 'ELEMENTARY')


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

school_crosswalk = read.csv("C:/Users/katri/Desktop/Thesis/school_crosswalk.csv")
school_crosswalk = school_crosswalk[!is.na(school_crosswalk$ID),]

# Match records to get ELA proficiency
# check errors here
for (i in 1:dim(elem.df)[1]) {
  il.sch.name = school_crosswalk$IL.original[school_crosswalk$CPS == elem.df$school_nm[i]]
  if (il.sch.name != ""){
    #ELA proficiency
    elem.df$prop.ELA.proficient[i] = ELA$X..ELA.Proficiency.1[ELA$School.Name == il.sch.name]
    elem.df$prop.ELA.proficient.male[i] = ELA$X..ELA.Proficiency...Male.1[ELA$School.Name == il.sch.name]
    elem.df$prop.ELA.proficient.female[i] = ELA$X..ELA.Proficiency...Female.1[ELA$School.Name == il.sch.name]
    elem.df$prop.ELA.proficient.white[i] = ELA$X..ELA.Proficiency...White.1[ELA$School.Name == il.sch.name]
    elem.df$prop.ELA.proficient.black[i] = ELA$X..ELA.Proficiency...Black.or.African.American.1[ELA$School.Name == il.sch.name]
    elem.df$prop.ELA.proficient.hispanic[i] = ELA$X..ELA.Proficiency...Hispanic.or.Latino.1[ELA$School.Name == il.sch.name]
    elem.df$prop.ELA.proficient.asian[i] = ELA$X..ELA.Proficiency...Asian.1[ELA$School.Name == il.sch.name]
    elem.df$prop.ELA.proficient.pi[i] = ELA$X..ELA.Proficiency...Native.Hawaiian.or.Other.Pacific.Islander.1[ELA$School.Name == il.sch.name]
    elem.df$prop.ELA.proficient.multirace[i] = ELA$X..ELA.Proficiency...Two.or.More.Races.1[ELA$School.Name == il.sch.name]
    elem.df$prop.ELA.proficient.lowincome[i] = ELA$X..ELA.Proficiency...Low.Income.1[ELA$School.Name == il.sch.name]
    elem.df$prop.ELA.proficient.homeless[i] = ELA$X..ELA.Proficiency...Homeless.1[ELA$School.Name == il.sch.name]
    elem.df$prop.ELA.proficient.disabilities[i] = ELA$X..ELA.Proficiency...Children.with.Disabilities.1[ELA$School.Name == il.sch.name]
    
    #Math proficiency
    elem.df$prop.math.proficient[i] = ELA$X..Math.Proficiency.1[ELA$School.Name == il.sch.name]
    elem.df$prop.math.proficient.male[i] = ELA$X..Math.Proficiency...Male.1[ELA$School.Name == il.sch.name]
    elem.df$prop.math.proficient.female[i] = ELA$X..MAth.Proficiency...Female.1[ELA$School.Name == il.sch.name]
    elem.df$prop.math.proficient.white[i] = ELA$X..Math.Proficiency...White.1[ELA$School.Name == il.sch.name]
    elem.df$prop.math.proficient.black[i] = ELA$X..Math.Proficiency...Black.or.African.American.1[ELA$School.Name == il.sch.name]
    elem.df$prop.math.proficient.hispanic[i] = ELA$X..Math.Proficiency...Hispanic.or.Latino.1[ELA$School.Name == il.sch.name]
    elem.df$prop.math.proficient.asian[i] = ELA$X..Math.Proficiency...Asian.1[ELA$School.Name == il.sch.name]
    elem.df$prop.math.proficient.pi[i] = ELA$X..Math.Proficiency...Native.Hawaiian.or.Other.Pacific.Islander.1[ELA$School.Name == il.sch.name]
    elem.df$prop.math.proficient.multirace[i] = ELA$X..Math.Proficiency...Two.or.More.Races.1[ELA$School.Name == il.sch.name]
    elem.df$prop.math.proficient.lowincome[i] = ELA$X..Math.Proficiency...Low.Income.1[ELA$School.Name == il.sch.name]
    elem.df$prop.math.proficient.homeless[i] = ELA$X..Math.Proficiency...Homeless.1[ELA$School.Name == il.sch.name]
    elem.df$prop.math.proficient.disabilities[i] = ELA$X..Math.Proficiency...Children.with.Disabilities.1[ELA$School.Name == il.sch.name]
    
  }
}


new.col.names = c("prop.school.black", 
                  "prop.school.white",
                  "prop.school.hispanic",
                  "prop.school.asian",
                  "prop.school.pi",
                  "prop.school.multi",
                  "prop.school.disability",
                  "prop.school.EL",
                  "prop.school.lowincome",
                  "prop.school.homeless"
                  )

old.col.names = c("X..Student.Enrollment...Black.or.African.American", 
                  "X..Student.Enrollment...White",
                  "X..Student.Enrollment...Hispanic.or.Latino",
                  "X..Student.Enrollment...Asian",
                  "X..Student.Enrollment...Native.Hawaiian.or.Other.Pacific.Islander",
                  "X..Student.Enrollment...Two.or.More.Races",
                  "X..Student.Enrollment...Children.with.Disabilities",
                  "X..Student.Enrollment...EL",
                  "X..Student.Enrollment...Low.Income",
                  "X..Student.Enrollment...Homeless"
                  )

for (i in 1:dim(elem.df)[1]) {
  il.sch.name = school_crosswalk$IL.original[school_crosswalk$CPS == elem.df$school_nm[i]]
  if (il.sch.name != ""){
    for (var in 1:length(new.col.names)) {
      elem.df[i, new.col.names[var]] = general[general$School.Name == il.sch.name,][old.col.names[var]]
      if (is.na(elem.df[i, new.col.names[var]])) {
        elem.df[i, new.col.names[var]] = 0
      }
    }
  }
}

elem.df$white.prop.difference =  elem.df$prop.school.white - (100 * elem.df$prop.white) 
elem.df$white.prop.difference.percent =  elem.df$white.prop.difference / (elem.df$prop.white * 100)


#===================================================================================================
# Save elem.df

#==================================================================================================


#====================================================================================================
# Viewing data

#====================================================================================================

# Creating a function to plot district-level data
plot.map <- function (var, title, limits=c(min(var, na.rm = T), mean(var, na.rm=T), max(var, na.rm=T)), 
                      colors=c("blue", "white","red")) {
  ggplot(elem.df, aes(fill = var), color = "white") +
    geom_sf() +
    coord_sf() +  
    scale_fill_gradient2(high=colors[1], 
                         low=colors[3], 
                         mid=colors[2], 
                         midpoint=limits[2],
                         limits= c(limits[1],limits[3])) +
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


#===================================================================================
# Block group weights for income data from the American Community Survey


# Process:

# (A) Create a matrix of block weights for each block group using (pop of block)/(total block group pop)
# (B) Multiply weights by nums of households in each income category
# (C) Multiply by block/district binary matrix
# (D) Store in elem.df

#===================================================================================

# Get ASC population data
acs.pop = chi_acs_data[chi_acs_data$variable=="total",]
# Matrix of block / block group intersections
block_group_matrix = st_intersects(census2010$geometry, acs.pop$geometry, F)
# Weight by block populations
block_pop_matrix = sweep(block_group_matrix, MARGIN = 1, census2010$population, '*')
# Divide by total block group populations
block_pop_weights = block_pop_matrix / colSums(block_pop_matrix)




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
    
  
  
  







