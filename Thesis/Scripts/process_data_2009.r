#=========================================================================
# Initially weighting elementary school district demographics by 
# block-level census data. This is decennial data for now. May update
# to ACS block-group data, which is more up-to-date.

# Later: could use high level population distribution across blocks
# to weight block-group ACS data

# to-do : income data
#         determine appropriate metrics 

# Use population distribution weights from decennial to weight
# ACS (for now)
#=========================================================================

# Load packages

library(tidyr)
library(dplyr)
library(tidycensus) #interacts with census API
library(sf) #shapefile package
library(ggplot2)
library(viridis) #for nice scaled geo plots
library(lwgeom)
library(reldist) #for gini index calculation
library(ape) #spatio-temporal methods
library(fitdistrplus) #fitting continuous dist to binned data
library(gglorenz) #plot Lorenz curve
library(censusapi)
library(tigris)

Sys.setenv(CENSUS_KEY="5124ebb9660d4aa85433f8785e368e05dce27acb") #API key
Sys.setenv(CENSUS_API_KEY="5124ebb9660d4aa85433f8785e368e05dce27acb") #API key


#==========================================================================
# Get data

# (A) Decennial census data for population weights by block
# (B) ACS data for block-group level demographic characteristics
# (C) Elementary school geographic boundaries

#==========================================================================

# Decennial census population data


varcodes = c("P001001",
        "P003001",
         "P003002", 
         "P003003",
         "P004003",
         "P003004",
         "P003005",
         "P003006",
         "P003008",
         "P003007")

varnames = c(
  "population",
  "all.races",
  "white",
  "af.am",
  "hispanic",
  "am.ind",
  "asian",
  "nh.pi",
  "multiple",
  "other")


options(tigris_use_cache = TRUE)

census2000 <- getCensus(
       name = "dec/sf1",
       vintage = 2010,
       vars = varcodes, 
       region = "block:*",
       regionin = "state:17+county:031+tract:*")

colnames(census2000)[5:14] = varnames

blocks(state = 'IL', county=031) -> blocks_sf

concat = function(list){
  #Function to concatenate strings
  if(length(list) == 1){
    return (list[1])
  }
  return (paste(list[1], concat(list[2:length(list)]), sep=""))
}

# Building GEOID
census2000$GEOID = apply(census2000[,1:4], FUN=concat, MARGIN=1)

# Ordering ids to match
census2000 = census2000[order(census2000$GEOID),]
blocks_sf = blocks_sf[order(blocks_sf$GEOID10),]

st_geometry(census2000) = blocks_sf$geometry


# ACS block-group data

chi_acs_data <- get_acs(year=2009, geography = "tract", state="IL", county="Cook", geometry = TRUE,
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

acs_data = chi_acs_data %>% data.frame() %>%
  pivot_wider(
    names_from = "variable",
    names_prefix = "",
    values_from = "estimate",
    id_cols = c("NAME", "geometry", "GEOID") ) 


# Elementary school boundaries

elem_boundaries = st_read("~/Desktop/Thesis/Boundaries/SY08-09_elem_boundaries/08-09_boundaries.shx")
elem_boundaries = elem_boundaries %>% st_transform("NAD83")

# Elementary school general characteristics

general <- read.csv("~/Desktop/Thesis/School performance data/2009/general_info.csv", strip.white=T) %>% 
  filter(CITY == 'Chicago') %>% filter(SCHOOL.TYPE.NAME == 'ELEMENTARY')

IAR09 = read.csv("~/Desktop/Thesis/School performance data/2009/testdata_processed.txt", strip.white=T) %>%
  filter(City == 'Chicago') %>% filter(Type == 'ELEMENTARY')


#========================================================================================
# Set up new dataframe for data of interest

# Find weighted demographics

# Process:
# For each block:
#   Find area overlap with elem district / total block area
#   Use this to weight demographic characteristics
#   Add together for each elem district


#========================================================================================

elem.df = elem_boundaries[,c("school_add", "geometry", "school_nm")]

# Use intersects to make binary matrix of overlapping blocks and boundaries
# Expanding boundaries by 50 meters and ensuring the block is fully contained
block_to_elem = t(st_within(st_transform(census2000$geometry, crs = 3488), 
                            st_buffer(st_transform(elem_boundaries$geometry, crs=3488), dist=50), F))

# Find district-level demographics
for (col in colnames(census2000)[5:12]) {
  elem.df[col] = block_to_elem %*% as.matrix(data.frame(census2000)[col])
  elem.df[concat(c("prop.",col))] = as.matrix(data.frame(elem.df)[col]) / elem.df$population
}



#===================================================================================
# Matching school names across records
#===================================================================================

school_crosswalk = read.csv("~/Desktop/Thesis/school_crosswalk.csv")
school_crosswalk = school_crosswalk[!is.na(school_crosswalk$ID),]

fix_spaces = function(x){
  return(gsub(pattern = "\xca", replacement = " ", x=x))
}

school_crosswalk = apply(school_crosswalk, MARGIN=2, 
                         FUN = fix_spaces)

data.frame(school_crosswalk) -> school_crosswalk

merge_by_school <- function(df, new, old, year = 0, na.opt=NA){
  for (i in 1:dim(elem.df)[1]) {
    # get school name in IL board of ed data
    if(year != 2009){
      il.sch.name = school_crosswalk$IL.original[school_crosswalk$CPS.2009 == elem.df$school_nm[i]]
    } else {
      il.sch.name = school_crosswalk$IL.2009[school_crosswalk$CPS.2009 == elem.df$school_nm[i]]
    }
    print(il.sch.name)
    # if a match
    if (il.sch.name != ""){
      for (var in 1:length(new)) {
        #print(var)
        # copy each value over to elem.df
        data = df[df$School.Name == il.sch.name,old[var]]
        if (length((data)) > 0){
          elem.df[i, new[var]] = data
          if (is.na(data)) {
            # record NAs as 0 or NA
            elem.df[i, new[var]] = na.opt
          }
        }
        
      }
    } else {
      #print(elem.df$school_nm[i])
    }
  } 
  return(elem.df)
}

new.col.names = c("prop.school.black", 
                  "prop.school.white",
                  "prop.school.hispanic",
                  "prop.school.asian",
                  "prop.school.pi",
                  "prop.school.multi",
                  "prop.school.EL",
                  "prop.school.lowincome",
                  "total.students")

old.col.names = c("SCHOOL...BLACK..", 
                  "SCHOOL...WHITE..",
                  "SCHOOL...HISPANIC..",
                  "SCHOOL...ASIAN..",
                  "SCHOOL...NATIVE.AMERICAN..",
                  "SCHOOL...MULTIRACIAL.ETHNIC..",
                  "L.E.P..SCHOOL..",
                  "LOW.INCOME.SCHOOL..",
                  "SCHOOL.TOTAL.ENROLLMENT")


colnames(general)[3] = "School.Name"
colnames(IAR09)[2] = "School.Name"


# add general demographic school information
elem.df = merge_by_school(general, new.col.names, old.col.names, na.opt = 0, year=2009)

# add testing data for grades 3 - 5
elem.df = merge_by_school(IAR09, colnames(IAR09)[6:29], colnames(IAR09)[6:29], year=2009)

# A metric to calculate difference in racial demographics in district and school
elem.df$prop.school.white = as.numeric(elem.df$prop.school.white)

elem.df$white.prop.difference =  elem.df$prop.school.white - (100 * elem.df$prop.white) 
elem.df$white.prop.difference.percent =  elem.df$ white.prop.difference / (elem.df$prop.white * 100)


#===================================================================================
# Block group weights for income data from the American Community Survey


# Process:

# (A) Create a matrix of block weights for each block group using (pop of block)/(total block group pop)
# (B) Multiply weights by nums of households in each income category
# (C) Multiply by block/district binary matrix
# (D) Store in elem.df

#===================================================================================

# DATA

# Get income data -- to do

tract.income <- get_acs(year=2019, geography = "tract", state="IL", county="Cook", geometry = TRUE,
                        variables = c(#total = "B00001_001", 
                                      total.income = "B19001_001",
                                      income.10k = "B19001_002",
                                      income.15k = "B19001_003",
                                      income.20k = "B19001_004",
                                      income.25k = "B19001_005",
                                      income.30k = "B19001_006",
                                      income.35k = "B19001_007",
                                      income.40k = "B19001_008",
                                      income.50k = "B19001_009",
                                      income.60k = "B19001_010",
                                      income.75k = "B19001_011",
                                      income.100k = "B19001_012",
                                      income.125k = "B19001_013",
                                      income.150k = "B19001_014",
                                      income.200k = "B19001_015",
                                      income.more = "B19001_016",
                                      home.owners = "B25003_002",
                                      home.total = "B25003_001") )


tract.income = tract.income %>% data.frame() %>%
  pivot_wider(
    names_from = "variable",
    names_prefix = "",
    values_from = "estimate",
    id_cols = c("NAME", "geometry", "GEOID") ) 

acs.income = tract.income

# WEIGHTING

# Get ASC population data
acs.pop = chi_acs_data[chi_acs_data$variable=="total",]
# Matrix of block / block group intersections
# For 2009 data this is just tracts
block_group_matrix = st_within(st_transform(census2000$geometry, crs = 3488), 
                               st_buffer(st_transform(acs.pop$geometry, crs=3488), dist=50), F)

# Weight by block populations
block_pop_matrix = sweep(block_group_matrix, MARGIN = 1, census2000$population, '*')

# Divide by total block group populations; deal with divide by zero case
pops = ifelse(colSums(block_pop_matrix) != 0, 1/colSums(block_pop_matrix), 0)
block_pop_weights = sweep(block_pop_matrix, MARGIN = 2, pops, "*")

# Deal with scattered NAs in block groups by assigning city median income
acs_data$median.household.income[is.na(acs_data$median.household.income)] <- median(acs_data$median.household.income, na.rm=T)

acs_data$median.gross.rent[is.na(acs_data$median.gross.rent)] <- median(acs_data$median.gross.rent, na.rm=T)


# INCOME

# Apply weightings to get sum of med incomes for each district
total.income = block_to_elem %*% (block_pop_matrix %*% acs_data$median.household.income) 
# Divide by total population to get average
elem.df$med.income = total.income / (block_to_elem %*% census2000$population)

# RENT

# Apply weightings to get sum of med rent for each district
total.rent = block_to_elem %*% (block_pop_matrix %*% acs_data$median.gross.rent) 
# Divide by total population to get average
elem.df$med.rent = total.rent / (block_to_elem %*% census2000$population)


#===========================================================================================
# Set count of income categories
#===========================================================================================


# Replace NAs with zeros
acs.income$total.income[is.na(acs.income$total.income)] <- 0

# Matrix of block / block group intersections
block_to_tract = st_within(st_transform(census2000$geometry, crs = 3488), 
                           st_buffer(st_transform(acs.income$geometry, crs=3488), dist=50), F)

# Weight by block populations
block_to_tract = sweep(block_to_tract, MARGIN = 1, census2000$population, '*')

# Divide by total block group populations; deal with divide by zero case
pops = ifelse(colSums(block_to_tract) != 0, 1/colSums(block_to_tract), 0)
block_tract_weights = sweep(block_to_tract, MARGIN = 2, pops, "*")

# Apply weightings to get prop of pop at each income level for each district
for (var in colnames(acs.income)[-(1:3)]){
  elem.df[var] = block_to_elem %*% (block_tract_weights %*% as.matrix(acs.income[var]))
  if (var == "home.owners") {
    elem.df[var] = data.frame(elem.df)[var] / elem.df$home.total
  } else if ((var != "total.income") && (var != "home.total")) {
    elem.df[var] = data.frame(elem.df)[var] / elem.df$total.income
  }
}



#=============================================================================================
# Income Distributions

#============================================================================================

# Fit log normal dist

income_dist = function(df, distr) {
  total = df$total.income
  # Get unif data within categories; exp for highest category
  left = c(rep(0, round(df$income.10k * total)), 
           rep(10, round(df$income.15k * total)), 
           rep(15, round(df$income.20k * total)), 
           rep(20, round(df$income.25k * total)),
           rep(25, round(df$income.30k * total)), 
           rep(30, round(df$income.35k * total)),
           rep(35, round(df$income.40k * total)), 
           rep(40, round(df$income.50k * total)),
           rep(50, round(df$income.60k * total)), 
           rep(60, round(df$income.75k * total)), 
           rep(75, round(df$income.100k * total)),
           rep(100, round(df$income.125k * total)),
           rep(125, round(df$income.150k * total)),
           rep(150, round(df$income.200k * total)),
           rep(200, round(df$income.more * total)))
  right = c(rep(10, round(df$income.10k * total)), 
            rep(15, round(df$income.15k * total)), 
            rep(20, round(df$income.20k * total)), 
            rep(25, round(df$income.25k * total)),
            rep(30, round(df$income.30k * total)), 
            rep(35, round(df$income.35k * total)),
            rep(40, round(df$income.40k * total)), 
            rep(50, round(df$income.50k * total)),
            rep(60, round(df$income.60k * total)), 
            rep(75, round(df$income.75k * total)), 
            rep(100, round(df$income.100k * total)),
            rep(125, round(df$income.125k * total)),
            rep(150, round(df$income.150k * total)),
            rep(200, round(df$income.200k * total)),
            rep(NA, round(df$income.more * total)))
  
  bins = data.frame("left" = left, "right" = right)
  fitted_distr <- fitdistcens(bins, distr=distr)
  #plot
  #ggplot() +
  # stat_function(fun = dgamma, args = fitted_distr$estimate, colour = "blue") +
  #  xlim(c(0, 250))
  
  #Get random series
  res = rlnorm(as.integer(total), fitted_distr$estimate)
  
  # Calculate Gini index and standard deviations
  return(data.frame("gini" = gini(res), "sd" = sd(res)))
}

income_dist(elem.df[1,], "lnorm") -> x


for (i in 1:dim(elem.df)[1]){
  #elem.df[i,c("gamma.gini","gamma.sd")] = income_dist(elem.df[i,], "gamma")
  elem.df[i,c("lnorm.gini","lnorm.sd")] = income_dist(elem.df[i,], "lnorm")
}



#===========================================================================================
# Get distribution of outcomes
#===========================================================================================


test_category = function(subject, grade, level) {
  return( concat(c("Grade",grade,".", subject, ".", "Level", level)) )
}

test_scores = function(df, subject, grade) {
  # Get unif data within categories
  if (is.na(df$All.students.IAR.Mathematics.Level.1...Grade.3)){
    return(NA)
  }
  res = c(runif(as.integer(data.frame(df)[test_category(subject, grade, 1)]), 650, 700), 
          runif(as.integer(data.frame(df)[test_category(subject, grade, 2)]), 700, 725),
          runif(as.integer(data.frame(df)[test_category(subject, grade, 3)]),725, 750),
          runif(as.integer(data.frame(df)[test_category(subject, grade, 4)]), 750, 786),
          runif(as.integer(data.frame(df)[test_category(subject, grade, 5)]), 786, 850))
  # Calculate Gini index and standard deviations
  return(data.frame("mean" = mean(res), "sd" = sd(res)))
}

score_distr = function(df, subject, grade) {
  # generate continuous distribution
  total = sum(as.integer(data.frame(df)[test_category(subject, grade, 1)]),
              as.integer(data.frame(df)[test_category(subject, grade, 2)]),
              as.integer(data.frame(df)[test_category(subject, grade, 3)]),
              as.integer(data.frame(df)[test_category(subject, grade, 4)]),
              as.integer(data.frame(df)[test_category(subject, grade, 5)]))
  if (is.na(df$All.students.IAR.Mathematics.Level.1...Grade.3)){
    return(NA)
  }
  left = c(rep(650, as.integer(data.frame(df)[test_category(subject, grade, 1)])), 
           rep(700, as.integer(data.frame(df)[test_category(subject, grade, 2)])),
           rep(725,as.integer(data.frame(df)[test_category(subject, grade, 3)])),
           rep(750, as.integer(data.frame(df)[test_category(subject, grade, 4)])),
           rep(786, as.integer(data.frame(df)[test_category(subject, grade, 5)])))
  right = c(rep(700, as.integer(data.frame(df)[test_category(subject, grade, 1)])), 
            rep(725, as.integer(data.frame(df)[test_category(subject, grade, 2)])),
            rep(750,as.integer(data.frame(df)[test_category(subject, grade, 3)])),
            rep(786, as.integer(data.frame(df)[test_category(subject, grade, 4)])),
            rep(850, as.integer(data.frame(df)[test_category(subject, grade, 5)])))
  bins = data.frame("left" = left, "right" = right)
  
  if(dim(bins)[1] == 0){
    return(c(NA,NA))
  }
  
  fitted_distr <- fitdistcens(bins, distr="norm")
  res = rnorm(as.integer(total), fitted_distr$estimate)
  try({
    fitted_distr <- fitdistcens(bins, distr="gamma")
    #Get random series
    res = rgamma(as.integer(total), fitted_distr$estimate)
  })
  
  # Calculate standard deviations and gamma parameters
  return(data.frame("mean" = mean(res), "sd" = sd(res)))
}

group.props = function(subject, grade){
  # Get proportion of students scoring in each proficiency level across all schools
  res = vector()
  for(i in 1:4){
    res = c(res, mean(as.vector(data.frame(elem.df)[,test_category(subject, grade, i)]), na.rm=T))
  }
  return(res)
}


test_levels = function(df, subject, grade) {
  # Get unif data within categories; exp for highest category
  if (is.na(data.frame(df)[test_category(subject, grade, 1)])){
    return(NA)
  }
  res = c(rep(1, as.integer(data.frame(df)[test_category(subject, grade, 1)])), 
          rep(2, as.integer(data.frame(df)[test_category(subject, grade, 2)])),
          rep(3, as.integer(data.frame(df)[test_category(subject, grade, 3)])),
          rep(4, as.integer(data.frame(df)[test_category(subject, grade, 4)])))
         # rep(5, as.integer(data.frame(df)[test_category(subject, grade, 5)])))
  # Calculate mean and standard deviations and chi-squared
  if(length(res) == 0){
    return(rep(NA, 3))
  }
  
  return(data.frame("mean" = mean(res), 
                    "sd" = sd(res), 
                    # chi-sq null distribution is the overall proportion in each prof. level
                    "chi_sq" = chisq.test(table(factor(res, levels = 1:4)),
                                          p=group.props(subject, grade),
                                          rescale.p = T)$statistic))
}

#TODO -- figure out why gamma MLE throws error for school 231

# calculate disparities for each school
for (i in c(1:dim(elem.df)[1])){
  elem.df[i,c("ela.grade3.mean","ela.grade3.sd", "ela.grade3.chisq")] = test_levels(elem.df[i,], "ELA", 3)
  elem.df[i,c("math.grade3.mean","math.grade3.sd", "math.grade3.chisq")] = test_levels(elem.df[i,], "Math", 3)
  
 # elem.df[i,c("ela.grade3.gamma.mean","ela.grade3.gamma.sd")] = score_distr(elem.df[i,], "ELA", 3)
#  elem.df[i,c("math.grade3.gamma.mean","math.grade3.gamma.sd")] = score_distr(elem.df[i,], "Mathematics", 3)
  
  elem.df[i,c("ela.grade5.mean","ela.grade5.sd", "ela.grade5.chisq")] = test_levels(elem.df[i,], "ELA", 5)
  elem.df[i,c("math.grade5.mean","math.grade5.sd", "math.grade5.chisq")] = test_levels(elem.df[i,], "Math", 5)
  
 # elem.df[i,c("ela.grade5.gamma.mean","ela.grade5.gamma.sd")] = score_distr(elem.df[i,], "ELA", 5)
#  elem.df[i,c("math.grade5.gamma.mean","math.grade5.gamma.sd")] = score_distr(elem.df[i,], "Mathematics", 5)
  
}

plot.map(elem.df$ela.grade3.chisq, option="inferno", direction=-1)

#=====================================================================================
# Racial homogeneity
#=====================================================================================

for (var in c("prop.school.white", 
              "prop.school.black", 
              "prop.school.hispanic",
              "prop.school.asian")){
  elem.df[is.na(elem.df[var]),var] = 0
}

elem.df$largest.group = apply(data.frame(elem.df)[,c("prop.school.white", 
                                                     "prop.school.black", 
                                                     "prop.school.hispanic",
                                                     "prop.school.asian")], 1, max, na.rm=T)

elem.df[,c("school.white", 
           "school.black", 
           "school.hispanic",
           "school.asian")] = data.frame(elem.df)[,c("prop.school.white", 
                                                     "prop.school.black", 
                                                     "prop.school.hispanic",
                                                     "prop.school.asian")] * elem.df$total.students / 100

elem.df$school.white %>% as.integer() -> elem.df$school.white
elem.df$school.black %>% as.integer() -> elem.df$school.black
elem.df$school.hispanic %>% as.integer() -> elem.df$school.hispanic
elem.df$school.asian %>% as.integer() -> elem.df$school.asian

for (var in c("school.white", 
              "school.black", 
              "school.hispanic",
              "school.asian")){
  elem.df[is.na(elem.df[var]),var] = 0
}

elem.df$race.chi.sq = NA

for (i in 1:dim(elem.df)[1]){
  if(!(i %in% c(151, 295))){
    elem.df$race.chi.sq[i] = chisq.test(data.frame(elem.df)[i,c("school.white", 
                                                                "school.black", 
                                                                "school.hispanic",
                                                                "school.asian")],
                                        p = c(0.0975,0.471, 0.3785, 0.031),
                                        rescale.p = T)$statistic
  }
}

elem.df$most.common.group = NA
max.race = apply(data.frame(elem.df)[,c("prop.white", 
                                        "prop.af.am", 
                                        "prop.hispanic",
                                        "prop.asian")], 1, max, na.rm=T)
elem.df$most.common.group[elem.df$prop.white== max.race] = "White"
elem.df$most.common.group[elem.df$prop.af.am == max.race] = "Black"
elem.df$most.common.group[elem.df$prop.hispanic == max.race] = "Hispanic"
elem.df$most.common.group[elem.df$prop.asian == max.race] = "Asian"

#===================================================================================================
# Save elem.df

#==================================================================================================

write.csv(data.frame(st_drop_geometry(elem.df)), "~/Desktop/Thesis/2009_elem_df.csv")


