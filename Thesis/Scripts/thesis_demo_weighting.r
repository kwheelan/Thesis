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
library(tidycensus) #interacts with census API
library(sf) #shapefile package
library(ggplot2)
library(viridis) #for nice scaled geo plots
library(lwgeom)
library(reldist) #for gini index calculation
library(ape) #spatio-temporal methods
library(fitdistrplus) #fitting continuous dist to binned data
library(gglorenz) #plot Lorenz curve


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

acs_data = chi_acs_data %>% data.frame() %>%
  pivot_wider(
    names_from = "variable",
    names_prefix = "",
    values_from = "estimate",
    id_cols = c("NAME", "geometry", "GEOID") ) 


# Elementary school boundaries

elem_boundaries = st_read("C:/Users/katri/Desktop/Thesis/Boundaries/SY19-20_elem_boundaries/19-20_boundaries.shx")
elem_boundaries = elem_boundaries %>% st_transform("NAD83")

# Elementary school general characteristics

general <- read.csv("C:/Users/katri/Desktop/Thesis/School performance data/2019/general_ILboard.csv") %>% 
  filter(City == 'Chicago') %>% filter(School.Type == 'ELEMENTARY')

ELA = read.csv("C:/Users/katri/Desktop/Thesis/School performance data/2019/general_ILboard_mathELA.csv") %>% 
  filter(City == 'Chicago') %>% filter(School.Type == 'ELEMENTARY')

IAR19 = read.csv("C:/Users/katri/Desktop/Thesis/School performance data/2019/2019_IAR.csv") %>%
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

elem.df = elem_boundaries[,c("school_add", "geometry", "school_nm")]

# Use intersects to make binary matrix of overlapping blocks and boundaries
# Expanding boundaries by 50 meters and ensuring the block is fully contained
block_to_elem = t(st_within(st_transform(census2010$geometry, crs = 3488), 
                            st_buffer(st_transform(elem_boundaries$geometry, crs=3488), dist=50), F))

# Find district-level demographics
for (col in colnames(census2010)[-(1:3)]) {
  elem.df[col] = block_to_elem %*% as.matrix(data.frame(census2010)[col])
  elem.df[paste("prop.",col, sep="")] = as.matrix(data.frame(elem.df)[col]) / elem.df$population
}



#===================================================================================
# Matching school names across records
#===================================================================================

school_crosswalk = read.csv("C:/Users/katri/Desktop/Thesis/school_crosswalk.csv")
school_crosswalk = school_crosswalk[!is.na(school_crosswalk$ID),]



merge_by_school <- function(df, new, old){
  for (i in 1:dim(elem.df)[1]) {
    # get school name in IL board of ed data
    il.sch.name = school_crosswalk$IL.original[school_crosswalk$CPS == elem.df$school_nm[i]]
    # if a match
    if (il.sch.name != ""){
      for (var in 1:length(new)) {
        # copy each value over to elem.df
        elem.df[i, new[var]] = df[df$School.Name == il.sch.name,][old[var]]
        if (is.na(elem.df[i, new[var]])[1]) {
          # record NAs as 0s
          elem.df[i, new[var]] = 0
        }
      }
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
                  "prop.school.disability",
                  "prop.school.EL",
                  "prop.school.lowincome",
                  "prop.school.homeless",
                  "total.students")

old.col.names = c("X..Student.Enrollment...Black.or.African.American", 
                  "X..Student.Enrollment...White",
                  "X..Student.Enrollment...Hispanic.or.Latino",
                  "X..Student.Enrollment...Asian",
                  "X..Student.Enrollment...Native.Hawaiian.or.Other.Pacific.Islander",
                  "X..Student.Enrollment...Two.or.More.Races",
                  "X..Student.Enrollment...Children.with.Disabilities",
                  "X..Student.Enrollment...EL",
                  "X..Student.Enrollment...Low.Income",
                  "X..Student.Enrollment...Homeless",
                  "X..Student.Enrollment")

# add general demographic school information
elem.df = merge_by_school(general, new.col.names, old.col.names)

# add testing data for grades 3 - 5
elem.df = merge_by_school(IAR19, colnames(IAR19)[c(11:20, 201:210, 391:400)], colnames(IAR19)[c(11:20, 201:210, 391:400)])

# A metric to calculate difference in racial demographics in district and school
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
                        variables = c(total = "B06010_001", 
                                      income.10k = "B06010_004",
                                      income.15k = "B06010_005",
                                      income.25k = "B06010_006",
                                      income.35k = "B06010_007",
                                      income.50k = "B06010_008",
                                      income.65k = "B06010_009",
                                      income.75k = "B06010_010",
                                      income.more = "B06010_011",
                                      home.owners = "B07013_002",
                                      home.total = "B07013_001") )


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
block_group_matrix = st_within(st_transform(census2010$geometry, crs = 3488), 
                          st_buffer(st_transform(acs.pop$geometry, crs=3488), dist=50), F)

# Weight by block populations
block_pop_matrix = sweep(block_group_matrix, MARGIN = 1, census2010$population, '*')

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
elem.df$med.income = total.income / (block_to_elem %*% census2010$population)

# RENT

# Apply weightings to get sum of med rent for each district
total.rent = block_to_elem %*% (block_pop_matrix %*% acs_data$median.gross.rent) 
# Divide by total population to get average
elem.df$med.rent = total.rent / (block_to_elem %*% census2010$population)


#===========================================================================================
# Set count of income categories
#===========================================================================================


# Replace NAs with zeros
acs.income$total[is.na(acs.income$total)] <- 0

# Matrix of block / block group intersections
block_to_tract = st_within(st_transform(census2010$geometry, crs = 3488), 
                                              st_buffer(st_transform(acs.income$geometry, crs=3488), dist=50), F)

# Weight by block populations
block_to_tract = sweep(block_to_tract, MARGIN = 1, census2010$population, '*')

# Divide by total block group populations; deal with divide by zero case
pops = ifelse(colSums(block_to_tract) != 0, 1/colSums(block_to_tract), 0)
block_tract_weights = sweep(block_to_tract, MARGIN = 2, pops, "*")

# Apply weightings to get prop of pop at each income level for each district
for (var in colnames(acs.income)[4:14]){
  elem.df[var] = block_to_elem %*% (block_tract_weights %*% as.matrix(acs.income[var]))
  if (var == "home.owners") {
    elem.df[var] = data.frame(elem.df)[var] / elem.df$home.total
  } else if ((var != "total") && (var != "home.total")) {
    elem.df[var] = data.frame(elem.df)[var] / elem.df$total
  }
}



#=============================================================================================
# Income Distributions

#============================================================================================

# Fit gamma dist

income_dist = function(df, distr) {
  total = df$total
  # Get unif data within categories; exp for highest category
  left = c(rep(0, round(df$income.10k * total)), 
          rep(10, round(df$income.15k * total)), 
          rep(15, round(df$income.25k * total)), 
          rep(25, round(df$income.35k * total)), 
          rep(35, round(df$income.50k * total)), 
          rep(50, round(df$income.65k * total)), 
          rep(65, round(df$income.75k * total)), 
          rep(75, round(df$income.more * total)))
  right = c(rep(10, round(df$income.10k * total)), 
           rep(15, round(df$income.15k * total)), 
           rep(25, round(df$income.25k * total)), 
           rep(35, round(df$income.35k * total)), 
           rep(50, round(df$income.50k * total)), 
           rep(65, round(df$income.65k * total)), 
           rep(75, round(df$income.75k * total)), 
           rep(NA, round(df$income.more * total)))
  
  bins = data.frame("left" = left, "right" = right)
  fitted_distr <- fitdistcens(bins, distr=distr)
  #plot
  #ggplot() +
   # stat_function(fun = dgamma, args = fitted_distr$estimate, colour = "blue") +
  #  xlim(c(0, 250))
  
  #Get random series
  res = rgamma(as.integer(total), fitted_distr$estimate)
  
  # Calculate Gini index and standard deviations
  return(data.frame("gini" = gini(res), "sd" = sd(res)))
}



# use uniform dists
income_list = function(df, num=0.005) {
  total = df$total
  # Get unif data within categories; exp for highest category
  res = c(runif(round(df$income.10k * total), 0, 10), 
          runif(round(df$income.15k * total), 10, 15), 
          runif(round(df$income.25k * total), 15, 25), 
          runif(round(df$income.35k * total), 25, 35), 
          runif(round(df$income.50k * total), 35, 50), 
          runif(round(df$income.65k * total), 50 ,65), 
          runif(round(df$income.75k * total), 65, 75), 
          rexp(round(df$income.more * total), num / df$income.more) + 75)
  #ggplot() + geom_density(aes(res))
  
  # Calculate Gini index and standard deviations
  return(data.frame("gini" = gini(res), "sd" = sd(res)))
}

for (i in 1:dim(elem.df)[1]){
  elem.df[i,c("gamma.gini","gamma.sd")] = income_dist(elem.df[i,], "gamma")
  elem.df[i,c("lnorm.gini","lnorm.sd")] = income_dist(elem.df[i,], "lnorm")
}


#===========================================================================================
# Get distribution of outcomes
#===========================================================================================


test_category = function(subject, grade, level) {
  return( paste( paste( paste("All.students.IAR.", subject, sep=""),
                 paste("Level", level, sep="."), sep = "." ),
                 paste("Grade", grade, sep="."), sep ="...") )
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
  for(i in 1:5){
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
          rep(4, as.integer(data.frame(df)[test_category(subject, grade, 4)])),
          rep(5, as.integer(data.frame(df)[test_category(subject, grade, 5)])))
  # Calculate mean and standard deviations and chi-squared
  if(length(res) == 0){
    return(rep(NA, 3))
  }
    
  return(data.frame("mean" = mean(res), 
                    "sd" = sd(res), 
                    # chi-sq null distribution is the overall proportion in each prof. level
                    "chi_sq" = chisq.test(table(factor(res, levels = 1:5)),
                                          p=group.props(subject, grade),
                                          rescale.p = T)$statistic))
}

#TODO -- figure out why gamma MLE throws error for school 231

# calculate disparities for each school
for (i in c(1:dim(elem.df)[1])){
  elem.df[i,c("ela.grade3.mean","ela.grade3.sd", "ela.grade3.chisq")] = test_levels(elem.df[i,], "ELA", 3)
  elem.df[i,c("math.grade3.mean","math.grade3.sd", "math.grade3.chisq")] = test_levels(elem.df[i,], "Mathematics", 3)
  
  elem.df[i,c("ela.grade3.gamma.mean","ela.grade3.gamma.sd")] = score_distr(elem.df[i,], "ELA", 3)
  elem.df[i,c("math.grade3.gamma.mean","math.grade3.gamma.sd")] = score_distr(elem.df[i,], "Mathematics", 3)
  
  elem.df[i,c("ela.grade5.mean","ela.grade5.sd", "ela.grade5.chisq")] = test_levels(elem.df[i,], "ELA", 5)
  elem.df[i,c("math.grade5.mean","math.grade5.sd", "math.grade5.chisq")] = test_levels(elem.df[i,], "Mathematics", 5)
  
  elem.df[i,c("ela.grade5.gamma.mean","ela.grade5.gamma.sd")] = score_distr(elem.df[i,], "ELA", 5)
  elem.df[i,c("math.grade5.gamma.mean","math.grade5.gamma.sd")] = score_distr(elem.df[i,], "Mathematics", 5)
  
}


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

write.csv(data.frame(st_drop_geometry(elem.df)), "~/Desktop/Thesis/elem_df.csv")

#=====================================================================================
# Regressions
#=====================================================================================

elem.df$levels.ela.grade3.sd[elem.df$levels.ela.grade3.sd == 0] <- NA
elem.df$levels.math.grade3.sd[elem.df$levels.math.grade3.sd == 0] <- NA
elem.df$largest.group[elem.df$largest.group < 0] <- NA


lmfull = lm(data = elem.df, ela.grade3.chisq ~ 
              (gini + 
              prop.school.white + 
              prop.school.black + 
              prop.school.hispanic +
              prop.af.am +
              prop.white + 
              largest.group + 
              med.income + 
              med.rent * home.owners +
              prop.school.lowincome +
              prop.school.EL)^2
)

lm.interactions = lm(data = elem.df, ela.grade3.mean ~ 
                 prop.school.white + 
                 prop.school.black + 
                 gamma.gini +
                 prop.white + 
                 prop.af.am + 
                 largest.group +
                 med.income + 
                 log(med.income) + 
                 med.rent + 
                 home.owners + 
                 largest.group * prop.school.black +
                 med.income * prop.school.black +
                 gamma.gini * prop.school.black +
                 med.rent * home.owners +
                 gamma.gini * med.income + 
                 gamma.gini * prop.school.lowincome +
                 gamma.gini * largest.group + 
                 prop.school.white * prop.school.lowincome +
                 prop.school.black * med.rent * home.owners +
                 prop.school.black * prop.school.lowincome +
                 prop.white * med.rent * home.owners +
                 prop.school.EL
) 
lm.interactions %>% summary

step(lm.interactions) -> lmstep

step(lm.interactions, k=3) -> lmstep
lmstep %>% summary


final.model = lm(formula = log(math.grade3.chisq) ~ 
                   gini + 
                   prop.school.white + 
                   #prop.school.black + 
                   #prop.school.hispanic + 
                   #prop.af.am + 
                   #prop.white + 
                   largest.group + 
                   #log(race.chi.sq) + 
                   med.income + 
                   #med.rent +
                   #med.rent:home.owners + 
                   #home.owners + 
                   prop.school.lowincome + 
                   #gini:prop.white + 
                   gini:largest.group + 
                   gini:med.income +
                   prop.school.white:prop.school.hispanic +
                   prop.school.white:prop.school.black +
                   #prop.school.black:med.rent + 
                   #prop.school.black:home.owners +
                   prop.school.black:home.owners +
                   prop.school.black:prop.school.lowincome + 
                   #prop.school.hispanic:med.rent + 
                   #prop.af.am:prop.school.lowincome + 
                   largest.group:med.income, 
                 data = elem.df) 

final.model %>% summary

hat.vals = lm.influence(final.model)[[1]]
influencial.observations = elem.df[which(hat.vals > .05), ]
influencial.observations

elem.no.outliers = elem.df[(-which(hat.vals > 0.05)),]

lm.new = lm(formula = ela.grade3.chisq ~ 
              prop.school.white + 
              prop.school.black +
              prop.school.white * prop.school.lowincome + 
              gini + 
              prop.school.lowincome +
              prop.school.EL  + 
              gini*med.income + 
              largest.group  +
              med.income,
              data = elem.no.outliers)

lm.new %>% summary

#=====================================================================================
# Checking spatial autocorrelation
#=====================================================================================

inv.dists = 1 / st_distance(st_centroid(elem.df$geometry[-c(37,151,295)]))
diag(inv.dists) = 0
units(inv.dists) <- NULL

# Yes, there is spatial autocorrelation
Moran.I(elem.df$ela.grade3.chisq[-c(37,151,295)], inv.dists, na.rm=T)
Moran.I(elem.df$math.grade3.chisq[-c(37,151,295)], inv.dists, na.rm=T)

# But a lot of this comes from income and race
Moran.I(as.vector(elem.df$prop.af.am[-c(37,151,295)]), inv.dists, na.rm=T)
Moran.I(as.vector(elem.df$med.income[-c(37,151,295)]), inv.dists, na.rm=T)

# See if pattern persists after control

lm.control = lm(formula = (ela.grade3.mean) ~ 
                   gini + 
                   prop.school.white +
                   prop.af.am +
                   largest.group + 
                   med.income + 
                   home.owners +
                   prop.school.lowincome +
                   largest.group:med.income, 
                   data = elem.df) 

Moran.I(as.vector(residuals(lm.control)), inv.dists, na.rm=T)
 Moran.I(as.vector(residuals(final.model)), inv.dists, na.rm=T)

elem.df$residuals = NA
elem.df$residuals[-c(37,151,295)] = residuals(lm.control)
plot.map(elem.df$residuals, "Residuals", option="magma", direction=-1)

