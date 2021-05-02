
library(tidyr)
library(dplyr)
library(tidycensus)
library(sf)
library(ggplot2)
library(viridis)
library(lwgeom)

load_variables(2018, "acs1") -> varnames
#load_variables(2010, "sf1") -> decennial

#Cleaning school data

general <- read.csv("C:/Users/katri/Desktop/Thesis/general_ILboard.csv") %>% 
      filter(City == 'Chicago') %>% filter(School.Type == 'ELEMENTARY')

scores <- read.csv("C:/Users/katri/Desktop/Thesis/general_ILboard_mathELA.csv") %>% 
      filter(City == 'Chicago') %>% filter(School.Type == 'ELEMENTARY')


elem_boundaries = st_read("C:/Users/katri/Desktop/Thesis/elem_boundaries.shx")
census_tracts = st_read("C:/Users/katri/Desktop/Thesis/census_tracts.shx")

ggplot() + 
  geom_sf(data = census_tracts, size = 0.5, color = "red", fill="orange") + 
  ggtitle("Census Tract Boundary Plot") + 
  coord_sf()
 
ggplot() + 
  geom_sf(data = elem_boundaries, size = 0.5, color = "blue", fill="lightblue") + 
  ggtitle("Elementary School District Boundary Plot") + 
  coord_sf()

ggplot(elem_boundaries, aes(fill = fill, color = fill)) +
  geom_sf() +
  coord_sf()+ scale_fill_viridis(direction=-1, option="inferno") +
  scale_color_viridis(direction=-1, option="inferno") + ggtitle("Elementary School District Boundary Plot")
  
ggplot() +
  geom_sf(data = census_tracts, size = 0.5, color = "red") + 
  geom_sf(data = elem_boundaries, size = 0.5, color = "blue", alpha=0) + 
  ggtitle("Elem District and Census Tract Boundary Plot") + 
  coord_sf()


#=================================================================

general %>% select(c(X..Student.Enrollment...White, X..Student.Enrollment...Black.or.African.American, X..Student.Enrollment...Hispanic.or.Latino, X..Student.Enrollment...Asian, X..Student.Enrollment...Native.Hawaiian.or.Other.Pacific.Islander, X..Student.Enrollment...American.Indian.or.Alaska.Native, X..Student.Enrollment...Two.or.More.Races)) -> demographics

colnames(demographics) = c('White','Black','Hispanic','Asian', 'HIPacIs', 'AI_AN', 'mult')

demographics$max <- apply(demographics, 1, max, na.rm=T)
demographics$D50 <- demographics$max > 50
demographics$D70 <- demographics$max > 70
demographics$D90 <- demographics$max > 90

pie(table(demographics$D90)) + title("Schools with 90%+ students same race")

cPS_demographics <- demographics[1,1:5]
colnames(cPS_demographics)[5] = c('Other')
cPS_demographics$White = sum(demographics$White, na.rm=T)/sum(demographics[,1:7], na.rm=T)
cPS_demographics$Black = sum(demographics$Black, na.rm=T)/sum(demographics[,1:7], na.rm=T)
cPS_demographics$Hispanic = sum(demographics$Hispanic, na.rm=T)/sum(demographics[,1:7], na.rm=T)
cPS_demographics$Asian = sum(demographics$Asian, na.rm=T)/sum(demographics[,1:7], na.rm=T)
cPS_demographics$Other = sum(demographics[5:7], na.rm=T)/sum(demographics[,1:7], na.rm=T)


ggplot() +
  geom_histogram(data = demographics, aes(White), fill="skyblue") +
  xlab("Percent White") +
  ggtitle("Racial Distribution of Chicago Elementary Schools")

ggplot() +
  geom_histogram(data = demographics, aes(Black), fill="skyblue") +
  xlab("Percent Black or African-American") +
  ggtitle("Racial Distribution of Chicago Elementary Schools")

ggplot() +
  geom_histogram(data = demographics, aes(Hispanic), fill="skyblue") +
  xlab("Percent Hispanic") +
  ggtitle("Racial Distribution of Chicago Elementary Schools")

ggplot() +
  geom_histogram(data = general, aes(X..Student.Enrollment...Low.Income), fill="orange") +
  xlab("Percent Low Income") +
  ggtitle("Income Distribution of Chicago Elementary Schools")

ggplot() +
  geom_histogram(data = general, aes(X..Student.Enrollment...Homeless), fill="orange") +
  xlab("Percent Homeless") +
  ggtitle("Income Distribution of Chicago Elementary Schools")


#===========================================================================

tract_data = read.csv("C:/Users/katri/Desktop/Thesis/tract_covariates.csv") %>% filter(czname == "Chicago")

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


chi_dec_data <- get_decennial(year=2010, geography = "block", state="IL", county="Cook", geometry = TRUE,
                      variables = c(all.races = "P003001",
                                white = "P005003", #not hispanic or latino
                               af.am = "P003003",
                              hispanic = "P004003",
                             am.ind = "P003004",
                            asian = "P003005",
                           nh.pi = "P003006",
                          multiple = "P003008",
                         other = "P003007"))

chi_dec_data <- get_decennial(year=2010, geography = "block", state="IL", county="Cook", geometry = TRUE, 
                              variables=c(median.household.income = "B19013_001"))

elem_boundaries = elem_boundaries %>% st_set_crs("NAD83")

ggplot() +
  xlim(-87.85, -87.525) + ylim(41.65, 42.01) +
  geom_sf(data = chi_acs_data[chi_acs_data$variable=="median.household.income",], aes(fill = (estimate), color = (estimate))) +
  scale_fill_viridis(direction=-1, option="inferno", limits=c(0, 80000)) +
  scale_color_viridis(direction=-1, option="inferno", limits=c(0, 80000)) + 
  geom_sf(data = elem_boundaries, size = 0.25, color = "black", alpha=0) +
  labs(title = "Median household income", 
       subtitle = "Census tracts overlaid with elementary school boundaries",
       caption = "Data souce: 2019 American Community Survey")

race <- get_acs(year=2019, geography = "block group", state="IL", county="Cook", geometry = TRUE,
                        variables = c(popululation = "B03002_001"))

black <- get_acs(year=2019, geography = "block group", state="IL", county="Cook", geometry = TRUE,
                                  variables = c(popululation = "B03002_004"))

white <- get_acs(year=2019, geography = "block group", state="IL", county="Cook", geometry = TRUE,
                 variables = c(popululation = "B03002_003"))

hisp <- get_acs(year=2019, geography = "block group", state="IL", county="Cook", geometry = TRUE,
                variables = c(popululation = "B03002_012"))


race$prop.af.am = black$estimate/race$estimate
race$prop.white = white$estimate/race$estimate
race$prop.hisp = hisp$estimate/race$estimate
                                                

chi_dec_data <- get_decennial(year=2010, geography = "block", state="IL", county="Cook", geometry = TRUE,
                                                  variables = c(popululation = "P001001"))
                            

ggplot() +
  xlim(-87.85, -87.525) + ylim(41.65, 42.01) +
  geom_sf(data = race, aes(fill = prop.af.am, color = prop.af.am)) +
  scale_fill_viridis(direction=1, option="viridis") +
  scale_color_viridis(direction=1, option="viridis") + 
  geom_sf(data = elem_boundaries, size = 0.25, color = "black", alpha=0) +
  labs(title = "Percent Black or African-American", 
       subtitle = "Census tracts overlaid with elementary school boundaries",
       caption = "Data souce: 2019 American Community Survey")


ggplot() +
  xlim(-87.85, -87.525) + ylim(41.65, 42.01) +
  geom_sf(data = race, aes(fill = prop.white, color = prop.white)) +
  scale_fill_viridis(direction=1) +
  scale_color_viridis(direction=1) + 
  geom_sf(data = elem_boundaries, size = 0.25, color = "black", alpha=0) +
  labs(title = "Percent White", 
       subtitle = "Census tracts overlaid with elementary school boundaries",
       caption = "Data souce: 2018 American Community Survey")

ggplot() +
  xlim(-87.85, -87.525) + ylim(41.65, 42.01) +
  geom_sf(data = race, aes(fill = prop.hisp, color = prop.hisp)) +
  scale_fill_viridis(direction=1) +
  scale_color_viridis(direction=1) + 
  geom_sf(data = elem_boundaries, size = 0.25, color = "black", alpha=0) +
  labs(title = "Percent Hispanic", 
       subtitle = "Census tracts overlaid with elementary school boundaries",
       caption = "Data souce: 2018 American Community Survey")

#===========================================================================
#Test scores
#===========================================================================

tests.scores <- read.csv("C:/Users/katri/Desktop/Thesis/2019_IAR.csv") %>% 
                filter(City == 'Chicago') %>% 
                filter(School.Type == 'ELEMENTARY')

proficiency <- read.csv("C:/Users/katri/Desktop/Thesis/general_ILboard_mathELA.csv") %>% 
  filter(City == 'Chicago') %>% 
  filter(School.Type == 'ELEMENTARY')


colors <- c("Level 1" = "red", "Level 2" = "orange", "Level 3" = "green", "Level 4" = "blue", "Level 5" = "purple")

ggplot() + 
  geom_density(data = tests.scores, aes(All.students.IAR.ELA.Level.5...Grade.3, col="Level 5")) + 
  geom_density(data = tests.scores, aes(All.students.IAR.ELA.Level.4...Grade.3, col="Level 4")) +
  geom_density(data = tests.scores, aes(All.students.IAR.ELA.Level.3...Grade.3, col="Level 3")) +
  geom_density(data = tests.scores, aes(All.students.IAR.ELA.Level.2...Grade.3, col="Level 2")) + 
  geom_density(data = tests.scores, aes(All.students.IAR.ELA.Level.1...Grade.3, col="Level 1")) + 
  labs(x = "Percent",
       y = "Density",
       color = "Legend") +
  scale_color_manual(values = colors) + 
  ggtitle("Proficiency Levels, ELA")


ggplot() + 
  geom_density(data = tests.scores, aes(All.students.IAR.Mathematics.Level.5...Grade.3, col="Level 5")) + 
  geom_density(data = tests.scores, aes(All.students.IAR.Mathematics.Level.4...Grade.3, col="Level 4")) +
  geom_density(data = tests.scores, aes(All.students.IAR.Mathematics.Level.3...Grade.3, col="Level 3")) +
  geom_density(data = tests.scores, aes(All.students.IAR.Mathematics.Level.2...Grade.3, col="Level 2")) + 
  geom_density(data = tests.scores, aes(All.students.IAR.Mathematics.Level.1...Grade.3, col="Level 1")) + 
  labs(x = "Percent",
       y = "Density",
       color = "Legend") +
  scale_color_manual(values = colors) + 
  ggtitle("Proficiency Levels, Math")

scores = data.frame("level" = 1:5,
                    "prop"  = c(mean(tests.scores$All.students.IAR.ELA.Level.1...Grade.3, na.rm=T),
                    mean(tests.scores$All.students.IAR.ELA.Level.2...Grade.3, na.rm=T),
                    mean(tests.scores$All.students.IAR.ELA.Level.3...Grade.3, na.rm=T),
                    mean(tests.scores$All.students.IAR.ELA.Level.4...Grade.3, na.rm=T),
                    mean(tests.scores$All.students.IAR.ELA.Level.5...Grade.3, na.rm=T))
)

ggplot(data=scores, aes(x = level, y = prop)) + 
  geom_bar(stat="identity") +
  ggtitle("Overall Distribution of 3rd Grade ELA Score Levels in CPS Elementary Schools")+
  ylab("Percentage")

#=============================================================================
#Basic Regressions
#=============================================================================

lm(proficiency$X..Math.Proficiency.1 ~ demographics$max + general$X..Student.Enrollment...Low.Income) -> math.prof
summary(math.prof)

lm(proficiency$X..Math.Proficiency.1 ~ demographics$max + general$X..Student.Enrollment...Low.Income + demographics$White) -> math.prof.race.control
summary(math.prof.race.control)

lm(proficiency$X..Math.Proficiency.1 ~ demographics$max + general$X..Student.Enrollment...Low.Income + demographics$Black) -> math.prof.race.control2
summary(math.prof.race.control2)

lm(proficiency$X..Math.Proficiency.1 ~ demographics$max + general$X..Student.Enrollment...Low.Income + demographics$Hispanic) -> math.prof.race.control3
summary(math.prof.race.control3)

lm(proficiency$X..Math.Proficiency.1 ~ demographics$max + general$X..Student.Enrollment...Low.Income + demographics$Black + demographics$Hispanic) -> math.prof.race.control4
summary(math.prof.race.control4)

