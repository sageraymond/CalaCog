
## July 15th 2026
#
# 
#' *Let's look at the response variables and their appropriate distributions*
#
#
#
# Prepare workspace ---------------------------------------
#

rm(list = ls())
gc()

# load libraries
library(metafor)
library(broom)
library(data.table)
library(ggplot2)
library(tidyr)
library(multcomp)
library(dplyr)
library(glmmTMB)
library(DHARMa) #' [For assumption checking]
library(cpp11)
library(withr)
library(colorspace)
library(mvtnorm)
library(foreach)
library(doSNOW)

# Load data ---------------------------------------------------------------

dat <- readRDS("data/EventDataJul2025.Rds")




# # #Remove unecessary columns and create new dat file
# str(dat)
# dat <- dat %>% dplyr::select(-(c(Road.density, pop_density, ANTH, NAT, Nat100, Nat250, Total)))
# 
# #and limit to events in which animal was oriented
# dat <- dat %>% dplyr::filter(Orient == "Y") #1156 events
# 
# #clean up a few other things and save as RDS
# dat$Lope <- factor(dat$Lope, levels = c(0,1))
# unique(dat$Lope)
# 
# dat$Solves <- factor(dat$Solves, levels = c(0,1))
# unique(dat$Solves)
# 
# dat <- dat %>% dplyr::select(-(Orient))
# 
# unique(dat$PuzzleType)
# dat$PuzzleType <- factor(dat$PuzzleType, levels = c("W", "A"))
# unique(dat$PuzzleType)
# 
# dat$Light <- factor(dat$Light, levels = c("L", "D"))
# unique(dat$Light)
# 
# dat$urbanization <- factor(dat$urbanization, levels = c("Wild", "City"))
# unique(dat$urbanization)
# 
# unique(dat$Sex)
# dat$Sex <- ifelse(dat$Sex == "SM", "M", dat$Sex)
# dat$Sex <- ifelse(dat$Sex == "SF", "F", dat$Sex)
# 
# dat$Sex <- factor(dat$Sex, levels = c("U", "F", "M"))
# 
# unique(dat$Disease)
# dat$Disease <- ifelse(dat$Disease == "N", 0, 1)
# dat$Disease <- factor(dat$Disease, levels = c(0,1))
# 
# #Add a new binary column for contact
# dat$Contact <- 0
# dat$Contact <- ifelse(dat$Contact_duration > 0, 1, 0)
# unique(dat$Contact)
# str(dat$Contact)
# dat$Contact <- factor(dat$Contact, levels = c(0,1))
# 
# dat$Inv <- 0
# dat$Inv <- ifelse(dat$Inv_duration > 0, 1, 0)
# unique(dat$Inv)
# str(dat$Inv)
# dat$Inv <- factor(dat$Inv, levels = c(0,1))
# 
# #OK. now replace 0 values in contact duration and inv duration with NA
# dat$Contact_duration <- ifelse(dat$Contact_duration == 0, NA, dat$Contact_duration)
# dat$Inv_duration <- ifelse(dat$Inv_duration == 0, NA, dat$Inv_duration)
# #
# 
# #OK, now I need to add coordinate info back to this file
# locs <- fread("data/Data to build core dataframe/Locations_LandscapeVar1.csv")
# locs <- locs %>%
#   dplyr::select(Site_ID, Lat, Long) %>%
#   dplyr::rename("SiteID" = Site_ID)
# unique(locs$SiteID)
# 
# str(dat)
# dat1 <- left_join(dat, locs, by = "SiteID")
# View(dat1)
# 
# # #save this file
# saveRDS(dat1, file = "data/EventDataJul2025.Rds")

#let's check distributions for these things:
#"Behav_Complexity" "Contact_duration" "Inv_duration"     "Lope"             "Solves"          



# >>> Lope ----------------------------------------------------
m <- glmmTMB(Lope ~ urbanization + (1|SiteID),
             data = dat,
             family = binomial(link = "logit"))
m
summary(m)
# easy 

resids <- DHARMa::simulateResiduals(m)
plot(resids)
# Looks good. 

#test for spatial autocorrelation
resids.site <- recalculateResiduals(
  resids,
  group = dat$SiteID)

coords <- dat[!duplicated(dat$SiteID), c("Long", "Lat")]

testSpatialAutocorrelation(
  simulationOutput = resids.site,
  x = coords$Long,
  y = coords$Lat
)

#No need to account for spatial autocorrelation!!!!
# DHARMa Moran's I test for distance-based autocorrelation
# 
# data:  resids.site
# observed = -0.066026, expected = -0.022222, sd = 0.047981, p-value = 0.3613
# alternative hypothesis: Distance-based autocorrelation



m.disp <- glmmTMB(Lope ~ urbanization + (1|SiteID),
                  dispformula = ~ urbanization,
                  data = dat,
                  binomial(link = "logit"))
anova(m, m.disp)
# Not necessary. 
summary(m.disp) # 
summary(m)

# >>> Solves ----------------------------------------------------
unique(dat$Solves)

m <- glmmTMB(Solves ~ urbanization + (1|SiteID),
             data = dat,
             family = binomial(link = "logit"))
summary(m) #' 
confint(m)

#Model can't deal with perfect separation

resids <- DHARMa::simulateResiduals(m)
plot(resids)
#' *looking pretty pretty pretty good *

#test for spatial autocorrelation
resids.site <- recalculateResiduals(
  resids,
  group = dat$SiteID)

coords <- dat[!duplicated(dat$SiteID), c("Long", "Lat")]

testSpatialAutocorrelation(
  simulationOutput = resids.site,
  x = coords$Long,
  y = coords$Lat)

#No spatial autocorrelation
# DHARMa Moran's I test for distance-based autocorrelation
# 
# data:  resids.site
# observed = 0.0052397, expected = -0.0222222, sd = 0.0480913, p-value = 0.568
# alternative hypothesis: Distance-based autocorrelation


# >>> Contact ----------------------------------------------------
m <- glmmTMB(Contact ~ urbanization + (1|SiteID),
             data = dat,
             family = binomial(link = "logit"))
m
summary(m)
# easy 

resids <- DHARMa::simulateResiduals(m)
plot(resids)

#test for spatial autocorrelation
resids.site <- recalculateResiduals(
  resids,
  group = dat$SiteID)

coords <- dat[!duplicated(dat$SiteID), c("Long", "Lat")]

testSpatialAutocorrelation(
  simulationOutput = resids.site,
  x = coords$Long,
  y = coords$Lat)

#no autocoreelayion 
# DHARMa Moran's I test for distance-based autocorrelation
# 
# data:  resids.site
# observed = -0.011103, expected = -0.022222, sd = 0.048018, p-value = 0.8169
# alternative hypothesis: Distance-based autocorrelation

# Looks good. 

m.disp <- glmmTMB(Contact ~ urbanization + (1|SiteID),
                  dispformula = ~ urbanization,
                  data = dat,
                  binomial(link = "logit"))
anova(m, m.disp)
# Not necessary. 
summary(m.disp) # 
summary(m)

# >>> Invn ----------------------------------------------------
m <- glmmTMB(Inv ~ urbanization + (1|SiteID),
             data = dat,
             family = binomial(link = "logit"))
m
summary(m)
# easy 

resids <- DHARMa::simulateResiduals(m)
plot(resids)
# Looks good. 

#test for spatial autocorrelation
resids.site <- recalculateResiduals(
  resids,
  group = dat$SiteID)

coords <- dat[!duplicated(dat$SiteID), c("Long", "Lat")]

testSpatialAutocorrelation(
  simulationOutput = resids.site,
  x = coords$Long,
  y = coords$Lat)

#No autocorrelayion
# DHARMa Moran's I test for distance-based autocorrelation
# 
# data:  resids.site
# observed = -0.060458, expected = -0.022222, sd = 0.048094, p-value = 0.4266
# alternative hypothesis: Distance-based autocorrelation

m.disp <- glmmTMB(Inv ~ urbanization + (1|SiteID),
                  dispformula = ~ urbanization,
                  data = dat,
                  binomial(link = "logit"))
anova(m, m.disp)
# Not necessary. 
summary(m.disp) # 
summary(m)



# >>> Contact_duration ----------------------------------------------------
hist(dat$Contact_duration)
unique(dat$Contact_duration)
# ooooooo

m <- glmmTMB(Contact_duration ~ urbanization + (1|SiteID),
             data = dat,
             family = lognormal())
summary(m)

resids <- DHARMa::simulateResiduals(m)
plot(resids)
#  Looks good with lognormal, but it didn't with some of the other distributions

#test for spatial autocorrelation
m_dat <- model.frame(m)
m_dat <- left_join(m_dat, lat_long, by = "SiteID")

resids.site <- recalculateResiduals(
  resids,
  group = m_dat$SiteID)

coords <- m_dat[!duplicated(m_dat$SiteID), c("Long", "Lat")]

testSpatialAutocorrelation(
  simulationOutput = resids.site,
  x = coords$Long,
  y = coords$Lat
)


#No autocorrelayion

# DHARMa Moran's I test for distance-based autocorrelation
# 
# data:  resids.site
# observed = -0.121847, expected = -0.035714, sd = 0.053851, p-value = 0.1097
# alternative hypothesis: Distance-based autocorrelation


#check disp formula for fun
m.disp <- glmmTMB(Contact_duration ~ urbanization + (1|SiteID),
                  dispformula = ~ urbanization,
                  data = dat,
                  family = lognormal())
anova(m, m.disp)


hist(dat$Contact_duration)
hist(log(dat$Contact_duration+1))
# corroborates the lognormal distribution



# >>> Behav_Complexity ----------------------------------------------------
lat_long <- dat %>% dplyr::select(SiteID, Lat, Long)
lat_long <- unique(lat_long)

hist(dat$Behav_Complexity)
unique(dat$Behav_Complexity)

#
m <- glmmTMB(Behav_Complexity ~ urbanization + (1|SiteID),
             data = dat,
             family = poisson())
summary(m)

resids <- DHARMa::simulateResiduals(m)
plot(resids)
#  not bad at all. 


#test for spatial autocorrelation
m_dat <- model.frame(m)
m_dat <- left_join(m_dat, lat_long, by = "SiteID")

resids.site <- recalculateResiduals(
  resids,
  group = m_dat$SiteID)

coords <- m_dat[!duplicated(m_dat$SiteID), c("Long", "Lat")]

testSpatialAutocorrelation(
  simulationOutput = resids.site,
  x = coords$Long,
  y = coords$Lat
)


#No autocorrelayion
# DHARMa Moran's I test for distance-based autocorrelation
# 
# data:  resids.site
# observed = -0.044518, expected = -0.035714, sd = 0.053618, p-value = 0.8696
# alternative hypothesis: Distance-based autocorrelation

#check disp formula for fun
m.disp <- glmmTMB(Behav_Complexity ~ urbanization + (1|SiteID),
                  dispformula = ~ urbanization,
                  data = dat,
                  family = poisson())
anova(m, m.disp)
# Does not improve model quality.


# >>> Inv_duration ----------------------------------------------------
hist(dat$Inv_duration)
unique(dat$Inv_duration)
# ooooooo

m <- glmmTMB(Inv_duration ~ urbanization + (1|SiteID),
             data = dat,
             family = lognormal())
summary(m)

resids <- DHARMa::simulateResiduals(m)
plot(resids)
#  Looks good with lognormal, but it didn't with some of the other distributions


#test for spatial autocorrelation
m_dat <- model.frame(m)
m_dat <- left_join(m_dat, lat_long, by = "SiteID")

resids.site <- recalculateResiduals(
  resids,
  group = m_dat$SiteID)

coords <- m_dat[!duplicated(m_dat$SiteID), c("Long", "Lat")]

testSpatialAutocorrelation(
  simulationOutput = resids.site,
  x = coords$Long,
  y = coords$Lat
)

#No autocorrelation
# DHARMa Moran's I test for distance-based autocorrelation
# 
# data:  resids.site
# observed = -0.026668, expected = -0.029412, sd = 0.049989, p-value = 0.9562
# alternative hypothesis: Distance-based autocorrelation


#check disp formula for fun
m.disp <- glmmTMB(Inv_duration ~ urbanization + (1|SiteID),
                  dispformula = ~ urbanization,
                  data = dat,
                  family = lognormal())
anova(m, m.disp)


hist(dat$Inv_duration)
hist(log(dat$Inv_duration+1))
# corroborates the lognormal distribution




# SUMMARY -----------------------------------------------------------------

#' [Behav_Complexity == Poisson]
#' [Lope == binomial]
#' [Solves == binomial]
#' [Contact == binomial]
#' [Inv == binomial]
#' [Contact_duration == lognormal]
#' [Inv_duration == lognormal]
#' 
#' 
#' #Some summary for first p of results
dat %>% group_by(urbanization) %>% summarise(N = n()) #160 wild, 996 city

dat %>% group_by(Inv_duration) %>% summarise(N = n())
min(dat$Inv_duration, na.rm = TRUE)
max(dat$Inv_duration, na.rm = TRUE)
dat %>% group_by(urbanization, Inv_duration) %>% summarise(N = n())

dat2 <- dat %>% dplyr::filter(urbanization == "Wild")
dat2 %>% group_by(Inv_duration) %>% summarise(N = n())


dat %>% group_by(Contact_duration) %>% summarise(N = n())
min(dat$Contact_duration, na.rm = TRUE)
max(dat$Contact_duration, na.rm = TRUE)
dat %>% group_by(urbanization, Contact_duration) %>% summarise(N = n())

dat2 <- dat %>% dplyr::filter(urbanization == "Wild")
dat2 %>% group_by(Inv_duration) %>% summarise(N = n())
dat2 %>% group_by(Contact_duration) %>% summarise(N = n())

mean(dat$Behav_Complexity, na.rm = TRUE)
sd(dat$Behav_Complexity, na.rm = TRUE)
min(dat$Behav_Complexity, na.rm = TRUE)
max(dat$Behav_Complexity, na.rm = TRUE)

dat %>% group_by(Lope) %>% summarise(N = n())
dat2 %>% group_by(Lope) %>% summarise(N = n())

dat %>% group_by(Solves) %>% summarise(N = n())
dat2 %>% group_by(Solves) %>% summarise(N = n())

dat %>% group_by(Sex) %>% summarise(N = n())
dat %>% group_by(Disease) %>% summarise(N = n())
dat %>% group_by(urbanization, Disease) %>% summarise(N = n())

dat %>% group_by(Light) %>% summarise(N = n())
dat %>% group_by(GroupSize) %>% summarise(N = n())

mean(dat$temp, na.rm = TRUE)
sd(dat$temp, na.rm = TRUE)
min(dat$temp, na.rm = TRUE)
max(dat$temp, na.rm = TRUE)

dat %>% group_by(PuzzleType) %>% summarise(N = n())

head(dat)
xxx <- dat %>% dplyr::filter(Behav_Complexity > 4)




#Because reviewers were especially concerned about autocorrelation within city
#repeat spatial autocorrelation tests for urban only sites


# >>> Inv ----------------------------------------------------
m <- glmmTMB(Inv ~ Nat50 + (1|SiteID),
             data = dat,
             family = binomial(link = logit))
summary(m)

resids <- DHARMa::simulateResiduals(m)
plot(resids)
#  Looks good with lognormal, but it didn't with some of the other distributions


#test for spatial autocorrelation
m_dat <- model.frame(m)
m_dat <- left_join(m_dat, lat_long, by = "SiteID")

resids.site <- recalculateResiduals(
  resids,
  group = m_dat$SiteID)

coords <- m_dat[!duplicated(m_dat$SiteID), c("Long", "Lat")]

testSpatialAutocorrelation(
  simulationOutput = resids.site,
  x = coords$Long,
  y = coords$Lat
)

#No autocorrelation
# DHARMa Moran's I test for distance-based autocorrelation
# 
# data:  resids.site
# observed = -0.034097, expected = -0.034483, sd = 0.048666, p-value = 0.9937
# alternative hypothesis: Distance-based autocorrelation




# >>> Contact ----------------------------------------------------
m <- glmmTMB(Contact ~ Nat50 + (1|SiteID),
             data = dat,
             family = binomial(link = logit))
summary(m)

resids <- DHARMa::simulateResiduals(m)
plot(resids)
#  Looks good with lognormal, but it didn't with some of the other distributions


#test for spatial autocorrelation
m_dat <- model.frame(m)
m_dat <- left_join(m_dat, lat_long, by = "SiteID")

resids.site <- recalculateResiduals(
  resids,
  group = m_dat$SiteID)

coords <- m_dat[!duplicated(m_dat$SiteID), c("Long", "Lat")]

testSpatialAutocorrelation(
  simulationOutput = resids.site,
  x = coords$Long,
  y = coords$Lat
)

#No autocorrelation
# DHARMa Moran's I test for distance-based autocorrelation
# 
# data:  resids.site
# observed = -0.014211, expected = -0.034483, sd = 0.048708, p-value = 0.6773
# alternative hypothesis: Distance-based autocorrelation


# >>> Solves ----------------------------------------------------
m <- glmmTMB(Solves ~ Nat50 + (1|SiteID),
             data = dat,
             family = binomial(link = logit))
summary(m)

resids <- DHARMa::simulateResiduals(m)
plot(resids)
#  Looks good with lognormal, but it didn't with some of the other distributions


#test for spatial autocorrelation
m_dat <- model.frame(m)
m_dat <- left_join(m_dat, lat_long, by = "SiteID")

resids.site <- recalculateResiduals(
  resids,
  group = m_dat$SiteID)

coords <- m_dat[!duplicated(m_dat$SiteID), c("Long", "Lat")]

testSpatialAutocorrelation(
  simulationOutput = resids.site,
  x = coords$Long,
  y = coords$Lat
)

#No autocorrelation
# DHARMa Moran's I test for distance-based autocorrelation
# 
# data:  resids.site
# observed = -0.075164, expected = -0.034483, sd = 0.048656, p-value = 0.4031
# alternative hypothesis: Distance-based autocorrelation







# >>> Lopes ----------------------------------------------------
m <- glmmTMB(Lope ~ Nat50 + (1|SiteID),
             data = dat,
             family = binomial(link = logit))
summary(m)

resids <- DHARMa::simulateResiduals(m)
plot(resids)
#  Looks good with lognormal, but it didn't with some of the other distributions


#test for spatial autocorrelation
m_dat <- model.frame(m)
m_dat <- left_join(m_dat, lat_long, by = "SiteID")

resids.site <- recalculateResiduals(
  resids,
  group = m_dat$SiteID)

coords <- m_dat[!duplicated(m_dat$SiteID), c("Long", "Lat")]

testSpatialAutocorrelation(
  simulationOutput = resids.site,
  x = coords$Long,
  y = coords$Lat
)

#No autocorrelation
# DHARMa Moran's I test for distance-based autocorrelation
# 
# data:  resids.site
# observed = -0.121654, expected = -0.034483, sd = 0.048131, p-value = 0.07012
# alternative hypothesis: Distance-based autocorrelation




# >>> behav div ----------------------------------------------------
m <- glmmTMB(Behav_Complexity ~ Nat50 + (1|SiteID),
             data = dat,
             family = poisson())
summary(m)

resids <- DHARMa::simulateResiduals(m)
plot(resids)
#  Looks good with lognormal, but it didn't with some of the other distributions


#test for spatial autocorrelation
m_dat <- model.frame(m)
m_dat <- left_join(m_dat, lat_long, by = "SiteID")

resids.site <- recalculateResiduals(
  resids,
  group = m_dat$SiteID)

coords <- m_dat[!duplicated(m_dat$SiteID), c("Long", "Lat")]

testSpatialAutocorrelation(
  simulationOutput = resids.site,
  x = coords$Long,
  y = coords$Lat
)

#No autocorrelation
# DHARMa Moran's I test for distance-based autocorrelation
# 
# data:  resids.site
# observed = -0.123446, expected = -0.045455, sd = 0.052404, p-value = 0.1367
# alternative hypothesis: Distance-based autocorrelation



# >>> contact duration ----------------------------------------------------
m <- glmmTMB(Contact_duration ~ Nat50 + (1|SiteID),
             data = dat,
             family = lognormal())
summary(m)

resids <- DHARMa::simulateResiduals(m)
plot(resids)
#  Looks good with lognormal, but it didn't with some of the other distributions


#test for spatial autocorrelation
m_dat <- model.frame(m)
m_dat <- left_join(m_dat, lat_long, by = "SiteID")

resids.site <- recalculateResiduals(
  resids,
  group = m_dat$SiteID)

coords <- m_dat[!duplicated(m_dat$SiteID), c("Long", "Lat")]

testSpatialAutocorrelation(
  simulationOutput = resids.site,
  x = coords$Long,
  y = coords$Lat
)

#No autocorrelation
# DHARMa Moran's I test for distance-based autocorrelation
# 
# data:  resids.site
# observed = -0.132223, expected = -0.045455, sd = 0.052661, p-value = 0.09942
# alternative hypothesis: Distance-based autocorrelation




# >>> Investigate duration  ----------------------------------------------------
m <- glmmTMB(Inv_duration ~ Nat50 + (1|SiteID),
             data = dat,
             family = lognormal())
summary(m)

resids <- DHARMa::simulateResiduals(m)
plot(resids)
#  Looks good with lognormal, but it didn't with some of the other distributions


#test for spatial autocorrelation
m_dat <- model.frame(m)
m_dat <- left_join(m_dat, lat_long, by = "SiteID")

resids.site <- recalculateResiduals(
  resids,
  group = m_dat$SiteID)

coords <- m_dat[!duplicated(m_dat$SiteID), c("Long", "Lat")]

testSpatialAutocorrelation(
  simulationOutput = resids.site,
  x = coords$Long,
  y = coords$Lat
)

#No autocorrelation
# DHARMa Moran's I test for distance-based autocorrelation
# 
# data:  resids.site
# observed = -0.059748, expected = -0.038462, sd = 0.049127, p-value = 0.6648
# alternative hypothesis: Distance-based autocorrelation