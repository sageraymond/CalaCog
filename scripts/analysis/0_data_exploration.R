
## July 10th 2025
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

# Groundhog makes libraries consistent.
library("groundhog")
groundhog.day <- "2025-04-15"
libs <- c("metafor", "broom", "data.table",
          "ggplot2", "tidyr", "multcomp",
          "dplyr","glmmTMB",
          "DHARMa", #' [For assumption checking]
          "cpp11", "withr", "colorspace", "mvtnorm",
          "foreach", "doSNOW")
groundhog.library(libs, groundhog.day)

# Load data ---------------------------------------------------------------

dat <- fread("data/EventDataJul2025.csv")

guide <- readRDS("builds/batch_models_july_2025/model_guide.Rds")

unique(guide$response)

# >>> Behav_Complexity ----------------------------------------------------
hist(dat$Behav_Complexity)
unique(dat$Behav_Complexity)
# ooooooo
guide[response == "Behav_Complexity"]

m <- glmmTMB(Behav_Complexity ~ urbanization + (1|SiteID),
             data = dat,
             family = poisson())
summary(m)

resids <- DHARMa::simulateResiduals(m)
plot(resids)
# Wow, that's actually not bad at all. 



m.disp <- glmmTMB(Behav_Complexity ~ urbanization + (1|SiteID),
             dispformula = ~ urbanization,
             data = dat,
             family = poisson())
anova(m, m.disp)
# Does not improve model quality.

# >>> Contact_duration ----------------------------------------------------
hist(dat$Contact_duration)
hist(log(dat$Contact_duration+1))
# Nope lol.

unique(dat$Contact_duration)
guide[response == "Contact_duration"]
#' [What to do with this buddy...Hmmm]
#' 

m <- glmmTMB(Contact_duration ~ urbanization + (1|SiteID),
             data = dat,
             family = Gamma())

dat[Contact_duration < 0, ] 


dat[Contact_duration == 0, ]
m <- glmmTMB(Contact_duration ~ urbanization + (1|SiteID),
             ziformula = ~.,
             data = dat,
             family = lognormal())
summary(m)

# Duration is not significantly differnet in the wild, but number of 0s is. Cool zinflation for the win.
#' [That's promising!!! ]
resids <- DHARMa::simulateResiduals(m)
plot(resids)


m.disp <- glmmTMB(Contact_duration ~ urbanization + (1|SiteID),
                  dispformula = ~ urbanization,
                  ziformula = ~.,
                  data = dat,
                  family = lognormal())
anova(m, m.disp)
# Does not improve model quality.

# >>> Inv_duration ----------------------------------------------------
hist(dat$Inv_duration)
hist(log(dat$Inv_duration+1))
# Nope lol.

unique(dat$Inv_duration)
guide[response == "Inv_duration"]
#' 

dat[Inv_duration < 0, ] 
m <- glmmTMB(Inv_duration ~ urbanization + (1|SiteID),
             data = dat,
             family = Gamma())

dat[Inv_duration == 0, ]
m <- glmmTMB(Inv_duration ~ urbanization + (1|SiteID),
             ziformula = ~.,
             data = dat,
             family = lognormal())
summary(m)
# Ditto. Cool

# Check residuals:
resids <- DHARMa::simulateResiduals(m)
plot(resids)

# Significant heteroscedaisticity (right hand plot)
m.disp <- glmmTMB(Inv_duration ~ urbanization + (1|SiteID),
                  dispformula = ~ urbanization,
                  ziformula = ~.,
                  data = dat,
                  family = lognormal())
anova(m, m.disp)
summary(m.disp) # 
summary(m)

# >>> Lope ----------------------------------------------------
hist(dat$Lope)

unique(dat$Lope)

dat[Lope < 0, ] 
m <- glmmTMB(Lope ~ urbanization + (1|SiteID),
             data = dat,
             family = binomial(link = "logit"))
m
summary(m)
# easy 

resids <- DHARMa::simulateResiduals(m)
plot(resids)
# Looks good. 

m.disp <- glmmTMB(Lope ~ urbanization + (1|SiteID),
                  dispformula = ~ urbanization,
                  data = dat,
                  binomial(link = "logit"))
anova(m, m.disp)
# Not necessary. 
summary(m.disp) # 
summary(m)

# >>> Solves ----------------------------------------------------
hist(dat$Solves)
hist(log(dat$Solves))
# Nope lol.

unique(dat$Solves)
guide[response == "Solves"]

m <- glmmTMB(Solves ~ urbanization + (1|SiteID),
             data = dat,
             family = binomial(link = "logit"))
summary(m) #' 
confint(m)

library("multcomp")
glht(m, linfct = c("(Intercept) - urbanizationWild = 0")) |> summary()
# Strange. 

resids <- DHARMa::simulateResiduals(m)
plot(resids)
#' *looking pretty pretty pretty good *
dat[Solves == 0, ]

# SUMMARY -----------------------------------------------------------------

#' [Behav_Complexity == Poisson]
#' [Contact_duration == lognormal + ziformula]
#' [Inv_dispersion == lognormal + ziformula + scale]
#' [Lope == binomial]
#' [Solves == binomial]
#' 
#' 
#' #Some summary for first p of results
dat %>% group_by(urbanization) %>% summarise(N = n())
dat1 <- dat %>% dplyr::filter(Orient == "Y")

dat1 %>% group_by(Inv_duration) %>% summarise(N = n())
min(dat1$Inv_duration, na.rm = TRUE)
max(dat1$Inv_duration, na.rm = TRUE)
dat1 %>% group_by(urbanization, Inv_duration) %>% summarise(N = n())

dat2 <- dat1 %>% dplyr::filter(urbanization == "Wild")
dat2 %>% group_by(Inv_duration) %>% summarise(N = n())


dat1 %>% group_by(Contact_duration) %>% summarise(N = n())
min(dat1$Contact_duration, na.rm = TRUE)
max(dat1$Contact_duration, na.rm = TRUE)
dat1 %>% group_by(urbanization, Contact_duration) %>% summarise(N = n())

dat2 <- dat1 %>% dplyr::filter(urbanization == "Wild")
dat2 %>% group_by(Inv_duration) %>% summarise(N = n())
dat2 %>% group_by(Contact_duration) %>% summarise(N = n())

mean(dat1$Behav_Complexity, na.rm = TRUE)
sd(dat1$Behav_Complexity, na.rm = TRUE)
min(dat1$Behav_Complexity, na.rm = TRUE)
max(dat1$Behav_Complexity, na.rm = TRUE)

dat1 %>% group_by(Lope) %>% summarise(N = n())
dat2 %>% group_by(Lope) %>% summarise(N = n())

dat1 %>% group_by(Solves) %>% summarise(N = n())
dat2 %>% group_by(Solves) %>% summarise(N = n())

dat1 %>% group_by(Sex) %>% summarise(N = n())
dat1 %>% group_by(Disease) %>% summarise(N = n())
dat1 %>% group_by(urbanization, Disease) %>% summarise(N = n())

dat1 %>% group_by(Light) %>% summarise(N = n())
dat1 %>% group_by(GroupSize) %>% summarise(N = n())

mean(dat1$temp, na.rm = TRUE)
sd(dat1$temp, na.rm = TRUE)
min(dat1$temp, na.rm = TRUE)
max(dat1$temp, na.rm = TRUE)

dat1 %>% group_by(PuzzleType) %>% summarise(N = n())

head(dat)
xxx <- dat %>% dplyr::filter(Behav_Complexity > 4)
