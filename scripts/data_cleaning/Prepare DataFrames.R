#Set WD
setwd("C:/Users/sager/OneDrive/Desktop/school/MSc/Survey Info/Recon_Maps/Cognition/CalaCog/data/Data to build core dataframe")

#Load libraries
library(lme4)
library(survival)
library(Hmisc)
library(ggplot2)
library(AICcmodavg)
library(rafalib)
library(MuMIn)
library(MASS)
library(pROC)
library(car)
library(caret)
library(psych)
library(dplyr)
library(tidyr)
library(broom)
library(tidyverse)
library(magrittr)
library(irr)
library(splitstackshape)
library(jtools)
library(ggstance)
library(ggh4x)
library(grid)
library(gridExtra)
library(jtools)
library(interactions)
library(BAMMtools)
library(multcompView)
library(multcomp)
library(sjPlot)
library(klaR)
library(FactoMineR)
library(ggcorrplot)
library(vegan)
library(performance)

#Stitch all raw data (independent variables) together
#read in EACH DATA FILE
NE1 <- read.csv("Independent_Variables_NE1_2024.csv")
NE2 <- read.csv("Independent_Variables_NE2_2024.csv")
NE3 <- read.csv("Independent_Variables_NE3_2024.csv")
NE4 <- read.csv("Independent_Variables_NE4_2024.csv")
NE5 <- read.csv("Independent_Variables_NE5_2024.csv")
NE6 <- read.csv("Independent_Variables_NE6_2024.csv")
NE7 <- read.csv("Independent_Variables_NE7_2024.csv")

NW1 <- read.csv("Independent_Variables_NW1_2024.csv")
NW2 <- read.csv("Independent_Variables_NW2_2024.csv")
NW3 <- read.csv("Independent_Variables_NW3_2024.csv")
NW4 <- read.csv("Independent_Variables_NW4_2024.csv")
NW5 <- read.csv("Independent_Variables_NW5_2024.csv")
NW6 <- read.csv("Independent_Variables_NW6_2024.csv")

SE1 <- read.csv("Independent_Variables_SE1_2024.csv")
SE2 <- read.csv("Independent_Variables_SE2_2024.csv")
SE3 <- read.csv("Independent_Variables_SE3_2024.csv")
SE4 <- read.csv("Independent_Variables_SE4_2024.csv")
SE5 <- read.csv("Independent_Variables_SE5_2024.csv")
SE6 <- read.csv("Independent_Variables_SE6_2024.csv")
SE7 <- read.csv("Independent_Variables_SE7_2024.csv")

SW1 <- read.csv("Independent_Variables_SW1_2024.csv")
SW2 <- read.csv("Independent_Variables_SW2_2024.csv")
SW3 <- read.csv("Independent_Variables_SW3_2024.csv")
SW4 <- read.csv("Independent_Variables_SW4_2024.csv")
SW5 <- read.csv("Independent_Variables_SW5_2024.csv")
SW6 <- read.csv("Independent_Variables_SW6_2024.csv")
SW7 <- read.csv("Independent_Variables_SW7_2024.csv")

E1 <- read.csv("Independent_Variables_E1_2024.csv")
E2 <- read.csv("Independent_Variables_E2_2024.csv")
E3 <- read.csv("Independent_Variables_E3_2024.csv")
# No data at E4 <- read.csv("Independent_Variables_E4_2024.csv")
E5 <- read.csv("Independent_Variables_E5_2024.csv")
E6 <- read.csv("Independent_Variables_E6_2024.csv")
E7 <- read.csv("Independent_Variables_E7_2024.csv")
E8 <- read.csv("Independent_Variables_E8_2024.csv")
E9 <- read.csv("Independent_Variables_E9_2024.csv")
E10 <- read.csv("Independent_Variables_E10_2024.csv")
E11 <- read.csv("Independent_Variables_E11_2024.csv")
E12 <- read.csv("Independent_Variables_E12_2024.csv")
E13 <- read.csv("Independent_Variables_E13_2024.csv")
E14 <- read.csv("Independent_Variables_E14_2024.csv")


#merge all data
Locations <- rbind(NE1, NE2, NE3, NE4, NE5, NE6, NE7,
                   NW1, NW2, NW3, NW4, NW5, NW6,
                   SE1, SE2, SE3, SE4, SE5, SE6, SE7,
                   SW1, SW2, SW3, SW4, SW5, SW6, SW7,
                   E1, E2, E3, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14)

Locations$YEAR <- "2024"

#merge a/w and event and year to form a new identifier column
Locations$Identifier2 <- paste(Locations$Identifier, Locations$PuzzleType, Locations$YEAR, sep = "_")

#Also specify year for individual ID
Locations$UniqueID2 <- paste(Locations$UniqueID, Locations$YEAR, sep = "_")


#Remove things that are not unique (in terms of identifier 2)
Locations2 <- Locations %>%
  distinct(Identifier2, .keep_all = TRUE)

#Clean up data frame a bit
Locations2 <- Locations2 %>%
  dplyr::select(DateTime, Identifier, Identifier2, PuzzleType, YEAR,
                LorD, SiteID, GroupSize, agesex, UniqueID2, Disease, PuzzleSequence)


#Remove 2024 dataframes:
rm(list = setdiff(ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)], c("Locations2")))


#Do the same thing with 2025 data
#Read in 2025 dataframes
E1 <- read.csv("Independent_Variables_E1_2025.csv")
E2 <- read.csv("Independent_Variables_E2_2025.csv")
E4 <- read.csv("Independent_Variables_E4_2025.csv")
E5 <- read.csv("Independent_Variables_E5_2025.csv")
E6 <- read.csv("Independent_Variables_E6_2025.csv")
E7 <- read.csv("Independent_Variables_E7_2025.csv")
E8 <- read.csv("Independent_Variables_E8_2025.csv")
E9 <- read.csv("Independent_Variables_E9_2025.csv")
E10 <- read.csv("Independent_Variables_E10_2025.csv")
E12 <- read.csv("Independent_Variables_E12_2025.csv")
E13 <- read.csv("Independent_Variables_E13_2025.csv")
E14 <- read.csv("Independent_Variables_E14_2025.csv")
E15 <- read.csv("Independent_Variables_E15_2025.csv")
E16 <- read.csv("Independent_Variables_E16_2025.csv")

NE1 <- read.csv("Independent_Variables_NE1_2025.csv")
NE2 <- read.csv("Independent_Variables_NE2_2025.csv")
NE3 <- read.csv("Independent_Variables_NE3_2025.csv")
NE4 <- read.csv("Independent_Variables_NE4_2025.csv")
NE5 <- read.csv("Independent_Variables_NE5_2025.csv")
NE6 <- read.csv("Independent_Variables_NE6_2025.csv")
NE7 <- read.csv("Independent_Variables_NE7_2025.csv")

NW1 <- read.csv("Independent_Variables_NW1_2025.csv")
NW2 <- read.csv("Independent_Variables_NW2_2025.csv")
NW3 <- read.csv("Independent_Variables_NW3_2025.csv")
NW4 <- read.csv("Independent_Variables_NW4_2025.csv")
NW5 <- read.csv("Independent_Variables_NW5_2025.csv")
NW6 <- read.csv("Independent_Variables_NW6_2025.csv")
NW7 <- read.csv("Independent_Variables_NW7_2025.csv")


SE2 <- read.csv("Independent_Variables_SE2_2025.csv")
SE4 <- read.csv("Independent_Variables_SE4_2025.csv")
SE5 <- read.csv("Independent_Variables_SE5_2025.csv")
SE7 <- read.csv("Independent_Variables_SE7_2025.csv")
SE8 <- read.csv("Independent_Variables_SE8_2025.csv")
SE9 <- read.csv("Independent_Variables_SE9_2025.csv")

SW1 <- read.csv("Independent_Variables_SW1_2025.csv")
SW2 <- read.csv("Independent_Variables_SW2_2025.csv")
SW3 <- read.csv("Independent_Variables_SW3_2025.csv")
SW4 <- read.csv("Independent_Variables_SW4_2025.csv")
SW6 <- read.csv("Independent_Variables_SW6_2025.csv")
SW8 <- read.csv("Independent_Variables_SW8_2025.csv")

#merge all data
Locations25 <- rbind(NE1, NE2, NE3, NE4, NE5, NE6, NE7,
                   NW1, NW2, NW3, NW4, NW5, NW6, NW7,
                   SE2,SE4, SE5, SE7, SE8, SE9,
                   SW1, SW2, SW3, SW4, SW6, SW8,
                   E1, E2, E4, E5, E6, E7, E8, E9, E10, E12, E13, E14, E15, E16)

Locations25$YEAR <- "2025"

#merge a/w and event and year to form a new identifier column
Locations25$Identifier2 <- paste(Locations25$Identifier, Locations25$PuzzleType, Locations25$YEAR, sep = "_")

#Also specify year for individual ID
Locations25$UniqueID2 <- paste(Locations25$UniqueID, Locations25$YEAR, sep = "_")


#Remove things that are not unique (in terms of identifier 2)
Locations25. <- Locations25 %>%
  distinct(Identifier2, .keep_all = TRUE)

#Clean up data frame a bit
Locations25. <- Locations25. %>%
  dplyr::select(DateTime, Identifier, Identifier2, PuzzleType, YEAR,
                LorD, SiteID, GroupSize, agesex, UniqueID2, Disease, PuzzleSequence)

#Remove 2025 dataframes:
rm(list = setdiff(ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)], c("Locations2", "Locations25.")))


#Combine the two location files
AllLocations <- rbind(Locations2, Locations25.)
remove(Locations2)
remove(Locations25.)

#In this df, subjects are combined. Remove this information
IndVarsByEventSubjectsCombined <- AllLocations %>%
  dplyr::select(-(c(agesex, UniqueID2, Disease, PuzzleSequence)))
remove(AllLocations)

#Check for errors
unique(IndVarsByEventSubjectsCombined$Identifier2)
unique(IndVarsByEventSubjectsCombined$Identifier)
unique(IndVarsByEventSubjectsCombined$PuzzleType) #Problems
unique(IndVarsByEventSubjectsCombined$SiteID)
unique(IndVarsByEventSubjectsCombined$Identifier)
unique(IndVarsByEventSubjectsCombined$YEAR)

#Fix errors:
IndVarsByEventSubjectsCombined$PuzzleType <- ifelse(IndVarsByEventSubjectsCombined$PuzzleType == "N", "W", IndVarsByEventSubjectsCombined$PuzzleType)

#You need to find and duplicate these two events. And make one W and one A
#NW3_EVENT11_N + A_2025
#NW3_EVENT22_N + A_2025

# Define the IDs you want to select
selected_ids <- c("NW3_EVENT11_N + A_2025", "NW3_EVENT22_N + A_2025")

# Extract and duplicate the selected rows
new_rows <- IndVarsByEventSubjectsCombined %>%
  filter(Identifier2 %in% selected_ids) %>%
  slice(rep(1:n(), each = 2)) %>%
  mutate(PuzzleType = rep(c("W", "A"), times = length(selected_ids)))

# Append the modified rows back to the original dataset
IndVarsByEventSubjectsCombined2 <- bind_rows(IndVarsByEventSubjectsCombined, new_rows)

IndVarsByEventSubjectsCombined2 <- IndVarsByEventSubjectsCombined2 %>% dplyr::filter(PuzzleType != "N + A")
remove(selected_ids)
remove(IndVarsByEventSubjectsCombined)
remove(new_rows)
IndVarsByEventSubjectsCombined <- IndVarsByEventSubjectsCombined2
remove(IndVarsByEventSubjectsCombined2)

#Make Site ID Year column
IndVarsByEventSubjectsCombined$SiteIDYear <- paste(IndVarsByEventSubjectsCombined$SiteID,
                                                   IndVarsByEventSubjectsCombined$YEAR, sep = "_")

#Rewrite Identifier2 column so it is consistent
IndVarsByEventSubjectsCombined <- IndVarsByEventSubjectsCombined %>%
  dplyr::select(-(Identifier2))
IndVarsByEventSubjectsCombined$Identifier2 <- paste(IndVarsByEventSubjectsCombined$Identifier,
                                                    IndVarsByEventSubjectsCombined$PuzzleType,
                                                    IndVarsByEventSubjectsCombined$YEAR,
                                                    sep = "_")


#Next step is to develop urbanization metrics and append to this df
#Bring in variables from GIS
Locations_GIS1 <- read.csv("Locations_LandscapeVar1.csv")
Locations_GIS2 <- read.csv("Locations_LandscapeVar2.csv")

Locations_GIS1 <- Locations_GIS1 %>%
  dplyr::rename("SiteID" = Site_ID)

Locations_GIS2 <- Locations_GIS2 %>%
  dplyr::rename("SiteID" = Site_ID)

#Step 1. Categorize sites by level of urbanization

#Read in location data
LocData <- read.csv("2024Locations_Landscape_Final.csv")
LocData <- read.csv("2025_City_Site_Data_for_PCA.csv")

#Remove Elk Island Sites because we're just dealing with urbanized sites right now
LocData$Letter <- substr(LocData$Site_ID, 1, 1)

YEGData <- LocData %>%
  dplyr::filter(Letter != "E")

#Run PCA on relevant metrics of urbanization
#make dataframe with relevant info:
pca.data <- YEGData %>%
  dplyr::select(-(c(Common_ID, Lat, Long, Neighbourhood, AG, Letter)))

pca.data[is.na(pca.data)] <- 0
pca.data$Road.density <- as.numeric(pca.data$Road.density)
pca.data$NoBldg50 <- as.numeric(pca.data$NoBldg50)
pca.data$NoBldg100 <- as.numeric(pca.data$NoBldg100)
pca.data$NoBldg250 <- as.numeric(pca.data$NoBldg250)
pca.data$NoBldg500 <- as.numeric(pca.data$NoBldg500)
pca.data$NoBldg1000 <- as.numeric(pca.data$NoBldg1000)
pca.data$NoBldg1500 <- as.numeric(pca.data$NoBldg1500)

pca.data <- scale(pca.data[, -1], scale = TRUE, center = TRUE)

YEG.PCA <- princomp(pca.data[, -1], cor = TRUE)

#Check out eugenvalues as proportions

# Get eigenvalues
eigenvalues <- YEG.PCA$sdev^2

# Calculate total variance
total_variance <- sum(eigenvalues)

# Calculate the proportion of variance explained by PC1
pc1_proportion <- eigenvalues[1] / total_variance
pc1_proportion #0.3456919  

pc2_proportion <- eigenvalues[2] / total_variance
pc2_proportion #0.1851862 

pc3_proportion <- eigenvalues[3] / total_variance
pc3_proportion #0.1117146  

#Use first 5 components
cum_variance <- cumsum(eigenvalues) / total_variance
print(cum_variance)

#I will use a weighted sum approach, were urbanization score will be the weight x PC1 + weight X PC2 etc.
# Get the scores (the projection of the data on the principal components)
scores <- YEG.PCA$scores

# Calculate the proportion of variance explained by each PC
proportion_variance <- eigenvalues / sum(eigenvalues)

# Create a weighted urbanization score using the first few components
urbanization_score <- scores[, 1] * proportion_variance[1] +
  scores[, 2] * proportion_variance[2] +
  scores[, 3] * proportion_variance[3] +
  scores[, 4] * proportion_variance[4] +
  scores[, 5] * proportion_variance[5] 

# Check the urbanization score for each site
urbanization_score

# Add the urbanization score to the original data
YEGData$urbanization_score <- urbanization_score

YEGData <- YEGData %>% dplyr::select(Site_ID, urbanization_score, Road.density,
                                     pop_density, ANTH, NAT, Nat50, Nat100, Nat250)

#Negative values are very urbanized. Let's change that so small values are less
#urbanized
YEGData$urbanization_score <- YEGData$urbanization_score * (-1)

YEGData <- YEGData %>%
  dplyr::rename("SiteID" = Site_ID)

#Attach this to IndVarsbyEventSubject
IndVarsByEventSubjectsCombined <- left_join(IndVarsByEventSubjectsCombined,
                                            YEGData,
                                            by = "SiteID")

#Remove everything else
rm(list = setdiff(ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)], "IndVarsByEventSubjectsCombined"))


#replace NAs in urb level with a value of 1000
IndVarsByEventSubjectsCombined$urbanization_score[is.na(IndVarsByEventSubjectsCombined$urbanization_score)] <- 1000

IndVarsByEventSubjectsCombined$UrbLevel <- "Park"
IndVarsByEventSubjectsCombined$UrbLevel <- ifelse(IndVarsByEventSubjectsCombined$urbanization_score < 1000, "City", IndVarsByEventSubjectsCombined$UrbLevel)

#Put a pin in this dataframe. Now start working with event data to create outcome variables

#Load raw data
Events1 <- read.csv("2024_Events_Export.csv")
Events2 <- read.csv("2025_Events_Export_1_of_3.csv")
Events3 <- read.csv("2025_Events_Export_2_of_3.csv")
Events4 <- read.csv("2025_Events_Export_3_of_3.csv")

Events1$YEAR <- "2024"
Events2$YEAR <- "2025"
Events3$YEAR <- "2025"
Events4$YEAR <- "2025"

Events1$Identifier2 <- paste(Events1$Observation.id, Events1$YEAR, Events1$Subject, sep = "_")
Events2$Identifier2 <- paste(Events2$Observation.id, Events2$YEAR, Events2$Subject, sep = "_")
Events3$Identifier2 <- paste(Events3$Observation.id, Events3$YEAR, Events3$Subject, sep = "_")
Events4$Identifier2 <- paste(Events4$Observation.id, Events4$YEAR, Events4$Subject, sep = "_")

Events <- rbind(Events1, Events2, Events3, Events4)
remove(Events1)
remove(Events2)
remove(Events3)
remove(Events4)


#clean up dataframe
Events <- Events %>%
  dplyr::select(Observation.id, Observation.date, YEAR,
                Unique_ID, PuzzleType, Observation.duration.by.subject.by.observation,
                Behavior, Behavioral.category, Modifier..1, Duration..s., Subject, Identifier2)

Events <- Events %>%
  dplyr::rename("EventID" = Observation.id,
                "Date" = Observation.date,
                "Total_Event_Duration_by_Ind" = Observation.duration.by.subject.by.observation,
                "SiteEventID" = Unique_ID,
                "Behave_Duration" = Duration..s.)

#Create column for the SiteID
Events$SiteID <- gsub("\\_.*","",Events$SiteEventID)

#Rename everything; very inefficient! :(
Events$Behave <- paste(Events$Behavior, Events$Modifier..1)
Events$Behave <- ifelse(Events$Behave == "trotting ", "trot", Events$Behave)
Events$Behave <- ifelse(Events$Behave == "in view far more than 1 body length", "far", Events$Behave)
Events$Behave <- ifelse(Events$Behave == "walking ", "walk", Events$Behave)
Events$Behave <- ifelse(Events$Behave == "oriented puzzle", "orient_puzzle", Events$Behave)
Events$Behave <- ifelse(Events$Behave == "investigating puzzle", "inv_puzzle", Events$Behave)
Events$Behave <- ifelse(Events$Behave == "oriented camera", "orient_camera", Events$Behave)
Events$Behave <- ifelse(Events$Behave == "oriented lure", "orient_lure", Events$Behave)
Events$Behave <- ifelse(Events$Behave == "oriented unspecified or unclear", "orient_unk", Events$Behave)
Events$Behave <- ifelse(Events$Behave == "investigating unspecified", "inv_unk", Events$Behave)
Events$Behave <- ifelse(Events$Behave == "contacting bottom of puzzle", "contact_bottom", Events$Behave)
Events$Behave <- ifelse(Events$Behave == "contacting middle of puzzle", "contact_mid", Events$Behave)
Events$Behave <- ifelse(Events$Behave == "solves puzzle ", "solve", Events$Behave)
Events$Behave <- ifelse(Events$Behave == "contacting paw spinning", "contact_spin", Events$Behave)
Events$Behave <- ifelse(Events$Behave == "breaks puzzle ", "break", Events$Behave)

Events$Behave <- ifelse(Events$Behave == "not oriented ", "not_orient", Events$Behave)
Events$Behave <- ifelse(Events$Behave == "still ", "still", Events$Behave)
Events$Behave <- ifelse(Events$Behave == "out of view ", "out_of_view", Events$Behave)
Events$Behave <- ifelse(Events$Behave == "in view close less than 1 body length", "close", Events$Behave)
Events$Behave <- ifelse(Events$Behave == "lope or gallop ", "lopegallop", Events$Behave)
Events$Behave <- ifelse(Events$Behave == "contacting top of puzzle", "contact_top", Events$Behave)
Events$Behave <- ifelse(Events$Behave == "investigating ground", "inv_ground", Events$Behave)
Events$Behave <- ifelse(Events$Behave == "investigating camera", "inv_cam", Events$Behave)
Events$Behave <- ifelse(Events$Behave == "jump backs", "jump", Events$Behave)
Events$Behave <- ifelse(Events$Behave == "contacting unspecified", "contact_unk", Events$Behave)
Events$Behave <- ifelse(Events$Behave == "contacting pulling bungee etc", "contact_bungee", Events$Behave)
Events$Behave <- ifelse(Events$Behave == "contacting eating treats", "eats_treats", Events$Behave)
Events$Behave <- ifelse(Events$Behave == "urinates ", "urinates", Events$Behave)

Events <- Events %>%
  dplyr::select(EventID, Date, SiteEventID, PuzzleType, YEAR,
                Total_Event_Duration_by_Ind, Behave, SiteID, Behave_Duration, Subject,
                Identifier2)

Events$Identifier3 <- paste(Events$Observation.id, Events$YEAR, sep = "_")


#We will need summed behaviours (i.e., sum all trot instances in a single event)
#Create a data frame where this info is summed (i.e., 1 row per event per behaviour per individual)
Events_Summed <- Events %>% group_by(Identifier2, Behave) %>% 
  summarise(Total_behave_duration = sum(Behave_Duration))
Events_Summed <- as.data.frame(Events_Summed)
remove(Events)

#Start building outcome variables-------------------------------------------

#Get contact time (persistence)
Contact <- Events_Summed
#ID events in which animals contacted the puzzle
Contact$Contact <- "N"
Contact$Contact <- ifelse(Contact$Behave == "contact_top", "Y", Contact$Contact)
Contact$Contact <- ifelse(Contact$Behave == "contact_bottom", "Y", Contact$Contact)
Contact$Contact <- ifelse(Contact$Behave == "contact_unk", "Y", Contact$Contact)
Contact$Contact <- ifelse(Contact$Behave == "contact_mid", "Y", Contact$Contact)
Contact$Contact <- ifelse(Contact$Behave == "contact_bungee", "Y", Contact$Contact)
Contact$Contact <- ifelse(Contact$Behave == "contact_spin", "Y", Contact$Contact)

#Calculate contact time for each event
Contact2 <- Contact %>% 
  dplyr::filter(Contact == "Y") %>% #extract events in which contact occurred
  group_by(Identifier2) %>% #group by event 
  summarise(Contact_duration = sum(Total_behave_duration)) #get duration of contact

Contact2 <- as.data.frame(Contact2) #228 events

#Append to Events_Summed
Events_Summed <- left_join(Events_Summed, Contact2, by = "Identifier2")
remove(Contact)
remove(Contact2)


#Repeat for investigation time
Inv <- Events_Summed
#ID events in which animals contacted the puzzle
Inv$Inv <- "N"
Inv$Inv <- ifelse(Inv$Behave == "inv_cam", "Y", Inv$Inv)
Inv$Inv <- ifelse(Inv$Behave == "inv_puzzle", "Y", Inv$Inv)
Inv$Inv <- ifelse(Inv$Behave == "inv_ground", "Y", Inv$Inv)
Inv$Inv <- ifelse(Inv$Behave == "inv_unk", "Y", Inv$Inv)

#Events where investigation occurred
Inv2 <- Inv %>% 
  dplyr::filter(Inv == "Y") %>%
  group_by(Identifier2) %>% #group by event ID 
  summarise(Inv_duration = sum(Total_behave_duration)) #calculate length of investigation
Inv2 <- as.data.frame(Inv2) #487 events

#Append to Events_Summed
Events_Summed <- left_join(Events_Summed, Inv2, by = "Identifier2")
remove(Inv)
remove(Inv2)


#Next outcome variable will be complexity
Complex <- Events_Summed %>%
  dplyr::filter(Behave == "contact_bottom"|
                  Behave == "contact_spin"|
                  Behave == "contact_top"|
                  Behave == "contact_mid"|
                  Behave == "contact_unk"|
                  Behave == "contact_bungee")
Complex <- Complex %>%
  group_by(Identifier2) %>%
  summarise(Behav_Complexity = n())

Complex1 <- as.data.frame(Complex) #228 events


#Append to Events_Summed
Events_Summed <- left_join(Events_Summed, Complex1, by = "Identifier2")
remove(Complex)
remove(Complex1)


#Next outcome variable is lope/ gallop (fear)
Lope <- Events_Summed %>%
  dplyr::filter(Behave == "lopegallop")
Lope$Lope <- "Y"
Lope <- Lope %>%
  dplyr::select(Identifier2, Lope)

#Append to Events_Summed
Events_Summed <- left_join(Events_Summed, Lope, by = "Identifier2")
remove(Lope)


#Next outcome variable is solves
Solves <- Events_Summed %>%
  dplyr::filter(Behave == "solve") #48 solves total
Solves$Solves <- "Y"
Solves <- Solves %>%
  dplyr::select(Identifier2, Solves)

#Append to Events_Summed
Events_Summed <- left_join(Events_Summed, Solves, by = "Identifier2")
remove(Solves)


#Finally, I want to create a column that identified whether each animal was oriented at some point
Orient <- Events_Summed %>%
  dplyr::filter(Behave == "orient_puzzle"|
                  Behave == "orient_camera"|
                  Behave == "orient_lure"|
                  Behave == "orient_unk")
Orient$Orient <- "Y"
Orient <- Orient %>%
  dplyr::select(Identifier2, Orient)

Orient <- as.data.frame(Orient) #1688 events
Orient <- unique(Orient)

#Append to Events_Summed
Events_Summed <- left_join(Events_Summed, Orient, by = "Identifier2")
remove(Orient)

#Clean up Events_Summed
Events_Summed <- Events_Summed %>%
  dplyr::select(-(c(Behave, Total_behave_duration)))

#Replace NAs with meaningul zeros
Events_Summed$Contact_duration[is.na(Events_Summed$Contact_duration)] <- 0
Events_Summed$Inv_duration[is.na(Events_Summed$Inv_duration)] <- 0
Events_Summed$Lope[is.na(Events_Summed$Lope)] <- "N"
Events_Summed$Solves[is.na(Events_Summed$Solves)] <- "N"
Events_Summed$Orient[is.na(Events_Summed$Orient)] <- "N"

Events_Summed <- unique(Events_Summed)

Events_Summed <- Events_Summed %>%
  dplyr::rename("EventID_Year_Subject" = Identifier2)

#OK, I have some inconsistencies in how that identifier column is, so I have to repair to be able to join
#First, break that column into several constituents
Events_Summed$Site <- sub("_.*", "", Events_Summed$EventID_Year_Subject)
Events_Summed$Subject <- sub(".*_", "", Events_Summed$EventID_Year_Subject)
Events_Summed$YEAR <- sub(".*(202[4-5]).*", "\\1", Events_Summed$EventID_Year_Subject)
Events_Summed$Event <- sub("_202[4-5].*", "", Events_Summed$EventID_Year_Subject)
Events_Summed$AorW <- substr(Events_Summed$Event, nchar(Events_Summed$Event), nchar(Events_Summed$Event))
Events_Summed$EventNo <- sub(".*_?(EVENT\\d+).*", "\\1", Events_Summed$Event)

#Replace N with W
Events_Summed$AorW <- ifelse(Events_Summed$AorW == "N", "W", Events_Summed$AorW)

Events_Summed <- Events_Summed %>%
  dplyr::select(-(EventID_Year_Subject))

#Create new EventID_Year_Subject
Events_Summed$EventID_Year_Subject<- paste(Events_Summed$Site,
                                           Events_Summed$EventNo,
                                           Events_Summed$AorW,
                                           Events_Summed$YEAR,
                                           Events_Summed$Subject,
                                           sep = "_")

Events_Summed$Identifier2<- paste(Events_Summed$Site,
                                           Events_Summed$EventNo,
                                           Events_Summed$AorW,
                                           Events_Summed$YEAR,
                                           sep = "_")
Events_Summed <- Events_Summed %>% dplyr::select(-(YEAR))
##################End of outcome variables


########Complete x variables
FinalData <- left_join(Events_Summed, IndVarsByEventSubjectsCombined, by = "Identifier2")
#remove(IndVarsByEventSubjectsCombined)
remove(Events_Summed)

#Replace values of 1000 for NA in urbanization score column
FinalData$urbanization_score[FinalData$urbanization_score == 1000] <- NA

#Extract Sex from subject
FinalData$Sex <- sub(".*-(.*)\\d$", "\\1", FinalData$Subject)

#Add temperature
temps <- read.csv("climate-daily.csv")
temps <- temps %>%
  dplyr::select(LOCAL_DATE, MEAN_TEMPERATURE) %>%
  dplyr::rename("temp" = MEAN_TEMPERATURE,
                "Date" = LOCAL_DATE)


#Standradize date columns
FinalData$date_only <- as.Date(FinalData$DateTime)
temps$date_only <- as.Date(temps$Date)

FinalData <- left_join(FinalData, temps, by = "date_only")
remove(temps)

#Now get sequence (within SITE)
FinalData$SiteSequence <- sub("EVENT", "", FinalData$EventNo)

#Add disease status
Disease <- read.csv("Disease_From_Boris.csv")
Disease <- unique(Disease)

Disease <- Disease %>% dplyr::rename("Subject2" = UniqueID2)

FinalData$Subject2 <- paste(FinalData$Subject, FinalData$YEAR, sep = "_")
FinalData <- left_join(FinalData, Disease, by = "Subject2", relationship = "many-to-many")
remove(Disease)

#Remove columns that won't serve you and save dataframe
FinalData <- FinalData %>%
  dplyr::select(Contact_duration,
                Inv_duration,
                Behav_Complexity,
                Lope,
                Solves,
                Orient,
                SiteID,
                EventID_Year_Subject,
                Identifier2,
                AorW,
                YEAR,
                LorD,
                GroupSize,
                urbanization_score,
                UrbLevel,
                Sex,
                DateTime,
                temp,
                SiteSequence,
                Subject2,
                Disease,
                Road.density,
                pop_density,
                ANTH,
                NAT,
                Nat50,
                Nat100,
                Nat250)

#Rename some of these columns
FinalData <- FinalData %>%
  dplyr::rename("EventID_Year" = Identifier2,
                "PuzzleType" = AorW,
                "Year" = YEAR,
                "Light" = LorD,
                "urbanization" = UrbLevel,
                "Subject" = Subject2)

FinalData$urbanization <- ifelse(FinalData$urbanization == "Park", "Wild", FinalData$urbanization)

#Strangely, a few events seem to be missing site ID (n = 12)
#I can simply populate this 
FinalData$SiteID[is.na(FinalData$SiteID)] <- sub("_.*", "", FinalData$EventID_Year_Subject[is.na(FinalData$SiteID)])
FinalData$Year[is.na(FinalData$Year)] <- sub(".*(202[4-5]).*", "\\1", FinalData$EventID_Year[is.na(FinalData$Year)])
FinalData$GroupSize[is.na(FinalData$GroupSize)] <- 1
FinalData$urbanization[is.na(FinalData$urbanization)] <- "City"
FinalData$Light[is.na(FinalData$Light)] <- "L"
FinalData$Light <- ifelse(FinalData$Light == "N", "D", FinalData$Light)
FinalData$Subject <- sub("_NA$", "_2025", FinalData$Subject)
FinalData <- FinalData %>%
  group_by(Subject) %>%
  mutate(Disease = ifelse(is.na(Disease), first(na.omit(Disease)), Disease)) %>%
  ungroup()
FinalData$Disease[is.na(FinalData$Disease)] <- "N"
FinalData$temp[is.na(FinalData$temp)] <- -4.5
FinalData <- FinalData %>%
  group_by(SiteID) %>%
  mutate(urbanization_score = ifelse(is.na(urbanization_score), first(na.omit(urbanization_score)), urbanization_score)) %>%
  ungroup()
FinalData$EventIDNoYear <- (sub("^([^_]+)_([^_]+)_.*$", "\\1_\\2", FinalData$EventID_Year_Subject))
FinalData$EventIDYearNoPT <- paste(FinalData$EventIDNoYear, FinalData$Year, sep = "_")

FinalData <- FinalData %>%
  group_by(EventIDYearNoPT) %>%
  mutate(DateTime = ifelse(is.na(DateTime), first(na.omit(DateTime)), DateTime)) %>%
  ungroup()

FinalData <- FinalData %>%dplyr::select(-(c(EventIDNoYear, EventIDYearNoPT)))

na_event_ids <- as.data.frame(FinalData$EventID_Year_Subject[is.na(FinalData$DateTime)])

FinalData$DateTime <- ifelse(FinalData$EventID_Year_Subject == "SE4_EVENT2_W_2025_SE4-U1",
                             "2025-02-19 07:06:06",
                             FinalData$DateTime)

FinalData$DateTime <- ifelse(FinalData$EventID_Year_Subject == "SE4_EVENT4_A_2025_SE4-SF1",
                             "2025-02-21 05:40:08",
                             FinalData$DateTime)

FinalData$DateTime <- ifelse(FinalData$EventID_Year_Subject == "SE7_EVENT49_A_2025_SE7-F3",
                             "2025-03-26 05:46:43",
                             FinalData$DateTime)

FinalData$DateTime <- ifelse(FinalData$EventID_Year_Subject == "SE7_EVENT7_W_2025_SE7-F1",
                             "2025-02-28 03:10:42",
                             FinalData$DateTime)

FinalData$DateTime <- ifelse(FinalData$EventID_Year_Subject == "SW3_EVENT124_W_2025_SW3-F1",
                             "2025-02-09 02:48:20",
                             FinalData$DateTime)

FinalData$DateTime <- ifelse(FinalData$EventID_Year_Subject == "SW3_EVENT7_A_2025_SW3-F1",
                             "2025-01-03 17:18:14",
                             FinalData$DateTime)

FinalData$DateTime <- ifelse(FinalData$EventID_Year_Subject == "SW4_EVENT2_A_2025_SW4-F1",
                             "2025-01-02 23:37:32",
                             FinalData$DateTime)

FinalData$DateTime <- ifelse(FinalData$EventID_Year_Subject == "SW4_EVENT3_A_2025_SW4-M1",
                             "2025-01-03 14:49:28",
                             FinalData$DateTime)

FinalData$DateTime <- ifelse(FinalData$EventID_Year_Subject == "SW4_EVENT4_A_2025_SW4-M1",
                             "2025-01-04 15:56:52",
                             FinalData$DateTime)

FinalData$DateTime <- ifelse(FinalData$EventID_Year_Subject == "SW4_EVENT5_A_2025_SW4-F1",
                             "2025-01-05 12:27:54",
                             FinalData$DateTime)

FinalData$DateTime <- ifelse(FinalData$EventID_Year_Subject == "SW4_EVENT5_A_2025_SW4-F2",
                             "2025-01-05 12:27:54",
                             FinalData$DateTime)

FinalData$DateTime <- ifelse(FinalData$EventID_Year_Subject == "SW4_EVENT5_A_2025_SW4-M1",
                             "2025-01-05 12:27:54",
                             FinalData$DateTime)

FinalData <- as.data.frame(FinalData)

#Convert certain columns to factors
FinalData$Lope <- ifelse(FinalData$Lope == "Y", 1, 0)
FinalData$Solves <- ifelse(FinalData$Solves == "Y", 1, 0)

#Also strange. I have some NA values in some of my urbanization metrics
FinalData <- FinalData %>%
  group_by(SiteID) %>%
  mutate(pop_density = ifelse(is.na(pop_density), 
                              first(na.omit(pop_density)), 
                              pop_density)) %>%
  ungroup()

FinalData <- FinalData %>%
  group_by(SiteID) %>%
  mutate(Road.density = ifelse(is.na(Road.density), 
                              first(na.omit(Road.density)), 
                              Road.density)) %>%
  ungroup()

FinalData <- FinalData %>%
  group_by(SiteID) %>%
  mutate(ANTH = ifelse(is.na(ANTH), 
                              first(na.omit(ANTH)), 
                              ANTH)) %>%
  ungroup()


FinalData <- FinalData %>%
  group_by(SiteID) %>%
  mutate(NAT = ifelse(is.na(NAT), 
                              first(na.omit(NAT)), 
                              NAT)) %>%
  ungroup()

FinalData <- FinalData %>%
  group_by(SiteID) %>%
  mutate(Nat50 = ifelse(is.na(Nat50), 
                              first(na.omit(Nat50)), 
                              Nat50)) %>%
  ungroup()


FinalData <- FinalData %>%
  group_by(SiteID) %>%
  mutate(Nat100 = ifelse(is.na(Nat100), 
                              first(na.omit(Nat100)), 
                              Nat100)) %>%
  ungroup()


FinalData <- FinalData %>%
  group_by(SiteID) %>%
  mutate(Nat250 = ifelse(is.na(Nat250), 
                              first(na.omit(Nat250)), 
                              Nat250)) %>%
  ungroup()


FinalData$Nat50 <- ifelse(FinalData$SiteID == "NE5", 687.2926, FinalData$Nat50 )
FinalData$Nat100 <- ifelse(FinalData$SiteID == "NE5", 1254.457, FinalData$Nat100 )
FinalData$Nat250 <- ifelse(FinalData$SiteID == "NE5", 4928.224, FinalData$Nat250 )

#Revise Nat50 column a little
unique(FinalData$Nat50)
FinalData$Total <- 7854
FinalData <- FinalData %>% dplyr::mutate(PercNat = Nat50/Total*100,
                                         PercAnth = 100-PercNat)

FinalData <- FinalData %>% dplyr::select(-(c(Nat50, PercNat)))
FinalData <- FinalData %>% dplyr::rename("Nat50" = PercAnth)

#write.csv(FinalData, file = "EventDataJul2025.csv")



#########Now create a summary that summarises info from every site
#Remove events in which animal was not oriented:
Events <- FinalData %>%
  dplyr::filter(Orient == "Y")

#Strangely, a few events seem to be missing site ID (n = 12)
#I can simply populate this 

TotalEvents <- Events %>%
  group_by(SiteID) %>%
  summarise(TotalEvents = n())

Solves_by_Site <- Events %>%
  dplyr::filter(Solves == 1) %>%
  group_by(SiteID) %>%
  summarise(Solves = n())

Lope_by_Site <- Events %>%
  dplyr::filter(Lope == 1) %>%
  group_by(SiteID) %>%
  summarise(No.Lope = n())

Events$Contact <- "N"
Events$Contact <- ifelse(Events$Contact_duration > 0, "Y", Events$Contact)
Contact_by_Site <- Events %>%
  dplyr::filter(Contact == "Y") %>%
  group_by(SiteID) %>%
  summarise(No.Contact = n())

Events$Inv <- "N"
Events$Inv <- ifelse(Events$Inv_duration > 0, "Y", Events$Inv)
Inv_by_Site <- Events %>%
  dplyr::filter(Inv == "Y") %>%
  group_by(SiteID) %>%
  summarise(No.Inv = n())

#Total no. behaviours
Behav_by_Site <- Events %>%
  dplyr::filter(Behav_Complexity > 0) %>%
  group_by(SiteID) %>%
  summarise(No.Behav = max(Behav_Complexity, na.rm = TRUE))

#Get list of sites
SiteList <- Events %>% dplyr::select(SiteID, urbanization_score, urbanization)
SiteList <- unique(SiteList)

#Get urbanization metrics for city sites
UrbMetrics <- FinalData %>% 
  dplyr::select(SiteID, Road.density, pop_density, ANTH, NAT, Nat50, Nat100, Nat250)

UrbMetrics <- unique(UrbMetrics)

#Join everyone together
Frames <- list(SiteList,
               TotalEvents,
               Solves_by_Site,
               Lope_by_Site,
               Contact_by_Site,
               Inv_by_Site,
               Behav_by_Site,
               UrbMetrics)

SummaryData <- reduce(Frames, left_join, by = "SiteID")

#remove other things
rm(list = setdiff(ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)], "SummaryData"))

SummaryData <- as.data.frame(SummaryData)

#Populate Real zeroes
SummaryData$Solves[is.na(SummaryData$Solves)] <- 0
SummaryData$No.Lope[is.na(SummaryData$No.Lope)] <- 0
SummaryData$No.Contact[is.na(SummaryData$No.Contact)] <- 0
SummaryData$No.Inv[is.na(SummaryData$No.Inv)] <- 0
SummaryData$No.Behav[is.na(SummaryData$No.Behav)] <- 0

#Add a row for SE3 even though you didn't get any data there
SE3 <- as.data.frame(as.list(rep(0, ncol(SummaryData))))
colnames(SE3) <- colnames(SummaryData)
SE3$SiteID <- "SE3"
SummaryData <- rbind(SummaryData, SE3)
SummaryData$urbanization <- ifelse(SummaryData$SiteID == "SE3", "City", SummaryData$urbanization)
SummaryData$urbanization_score <- ifelse(SummaryData$SiteID == "SE3", -0.873909460814067, SummaryData$urbanization_score)

#write.csv(SummaryData, file = "SiteSummaryDataJul2025.csv")
