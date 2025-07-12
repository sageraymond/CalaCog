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

YEGData <- YEGData %>% dplyr::select(Site_ID, Common_ID, urbanization_score)

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
  select(Identifier2, Orient)

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
                Disease)

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

#Get list of sites
SiteList <- Events %>% dplyr::select(SiteID, urbanization_score, urbanization)
SiteList <- unique(SiteList)

#I can't think of a logical way to do this for complexity...

#Join everyone together
Frames <- list(SiteList,
               TotalEvents,
               Solves_by_Site,
               Lope_by_Site,
               Contact_by_Site,
               Inv_by_Site)

SummaryData <- reduce(Frames, left_join, by = "SiteID")
rm(list = setdiff(ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)], "SummaryData"))

SummaryData <- as.data.frame(SummaryData)

#Populate Real zeroes
SummaryData$Solves[is.na(SummaryData$Solves)] <- 0
SummaryData$No.Lope[is.na(SummaryData$No.Lope)] <- 0
SummaryData$No.Contact[is.na(SummaryData$No.Contact)] <- 0
SummaryData$No.Inv[is.na(SummaryData$No.Inv)] <- 0

#Add a row for SE3 even though you didn't get any data there
SE3 <- as.data.frame(as.list(rep(0, ncol(SummaryData))))
colnames(SE3) <- colnames(SummaryData)
SE3$SiteID <- "SE3"
SummaryData <- rbind(SummaryData, SE3)
SummaryData$urbanization <- ifelse(SummaryData$SiteID == "SE3", "City", SummaryData$urbanization)
SummaryData$urbanization_score <- ifelse(SummaryData$SiteID == "SE3", -0.873909460814067, SummaryData$urbanization_score)

#write.csv(SummaryData, file = "SiteSummaryDataJul2025.csv")



















#############BELOW THIS LINE IS ALL CRAP THAT LIKELY WON'T EVER BE USEFUL

#Add SiteID
Complex1$SiteID <- gsub("\\_.*","",Complex1$EventID)

#Add urbanization, remove sites with no contact events
Complex2 <- full_join(Complex1, Urb_Levels, by = "SiteID") %>% na.omit(Complex2)


#Get mean contact behaviours by site ID
Complex3 <- Complex2 %>%
  group_by(SiteID) %>%
  summarise(Mean_Complexity = mean(Count_Contact_Behaviours))

Complex3 <- as.data.frame(Complex3)

Complex4 <- left_join(Complex3, Urb_Levels, by = "SiteID")






#





Events_Summed <- left_join(Events_Summed, Urb_Levels, by = "site_i")
Events_Summed$site_id <- gsub("\\_.*","",Events_Summed$Identifier2)
Events_Summed$Subject <- sub(".*_", "", Events_Summed$Identifier2)
Events_Summed$Identifier3 <- sub("_[^_]+$", "", Events_Summed$Identifier2)


#What I really want is a dataframe with each of my outcome variables and predictors

















#Create SiteID column
Contact_by_Event$Site_ID <- gsub("\\_.*","",Contact_by_Event$EventID)

#To get contact by urbanization level, attach this to Urbanization levels
Contact_by_Urb <- left_join(Contact_by_Event, Urb_Levels, by = "Site_ID")

#Make a plot showing mean persistence time at park vs. city
PersistenceChartData <- Contact_by_Urb %>%
  group_by(UrbLevel) %>%
  summarise(mean_contact = mean(Contact_duration),
            SD = sd(Contact_duration))
PersistenceChartData <- as.data.frame(PersistenceChartData)
PersistenceChartData$UrbLevel <- ifelse(PersistenceChartData$UrbLevel!= "City", "Wild", PersistenceChartData$UrbLevel)
PerisenceChartMeans <- ggplot(data = PersistenceChartData,
       mapping = aes(y=mean_contact, x=UrbLevel)) +
  geom_bar(fill="#33CCFF", stat = "identity", colour = "black") +
  theme_classic() +
  geom_errorbar(aes(ymin = mean_contact - SD, ymax = mean_contact + SD), width = 0.2) + 
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none") +
  labs(x="Urb. Level", y="Mean contact time (s)") + 
  ggtitle("A. Persistence (City vs. Wild)")

#The analagous chart for the continuous metric would be a scatter plot... I think
PersistenceChartData2 <- Contact_by_Urb %>%
  dplyr::filter(UrbLevel != "Park") %>%
  group_by(Site_ID) %>%
  summarise(mean_contact = mean(Contact_duration))
PersistenceChartData2 <- as.data.frame(PersistenceChartData2)
PersistenceChartData2 <- left_join(PersistenceChartData2, YEGUrbScores, by = "Site_ID")

PerisenceChartLine <- ggplot(PersistenceChartData2, aes(x = urbanization_score, y = mean_contact)) +
  geom_point(color = "#33CCFF", size = 2) +  
  theme_classic() +
  geom_smooth(method = "lm", color = "black", fill = "#33CCFF") + 
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none") +
  labs(x="Urb. Score", y="Mean contact time (s)") + 
  ggtitle("B. Persistence (City Only)")

#Modelling will happen later
#Remove some stuff:
remove(Contact_by_Event)
remove(PersistenceChartData)
remove(PersistenceChartData2)

#Model it now
#Step 1. Should I include the 0s? Or perhaps do a hurdle model?
#Let's start by removeing events in which the animal was never oriented towards the puzzle
#Those are events where it's safe to say they didn't notice the puzzle
#Make a list of events that DO include orient_puzzle OR orient_camera
Orient <- Events_Summed %>%
  dplyr::filter(Behave == "orient_puzzle" |
                  Behave == "orient_camera") %>%
  dplyr::select(EventIDSubject)
Orient <- unique(Orient) #1146 events in which coyote was oriented at some point

Contact_by_Urb1 <- left_join(Orient, Contact_by_Urb, by = "EventIDSubject")
Contact_by_Urb1 <- Contact_by_Urb1 %>% dplyr::select(EventIDSubject, Contact_duration)
Contact_by_Urb1$Contact_duration[is.na(Contact_by_Urb1$Contact_duration)] <- 0
remove(Contact_by_Urb)








#Let's move on to behavioural complexity
#First metric of behavioural complexity is the number of behaviours exhibited while contacting
Complex1 <- Events_Summed %>% #start with events_summed
  dplyr::filter(Contact == "Y") %>% #extract events caharcterised by some form of contact
  dplyr::select(EventID, Behave) %>% #remove unecessary columns
  group_by(EventID) %>% summarise(Count_Contact_Behaviours = n())

Complex1 <- as.data.frame(Complex1) #only 58 events included some form of contact

#Add SiteID
Complex1$SiteID <- gsub("\\_.*","",Complex1$EventID)

#Add urbanization, remove sites with no contact events
Complex2 <- full_join(Complex1, Urb_Levels, by = "SiteID") %>% na.omit(Complex2)


#Get mean contact behaviours by site ID
Complex3 <- Complex2 %>%
  group_by(SiteID) %>%
  summarise(Mean_Complexity = mean(Count_Contact_Behaviours))

Complex3 <- as.data.frame(Complex3)

Complex4 <- left_join(Complex3, Urb_Levels, by = "SiteID")

#Summarise this in a model
#First park vs city
Complex_mod_1 <- glm(Mean_Complexity ~ UrbLevel, data = Complex4)
summ(Complex_mod_1, digits = 3)

Complex5 <- Complex4 %>% dplyr::filter(UrbLevel == "City")
Complex_mod_2 <- glm(Mean_Complexity ~ urbanization_score, data = Complex5)
summ(Complex_mod_2, digits = 3)

#Make relevant plots
ComplexChartData1 <- Complex4 %>%
  group_by(UrbLevel) %>%
  summarise(mean_complex = mean(Mean_Complexity),
            SD = sd(Mean_Complexity))
ComplexChartData1 <- as.data.frame(ComplexChartData1)
ggplot(data = ComplexChartData1,
       mapping = aes(y=mean_complex, x=UrbLevel)) +
  geom_bar(fill="#33CCFF", stat = "identity", colour = "black") +
  theme_classic() +
  geom_errorbar(aes(ymin = mean_complex - SD, ymax = mean_complex + SD), width = 0.2) + 
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none") +
  labs(x="Urb. Level", y="Mean behavioural complexity") + 
  ggtitle("Behavioural Complexity (Park vs. City)")


A <- plot_model(Complex_mod_2, type = "pred", terms = "urbanization_score[all]", 
                colors = "#33CCFF")
#Make prettier
B <- A + theme_classic() + ggtitle("Behavioural Complexity (within City)") + 
  theme(axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        plot.title = element_text(size = 12, face = "bold")) +
  labs(x = "Urbanization level", y = "Mean Behavioural Complexity")








Complex2$Urbanization <- factor(Complex2$Urbanization, levels = c("Moderate", "Park", "Very"))
Complex_mod <- glmer(Count_Contact_Behaviours ~ (1|SiteID) + Urbanization, data = Complex2, family = "poisson")
summ(Complex_mod, digits = 3)


#My second complexity indicator is the number of gait changes. 
#Begin with the Events dataframe and go from there

Gaits <- Events %>%
  dplyr::select(EventID, Behave, SiteID) %>%
  dplyr::filter(Behave == "walk" | Behave == "trot" | Behave == "still" | Behave == "lopegallop") %>%
  #up to now, we have a datafram that has only locomotion in it, with selected columns
  #to get the number of different gaits, we can just group by EventID and then count the number of rows
  group_by(EventID) %>% summarise(Gait_Count = n())
Gaits <- as.data.frame(Gaits) #459 events can be considered here

#Add SiteID
Gaits$SiteID <- gsub("\\_.*","",Gaits$EventID)

#Add urbanization
Gaits2 <- full_join(Gaits, Urb_Levels, by = "SiteID")

Gaits3 <- na.omit(Gaits2) #remove any NA values (i.e., sites with no events)

#Summarise as a model (poisson because count data)
#Start by setting moderate as reference category
Gaits3$Urbanization <- factor(Gaits3$Urbanization, levels = c("Moderate", "Park", "Very"))
Complex_mod1 <- glmer(Gait_Count ~ (1|SiteID) + Urbanization, data = Gaits3, family = "poisson")
summ(Complex_mod1, digits = 3)



#Moving on to exploration. First metric of exploration is investigate time
#Calculate investigate time for each event
Events_Summed$Inv <- "N"
Events_Summed$Inv <- ifelse(Events_Summed$Behave == "inv_cam", "Y", Events_Summed$Inv)
Events_Summed$Inv <- ifelse(Events_Summed$Behave == "inv_puzzle", "Y", Events_Summed$Inv)
Events_Summed$Inv <- ifelse(Events_Summed$Behave == "inv_ground", "Y", Events_Summed$Inv)
Events_Summed$Inv <- ifelse(Events_Summed$Behave == "inv_unk", "Y", Events_Summed$Inv)

#Should I retain events in which invetsigation occurred, or look at all? Let's do both
#Events where investigation occurred
Inv_by_Event <- Events_Summed %>% 
  dplyr::filter(Inv == "Y") %>% #consider only events where investigation occurred
  group_by(EventID) %>% #group by event ID 
  summarise(Inv_duration = sum(Total_behave_duration)) #calculate length of investigation
Inv_by_Event <- as.data.frame(Inv_by_Event) #140 events can be considered

#Create SiteID column
Inv_by_Event$SiteID <- gsub("\\_.*","",Inv_by_Event$EventID)

#To get contact by urbanization level, attach this to UrbData
Inv_by_Urb <- full_join(Urb_Levels, Inv_by_Event, by = "SiteID")
Inv_by_Urb <- na.omit(Inv_by_Urb)

#Model
Inv_by_Urb$UrbLevel <- factor(Inv_by_Urb$UrbLevel, levels = c("Park", "City"))
Explore1 <- glm(Inv_duration ~ (1|SiteID) + Urbanization, data = Inv_by_Urb)
summ(Explore1, digits = 3)



#Should I retain events in which invetsigation occurred, or look at all? Let's do both
#Events where investigation occurred + events when it didn't
EventList <- Events_Summed %>% dplyr::select(EventID) 
EventList <- unique(EventList) #458 unique events

#the dataframe Inv_by_Event includes inevstigation length for all events in which
#invetsigation occurred. Now merge it with the event list, and give 0 to other events
Inv_by_Event2 <- full_join(EventList, Inv_by_Event, by = "EventID")

#replace Nas with 0 (i.e., 0 iinvetsigate time)
Inv_by_Event2$Inv_duration[is.na(Inv_by_Event2$Inv_duration)] <- 0

#Create SiteID column
Inv_by_Event2$SiteID <- gsub("\\_.*","",Inv_by_Event2$EventID)

#To get contact by urbanization level, attach this to UrbData
Inv_by_Urb2 <- full_join(Urb_Levels, Inv_by_Event2, by = "SiteID")
Inv_by_Urb2 <- na.omit(Inv_by_Urb2)

#Now get mean investigation time
InvData <- Inv_by_Urb2 %>%
  group_by(SiteID) %>%
  summarise(MeanInvTime = mean(Inv_duration))

InvData <- as.data.frame(InvData)

InvData1 <- left_join(InvData, Urb_Levels, by = "SiteID")

#First model is for city vs park
InvMod1 <- glm(MeanInvTime ~ UrbLevel, data = InvData1)
summ(InvMod1, digits = 3)

InvData2 <- InvData1 %>%
  dplyr::filter(UrbLevel == "City")
InvMod2 <- glm(MeanInvTime ~ urbanization_score, data = InvData2)
summ(InvMod2, digits = 3)

#Make relevant plots
InvChartData1 <- InvData1 %>%
  group_by(UrbLevel) %>%
  summarise(mean_inv = mean(MeanInvTime),
            SD = sd(MeanInvTime))
InvChartData1 <- as.data.frame(InvChartData1)
ggplot(data = InvChartData1,
       mapping = aes(y=mean_inv, x=UrbLevel)) +
  geom_bar(fill="#33CCFF", stat = "identity", colour = "black") +
  theme_classic() +
  geom_errorbar(aes(ymin = mean_inv - SD, ymax = mean_inv + SD), width = 0.2) + 
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none") +
  labs(x="Urb. Level", y="Mean investigate time (s)") + 
  ggtitle("Exploration (Park vs. City)")


A <- plot_model(InvMod2, type = "pred", terms = "urbanization_score[all]", 
                colors = "#33CCFF")
#Make prettier
B <- A + theme_classic() + ggtitle("Exploration (within City)") + 
  theme(axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        plot.title = element_text(size = 12, face = "bold")) +
  labs(x = "Urbanization level", y = "Mean Investigate Time (s)")





#Repeat exlcudiung 0s
InvData3 <- Inv_by_Urb2 %>%
  dplyr::filter(Inv_duration != 0) %>%
  group_by(SiteID) %>%
  summarise(MeanInvTime = mean(Inv_duration))

InvData3 <- as.data.frame(InvData3)

InvData3 <- left_join(InvData3, Urb_Levels, by = "SiteID")

#First model is for city vs park
InvMod3 <- glm(MeanInvTime ~ UrbLevel, data = InvData3)
summ(InvMod3, digits = 3)

InvData4 <- InvData3 %>%
  dplyr::filter(UrbLevel == "City")
InvMod4 <- glm(MeanInvTime ~ urbanization_score, data = InvData4)
summ(InvMod4, digits = 3)



#Model
Inv_by_Urb2$Urbanization <- factor(Inv_by_Urb2$Urbanization, levels = c("Moderate", "Park", "Very"))
Explore2 <- lmer(Inv_duration ~ (1|SiteID) + Urbanization, data = Inv_by_Urb2)
summ(Explore2, digits = 3) #note that this is probably zero inflated


#Another way to get at this is the number of events charaterised by investigate!
#Simply summarise the number of events characterised by invetsigate at each site
Events_Summed$SiteID <- gsub("\\_.*","",Events_Summed$EventID)

Inv_by_Urb3 <- Events_Summed %>%
  dplyr::filter(Inv == "Y") %>%
  group_by(SiteID) %>%
  summarise(no.Inv = n())

Inv_by_Urb4 <- full_join(Inv_by_Urb3, Urb_Levels, by = "SiteID")
Inv_by_Urb4 <- as.data.frame(Inv_by_Urb4)
Inv_by_Urb4$no.Inv[is.na(Inv_by_Urb4$no.Inv)] <- 0
Inv_by_Urb4 <- na.omit(Inv_by_Urb4)
Inv_by_Urb4 <- left_join(Inv_by_Urb4, sitedata, by = "SiteID")


#First model is for city vs park (which needs to be corrected for no. events)
InvMod3 <- glm(no.Inv ~ UrbLevel.y, data = Inv_by_Urb4, offset = Total_Events)
summ(InvMod3, digits = 3)

Inv_by_Urb5 <- Inv_by_Urb4 %>%
  dplyr::filter(UrbLevel.y == "City")
InvMod4 <- glm(no.Inv ~ urbanization_score.y, data = Inv_by_Urb5, offset = Total_Events)
summ(InvMod4, digits = 3)




#MODEL
Inv_by_Urb4$Urbanization <- factor(Inv_by_Urb4$Urbanization, levels = c("Moderate", "Park", "Very"))
Explore3 <- glm(no.Inv ~ Urbanization, data = Inv_by_Urb4, 
                offset = log(Total_Events), family = "poisson")
summ(Explore3, digits = 3)


#Next exploration metric is the number of plastic puzzles (offset by total events)
UrbData$Urbanization <- factor(UrbData$Urbanization, levels = c("Moderate", "Park", "Very"))
UrbData1 <- UrbData %>% dplyr::filter(SiteID != "E4") 

Explore4 <- glm(Plastic ~ Urbanization, data = UrbData1, 
                offset = log(Total_Events), family = "poisson")
summ(Explore4, digits = 3)


####SHYNESS##
#First shyness variable is latency to first approach
sitedata$UrbLevel <- factor(sitedata$UrbLevel, levels = c("Park", "City"))
Shy1 <- glm(Days_To_First_Event ~ UrbLevel, data = sitedata, family = "poisson")
summ(Shy1, digits = 3)

sitedata1 <- sitedata %>% dplyr::filter(UrbLevel == "City")
Shy2 <- glm(Days_To_First_Event ~ urbanization_score, data = sitedata1, family = "poisson")
summ(Shy2, digits = 3)

#######
#######
#######

#Second shyness variables is frequency of galloping (i.e., events with gallop, offset by events)
Gallop <- Events_Summed %>%
  dplyr::filter(Behave == "lopegallop")

#Create SiteID column
Gallop$SiteID <- gsub("\\_.*","",Gallop$EventID)

Gallop2 <- Gallop %>% group_by(SiteID) %>%
  summarise(No.gallop = n()) 

Gallop2 <- as.data.frame(Gallop2) #now we have no gallop events at each site

#Join with UrbData
Gallop3 <- full_join(Gallop2, Urb_Levels, by = "SiteID") %>%
  dplyr::filter(SiteID != "E4")
Gallop3$No.gallop[is.na(Gallop3$No.gallop)] <- 0
Gallop4 <- left_join(Gallop3, sitedata, by = "SiteID")


#Model
Shy3 <- glm(No.gallop ~ UrbLevel.y, data = Gallop4, family = "poisson", 
            offset = log(Total_Events))
summ(Shy3, digits = 3)


Gallop5 <- Gallop4 %>%
  dplyr::filter(UrbLevel.y == "City")
Shy4 <- glm(No.gallop ~ urbanization_score.y + offset(log(Total_Events)), data = Gallop5, family = "poisson")
summ(Shy4, digits = 3)

#Make relevant charts
ShyChartData1 <- Gallop4 %>%
  dplyr::mutate(percgallop = No.gallop/ Total_Events*100) %>%
  group_by(UrbLevel.y) %>%
  summarise(mean_perc_gallop = mean(percgallop),
            SD = sd(percgallop))
ShyChartData1 <- as.data.frame(ShyChartData1)
ggplot(data = ShyChartData1,
       mapping = aes(y=mean_perc_gallop, x=UrbLevel.y)) +
  geom_bar(fill="#33CCFF", stat = "identity", colour = "black") +
  theme_classic() +
  geom_errorbar(aes(ymin = mean_perc_gallop - SD, ymax = mean_perc_gallop + SD), width = 0.2) + 
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none") +
  labs(x="Urb. Level", y="Mean Events with Run (%)") + 
  ggtitle("Fearfulness (Park vs. City)")


A <- plot_model(Shy4, type = "pred", terms = "urbanization_score.y[all]", 
                colors = "#33CCFF")
#Make prettier
B <- A + theme_classic() + ggtitle("Fearfulness (within City)") + 
  theme(axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        plot.title = element_text(size = 12, face = "bold")) +
  labs(x = "Urbanization level", y = "Mean contact time (s)")


#Third shyness variable is no. nocturnal events
Shy2 <- glm(Dark ~ Urbanization, data = UrbData1, family = "poisson", 
            offset = log(Total_Events))
summ(Shy2, digits = 3) #





#Next Up is problem solving
#Model number of solves offset by number of events
Solves <- Events_Summed %>%
  dplyr::filter(Behave == "solve") %>%
  group_by(EventID) %>%
  summarise(No.Solves = n())

#Create SiteID
Solves$SiteID <- gsub("\\_.*","",Solves$EventID)

Solves <- Solves %>%
  group_by(SiteID) %>%
  summarise(Total_Solves = n())

Solves1 <- full_join(Solves, sitedata, by = "SiteID")
Solves1$Total_Solves[is.na(Solves1$Total_Solves)] <- 0
Solves1 <- as.data.frame(Solves1)

Solves2 <- Solves1 %>%
  dplyr::filter(UrbLevel == "City")

SolveMod <- glm(Total_Solves ~ urbanization_score + offset(log(Total_Events)), 
                data = Solves2, family = "poisson")
summ(SolveMod, digits = 3) #



#Make relevant charts
SolveData <- Solves1 %>%
  group_by(UrbLevel) %>%
  summarise(NoSolves = sum(Total_Solves))
SolveData <- as.data.frame(SolveData)
ggplot(data = SolveData,
       mapping = aes(y=NoSolves, x=UrbLevel)) +
  geom_bar(fill="#33CCFF", stat = "identity", colour = "black") +
  theme_classic() +
  theme(axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        legend.text = element_text(colour = "black", face = "plain", size = 12),
        legend.title = element_blank(),
        legend.position = "none") +
  labs(x="Urb. Level", y="No. Solves") + 
  ggtitle("Problem Solving (Park vs. City)")


A <- plot_model(SolveMod, type = "pred", terms = "urbanization_score[all]", 
                colors = "#33CCFF")
#Make prettier
B <- A + theme_classic() + ggtitle("Problem Solving (within City)") + 
  theme(axis.title.y = element_text(colour = "black", face = "plain", size = 12),
        axis.title.x = element_text(colour = "black", face = "plain", size = 12),
        axis.text.y = element_text(colour = "black", face = "plain", size = 12),
        axis.text.x = element_text(colour = "black", face = "plain", size = 12),
        plot.title = element_text(size = 12, face = "bold")) +
  labs(x = "Urbanization level", y = "No. Solves (offset by total events)")




#OK, that's the end of the landscape piece. Now remove irrelevant objects
remove(Shy)
remove(Shy1)
remove(Shy2)
remove(SolveMod)
remove(Solves)
remove(Solves1)
remove(Complex_mod)
remove(Complex_mod1)
remove(Complex1)
remove(Complex2)
remove(Explore1)
remove(Explore2)
remove(Explore3)
remove(Explore4)
remove(Contact_by_Event)
remove(Contact_by_Urb)
remove(Gaits)
remove(Gaits2)
remove(Gaits3)
remove(Gallop)
remove(Gallop2)
remove(Gallop3)
remove(Inv_by_Event)
remove(Inv_by_Event2)
remove(Inv_by_Urb)
remove(Inv_by_Urb2)
remove(Inv_by_Urb3)
remove(Inv_by_Urb4)
remove(Orient)
remove(Orient2)
remove(Persistence_mod)
remove(Persistence2_mod)

#Get some stuff just for ACTWS abstract : P
#Total events
sum(LocData$Total_Events)

#try to get number of investigate, contact, solve
Events_Summed_Inv <- Events_Summed %>% dplyr::filter(Inv == "Y")
Events_Summed_Inv <- distinct(Events_Summed_Inv, EventID, .keep_all = TRUE)

Events_Summed_Contact <- Events_Summed %>% dplyr::filter(Contact == "Y")
Events_Summed_Contact <- distinct(Events_Summed_Contact, EventID, .keep_all = TRUE)

Events_Summed_Solve <- Events %>% dplyr::filter(Behave == "solve")
Events_Summed_Solve <- distinct(Events_Summed_Solve, SiteEventID, .keep_all = TRUE)

#Let's just compare the five things between urban and non urban for now
#Behavioural complexity, measured as number of different behaviours used when contacting
Contact <- Events %>% dplyr::filter(Behave == "contact_top" |
                                      Behave == "contact_bottom" |
                                      Behave == "contact_mid"|
                                      Behave == "contact_bungee"|
                                      Behave == "contact_spin")
Contact1 <- Contact %>%
  group_by(SiteEventID, Behave) %>%
  summarise(n = n())
Contact1 <- as.data.frame(Contact1)

Contact2 <- Contact1 %>%
  group_by(SiteEventID) %>%
  summarise(no.contact.behave = n())

Contact2$SiteID <- gsub("\\_.*","",Contact2$SiteEventID)
Contact2 <- as.data.frame(Contact2)

Contact3 <- left_join(Contact2, Urb_Levels, by = "SiteID")
Contact3$Urbanization <- ifelse(Contact3$Urbanization == "Park", "Wild", "City")
t.test(no.contact.behave~Urbanization, data = Contact3)
#t = 0.47188, df = 8.6224, p-value = 0.6487

#Exploration = investigate time (omit instances with no investigation)
Investigate <- Events %>% dplyr::filter(Behave == "inv_puzzle" |
                                      Behave == "inv_ground" |
                                      Behave == "inv_cam"|
                                      Behave == "inv_unk")
Investigate1 <- Investigate %>%
  group_by(SiteEventID) %>%
  summarise(Total.Inv.Time = sum(Behave_Duration))
Investigate1 <- as.data.frame(Investigate1)

Investigate1$SiteID <- gsub("\\_.*","",Investigate1$SiteEventID)
Investigate1 <- as.data.frame(Investigate1)

Investigate2 <- left_join(Investigate1, Urb_Levels, by = "SiteID")
Investigate2$Urbanization <- ifelse(Investigate2$Urbanization == "Park", "Wild", "City")
t.test(Total.Inv.Time~Urbanization, data = Investigate2)
#t = 0.22061, df = 30.062, p-value = 0.8269


# Persistence = duration of contact time
Contact4 <- Contact %>%
  group_by(SiteEventID) %>%
  summarise(Total.Cont.Time = sum(Behave_Duration))
Contact4 <- as.data.frame(Contact4)

Contact4$SiteID <- gsub("\\_.*","",Contact4$SiteEventID)
Contact4 <- as.data.frame(Contact4)

Contact5 <- left_join(Contact4, Urb_Levels, by = "SiteID")
Contact5$Urbanization <- ifelse(Contact5$Urbanization == "Park", "Wild", "City")
t.test(Total.Cont.Time~Urbanization, data = Contact5)
# t = 1.5072, df = 23.94, p-value = 0.1448


#Fear/ shyness
LocData$Urbanization <- ifelse(LocData$Letter == "E", "Wild", "City")
t.test(Days_To_First_Event~Urbanization, data = LocData)
#t = -2.7015, df = 22.058, p-value = 0.01302


#problem solving
Solve <- Events_Summed_Solve %>%
  group_by(SiteID) %>%
  summarise(no.solves = n())

##########I'll haver to do that but with urbanization as a continuous
PC <- YEGData2 %>%
  dplyr::select(SiteID, development)
#No contact behaviuors (behavioural complexity)
Contact6 <- inner_join(PC, Contact3, by = "SiteID")
ComplexityMod <- glm(no.contact.behave ~ development, data = Contact6, family = "poisson")
summary(ComplexityMod) #b = Events_Summed_Solve, P = 0.794

#Investigate (exploratopnm)
Investigate3 <- inner_join(PC, Investigate2, by = "SiteID")
InvestigateMod <- glm(Total.Inv.Time ~ development, data = Investigate3)
summary(InvestigateMod) #b = 5.835      , P = 0.0642 

#persistence
Persistence3 <- inner_join(PC, Contact5, by = "SiteID")
PersistenceMod <- glm(Total.Cont.Time ~ development, data = Persistence3)
summary(PersistenceMod) #b = 3.534      , P = 0.254686 

#Shyness
LocData2 <- inner_join(PC, LocData, by = "SiteID")
ShynessMod <- glm(Days_To_First_Event ~ development, data = LocData2, family = "poisson")
summary(ShynessMod) #b =  -3.531      , P = 0.000414  

#problem solving
Solve2 <- left_join(PC, Solve, by = "SiteID")
Solve2$no.solves[is.na(Solve2$no.solves)] <- 0

SolveMod <- glm(no.solves ~ development, data = Solve2, family = "poisson")
summary(SolveMod) #b =  0.3925      , P = 0.04423  



#Now, let's work on the event level analysis

#Begin with the effect of EXTRINSIC variables on each of your five things
#Extrinsic variables are as follows: urbanization level, darkness, puzzle type, group size, whether it is the first puzzle

#Start by ensuring you have all this info for every event
#Make a column that contains both event AND individual
Events$EventIDSubject <- paste(Events$EventID, Events$Subject, sep = "_")

#Reads in data file containing independent variables for each event
Ind <- read.csv("All_Independent_Vars.csv")
Ind <- Ind %>%dplyr::rename("EventID" = Identifier2)

#Join event info and Independent variables
Event_Data <- full_join(Events, Ind, by = "EventID")

#Remove one null event
Event_Data <- Event_Data %>%
  dplyr::filter(EventID != "NW6_EVENT37_W")

#Clean this up
Event_Data <- Event_Data %>%
  dplyr::select(EventID, Date, SiteEventID, Behave, SiteID.x, Behave_Duration, 
                Subject, EventIDSubject, PuzzleType.y, LorD, GroupSize, agesex,
                Disease, PuzzleSequence) %>%
  dplyr::rename("SiteID" = SiteID.x,
                "PuzzleType" = PuzzleType.y)

#Add urbanization level (first extrinsic variable)
Event_Data <- full_join(Event_Data, Urb_Levels, by = "SiteID")

#Create a dataframe that includes ONLY EventIDSubject and related info
Event_Data_Red <- Event_Data %>%
  dplyr::select(EventIDSubject, PuzzleType, LorD, GroupSize, agesex, Disease, 
                PuzzleSequence, Urbanization, SiteID, Subject)

Event_Data_Red <- unique(Event_Data_Red)

#Darkness, puzzle type, and group size, and sequence are already included in the df

#Create list of events (by subject)
EventSubjectList <- Event_Data %>% dplyr::select(EventIDSubject)
EventSubjectList <- unique(EventSubjectList) #539 events


#Metric 1 = PERSISTENCE
#The first outcome variable here is the length of time the animal contacts the puzzle
Event_Data_Summed <- Event_Data %>% group_by(EventIDSubject, Behave) %>% 
  summarise(Total_behave_duration = sum(Behave_Duration))

Event_Data_Summed <- as.data.frame(Event_Data_Summed)
Event_Data_Summed$Contact <- "N"
Event_Data_Summed$Contact <- ifelse(Event_Data_Summed$Behave == "contact_top", "Y", Event_Data_Summed$Contact)
Event_Data_Summed$Contact <- ifelse(Event_Data_Summed$Behave == "contact_bottom", "Y", Event_Data_Summed$Contact)
Event_Data_Summed$Contact <- ifelse(Event_Data_Summed$Behave == "contact_mid", "Y", Event_Data_Summed$Contact)
Event_Data_Summed$Contact <- ifelse(Event_Data_Summed$Behave == "contact_bungee", "Y", Event_Data_Summed$Contact)
Event_Data_Summed$Contact <- ifelse(Event_Data_Summed$Behave == "contact_spin", "Y", Event_Data_Summed$Contact)

E_Contact <- Event_Data_Summed %>%
  dplyr::filter(Contact == "Y") %>%
  dplyr::select(EventIDSubject, Total_behave_duration) %>%
  group_by(EventIDSubject) %>%
  summarise(Contact_Duration = sum(Total_behave_duration))

#Note: Here, we are ONLY interested in events where some contact occurred! SO this df is complete
#Join this df with the DF with all the other variables

E_Contact2 <- full_join(E_Contact, Event_Data_Red, by = "EventIDSubject") %>%
  na.omit(E_Contact2)
E_Contact2 <- as.data.frame(E_Contact2)

#Create model explaining persistence as a function of each extrinsic variable
E_Contact2$Urbanization <- factor(E_Contact2$Urbanization, levels = c("Moderate", "Park", "Very"))
E_Contact2$PuzzleSequence <- ifelse(E_Contact2$PuzzleSequence == 0, 1, E_Contact2$PuzzleSequence)

PersMod1 <- lmer(Contact_Duration ~ (1|SiteID/Subject) + Urbanization, data = E_Contact2)
summ(PersMod1, digits = 3)

PersMod2 <- lmer(Contact_Duration ~ (1|SiteID/Subject) + PuzzleType, data = E_Contact2)
summ(PersMod2, digits = 3)

PersMod3 <- lmer(Contact_Duration ~ (1|SiteID/Subject) + LorD, data = E_Contact2)
summ(PersMod3, digits = 3)

PersMod4 <- lmer(Contact_Duration ~ (1|SiteID/Subject) + GroupSize, data = E_Contact2)
summ(PersMod4, digits = 3)

PersMod5 <- lmer(Contact_Duration ~ (1|SiteID/Subject) + PuzzleSequence, data = E_Contact2)
summ(PersMod5, digits = 3)

#All of these models generate a notice about singular fit, which suggests that the models
#are overfitted. These warnings dissapear if I remove the complicated randome effect
#Leave for now, but keep in mind for future

#Remove unecessry dataframes
remove(E_Contact)
remove(E_Contact2)
remove(PersMod1)
remove(PersMod2)
remove(PersMod3)
remove(PersMod4)
remove(PersMod5)



#Second outcome variable to test is Behavioural complexity.
#First metric is number of behaviours exhibited while contacting the puzzle 
#Restrict to events with Contact

#Start with event data summed
BCData <- Event_Data_Summed %>%
  dplyr::filter(Contact == "Y") %>%
  group_by(EventIDSubject) %>%
  summarise(No.Contact.Behave = n())

BCData <- as.data.frame(BCData)

#Join to Event Data Red
BCData1 <- left_join(BCData, Event_Data_Red, by = "EventIDSubject")

#Create model explaining behavioural complexity as a function of each extrinsic variable
BCData1$Urbanization <- factor(BCData1$Urbanization, levels = c("Moderate", "Park", "Very"))
BCData1$PuzzleSequence <- ifelse(BCData1$PuzzleSequence == 0, 1, BCData1$PuzzleSequence)

BCMod1 <- glmer(No.Contact.Behave ~ (1|SiteID/Subject) + Urbanization, family = "poisson", data = BCData1)
summ(BCMod1, digits = 3)

BCMod2 <- glmer(No.Contact.Behave ~ (1|SiteID/Subject) + PuzzleType, family = "poisson", data = BCData1)
summ(BCMod2, digits = 3)

BCMod3 <- glmer(No.Contact.Behave ~ (1|SiteID/Subject) + LorD, family = "poisson", data = BCData1)
summ(BCMod3, digits = 3)

BCMod4 <- glmer(No.Contact.Behave ~ (1|SiteID/Subject) + GroupSize, family = "poisson", data = BCData1)
summ(BCMod4, digits = 3)

BCMod5 <- glmer(No.Contact.Behave ~ (1|SiteID/Subject) + PuzzleSequence, family = "poisson", data = BCData1)
summ(BCMod5, digits = 3)

#Same thing as before with singular fits!


#Try a second metric of behavioural complexity = number of gait changes
BCData3 <- Event_Data %>%
  dplyr::filter(Behave == "walk" | Behave == "trot" | Behave == "still" | Behave == "lopegallop") %>%
  group_by(EventIDSubject) %>%
  summarise(No.Gaits = n())
  

#Join to Event Data Red
BCData4 <- left_join(BCData3, Event_Data_Red, by = "EventIDSubject")
BCData4 <- as.data.frame(BCData4)

#Create model explaining behavioural complexity as a function of each extrinsic variable
BCData4$Urbanization <- factor(BCData4$Urbanization, levels = c("Moderate", "Park", "Very"))
BCData4$PuzzleSequence <- ifelse(BCData4$PuzzleSequence == 0, 1, BCData4$PuzzleSequence)
BCData4$LorD <- ifelse(BCData4$LorD == "N", "D", BCData4$LorD)

BC1Mod1 <- glmer(No.Gaits ~ (1|SiteID/Subject) + Urbanization, family = "poisson", data = BCData4)
summ(BC1Mod1, digits = 3)

BC1Mod2 <- glmer(No.Gaits ~ (1|SiteID/Subject) + PuzzleType, family = "poisson", data = BCData4)
summ(BC1Mod2, digits = 3)

BC1Mod3 <- glmer(No.Gaits ~ (1|SiteID/Subject) + LorD, family = "poisson", data = BCData4)
summ(BC1Mod3, digits = 3)

BC1Mod4 <- glmer(No.Gaits ~ (1|SiteID/Subject) + GroupSize, family = "poisson", data = BCData4)
summ(BC1Mod4, digits = 3)

BC1Mod5 <- glmer(No.Gaits ~ (1|SiteID/Subject) + PuzzleSequence, family = "poisson", data = BCData4)
summ(BC1Mod5, digits = 3)


#Remove unecessary dataframes
remove(BCData)
remove(BCData1)
remove(BCMod1)
remove(BCMod2)
remove(BCMod3)
remove(BCMod4)
remove(BCMod5)

remove(BCData3)
remove(BCData4)
remove(BC1Mod1)
remove(BC1Mod2)
remove(BC1Mod3)
remove(BC1Mod4)
remove(BC1Mod5)

#Next metric is Exploration. The first way we'll measure that is investigate time (omit 0s)
Event_Data_Summed$Inv <- "N"
Event_Data_Summed$Inv <- ifelse(Event_Data_Summed$Behave == "inv_cam", "Y", Event_Data_Summed$Inv)
Event_Data_Summed$Inv <- ifelse(Event_Data_Summed$Behave == "inv_puzzle", "Y", Event_Data_Summed$Inv)
Event_Data_Summed$Inv <- ifelse(Event_Data_Summed$Behave == "inv_ground", "Y", Event_Data_Summed$Inv)
Event_Data_Summed$Inv <- ifelse(Event_Data_Summed$Behave == "inv_unk", "Y", Event_Data_Summed$Inv)

IData <- Event_Data_Summed %>%
  dplyr::filter(Inv == "Y") %>%
  group_by(EventIDSubject) %>%
  summarise(Inv_Length = sum(Total_behave_duration))

IData1 <- left_join(IData, Event_Data_Red, by = "EventIDSubject")

#Create model explaining investigate time as a function of each extrinsic variable
IData1$Urbanization <- factor(IData1$Urbanization, levels = c("Moderate", "Park", "Very"))
IData1$PuzzleSequence <- ifelse(IData1$PuzzleSequence == 0, 1, IData1$PuzzleSequence)

IMod1 <- lmer(Inv_Length ~ (1|SiteID/Subject) + Urbanization, data = IData1)
summ(IMod1, digits = 3)

IMod2 <- lmer(Inv_Length ~ (1|SiteID/Subject) + PuzzleType, data = IData1)
summ(IMod2, digits = 3)

IMod3 <- lmer(Inv_Length ~ (1|SiteID/Subject) + LorD, data = IData1)
summ(IMod3, digits = 3)

IMod4 <- lmer(Inv_Length ~ (1|SiteID/Subject) + GroupSize, data = IData1)
summ(IMod4, digits = 3)

IMod5 <- lmer(Inv_Length ~ (1|SiteID/Subject) + PuzzleSequence, data = IData1)
summ(IMod5, digits = 3)


#The second way we'll measure that is investigate time (include 0s)
IData2 <- full_join(IData, Event_Data_Red, by = "EventIDSubject")
IData2$Inv_Length[is.na(IData2$Inv_Length)] <- 0


#Create model explaining investigate time as a function of each extrinsic variable
IData2$Urbanization <- factor(IData2$Urbanization, levels = c("Moderate", "Park", "Very"))
IData2$PuzzleSequence <- ifelse(IData2$PuzzleSequence == 0, 1, IData2$PuzzleSequence)

IData2$Urbanization <- factor(IData2$Urbanization, levels = c("Moderate", "Park", "Very"))
IData2$PuzzleSequence <- ifelse(IData2$PuzzleSequence == 0, 1, IData2$PuzzleSequence)
IData2$LorD <- ifelse(IData2$LorD == "N", "D", IData2$LorD)

I1Mod1 <- lmer(Inv_Length ~ (1|SiteID/Subject) + Urbanization, data = IData2)
summ(I1Mod1, digits = 3)

I1Mod2 <- lmer(Inv_Length ~ (1|SiteID/Subject) + PuzzleType, data = IData2)
summ(I1Mod2, digits = 3)

I1Mod3 <- lmer(Inv_Length ~ (1|SiteID/Subject) + LorD, data = IData2)
summ(I1Mod3, digits = 3)

I1Mod4 <- lmer(Inv_Length ~ (1|SiteID/Subject) + GroupSize, data = IData2)
summ(I1Mod4, digits = 3)

I1Mod5 <- lmer(Inv_Length ~ (1|SiteID/Subject) + PuzzleSequence, data = IData2)
summ(I1Mod5, digits = 3)



#The third way we'll look at this is whether or not investigation occurred (binary)
IData3 <- IData2
IData3$InvYV <- IData3$Inv_Length
IData3$InvYV <- ifelse(IData3$Inv_Length == 0, 0, 1)

#Model
I2Mod1 <- glmer(InvYV ~ (1|SiteID/Subject) + Urbanization, data = IData3, family = "binomial")
summ(I2Mod1, digits = 3)

I2Mod2 <- glmer(InvYV ~ (1|SiteID/Subject) + PuzzleType, data = IData3, family = "binomial")
summ(I2Mod2, digits = 3)

I2Mod3 <- glmer(InvYV ~ (1|SiteID/Subject) + LorD, data = IData3, family = "binomial")
summ(I2Mod3, digits = 3)

I2Mod4 <- glmer(InvYV ~ (1|SiteID/Subject) + GroupSize, data = IData3, family = "binomial")
summ(I2Mod4, digits = 3)

I2Mod5 <- glmer(InvYV ~ (1|SiteID/Subject) + PuzzleSequence, data = IData3, family = "binomial")
summ(I2Mod5, digits = 3)

#remove unecessary dudes
remove(I1Mod1)
remove(I1Mod2)
remove(I1Mod3)
remove(I1Mod4)
remove(I1Mod5)

remove(I2Mod1)
remove(I2Mod2)
remove(I2Mod3)
remove(I2Mod4)
remove(I2Mod5)

remove(IMod1)
remove(IMod2)
remove(IMod3)
remove(IMod4)
remove(IMod5)

remove(IData)
remove(IData1)
remove(IData2)
remove(IData3)


#Next up, we wanto to look at effects of extrinsic variables on our fear metrics
#These are (1) Time spent galloping (indication of stress), offset by time visible
# (2) binary: whether or not galloping is observed

#1 time spent galloping
#Start with Events_Summed
EventTime <- Events %>%
  dplyr::filter(Behave == "close" | Behave == "far") %>%
  dplyr::mutate(inview = Behave_Duration) %>%
  group_by(EventIDSubject) %>%
  summarise(EventTime = sum(inview))

EventTime <- as.data.frame(EventTime)

GallopTime <- Events %>%
  dplyr::filter(Behave == "lopegallop") %>%
  group_by(EventIDSubject) %>%
  summarise(GallopTime = sum(Behave_Duration))
GallopTime <- as.data.frame(GallopTime)

Gallop <- full_join(EventTime, GallopTime, by = "EventIDSubject")

Gallop1 <- full_join(Gallop, EventSubjectList, by = "EventIDSubject")
Gallop1$GallopTime[is.na(Gallop1$GallopTime)] <- 0

Gallop2 <- full_join(Gallop1, Event_Data_Red, by = "EventIDSubject")

#Model
Gallop2$Urbanization <- factor(Gallop2$Urbanization, levels = c("Moderate", "Park", "Very"))
Gallop2$PuzzleSequence <- ifelse(Gallop2$PuzzleSequence == 0, 1, Gallop2$PuzzleSequence)

GMod1 <- lmer(GallopTime ~ (1|SiteID/Subject) + Urbanization, data = Gallop2, offset = EventTime)
summ(GMod1)

GMod2 <- lmer(GallopTime ~ (1|SiteID/Subject) + PuzzleType, data = Gallop2, offset = EventTime)
summ(GMod2)

GMod3 <- lmer(GallopTime ~ (1|SiteID/Subject) + LorD, data = Gallop2, offset = EventTime)
summ(GMod3)

GMod4 <- lmer(GallopTime ~ (1|SiteID/Subject) + GroupSize, data = Gallop2, offset = EventTime)
summ(GMod4)

GMod5 <- lmer(GallopTime ~ (1|SiteID/Subject) + PuzzleSequence, data = Gallop2, offset = EventTime)
summ(GMod5)


#Above is probably very Zi. Now try as a binomial
#Model
Gallop2$GallopYN <- ifelse(Gallop2$GallopTime == 0, 0, 1)

G1Mod1 <- glmer(GallopYN ~ (1|SiteID/Subject) + Urbanization, data = Gallop2, family = "binomial")
summ(G1Mod1)

G1Mod2 <- glmer(GallopYN ~ (1|SiteID/Subject) + PuzzleType, data = Gallop2, family = "binomial")
summ(G1Mod2)

G1Mod3 <- glmer(GallopYN ~ (1|SiteID/Subject) + LorD, data = Gallop2, family = "binomial")
summ(G1Mod3)

G1Mod4 <- glmer(GallopYN ~ (1|SiteID/Subject) + GroupSize, data = Gallop2, family = "binomial")
summ(G1Mod4)

G1Mod5 <- glmer(GallopYN ~ (1|SiteID/Subject) + PuzzleSequence, data = Gallop2, family = "binomial")
summ(G1Mod5)

#cleanup
remove(G1Mod1)
remove(G1Mod2)
remove(G1Mod3)
remove(G1Mod4)
remove(G1Mod5)

remove(GMod1)
remove(GMod2)
remove(GMod3)
remove(GMod4)
remove(GMod5)

#Last one is problem solving
Solves <- Events %>%
  dplyr::filter(Behave == "solve") %>%
  dplyr::select(EventIDSubject, Behave) %>%
  rename("Solves" = Behave)

Solves <- unique(Solves)

Solves1 <- full_join(Solves, Event_Data_Red, by = "EventIDSubject")
Solves1$Solves <- ifelse(Solves1$Solves == "solve", 1, 0)
Solves1$Solves[is.na(Solves1$Solves)] <- 0


#MODEL
Solves1$Urbanization <- factor(Solves1$Urbanization, levels = c("Moderate", "Park", "Very"))
Solves1$PuzzleSequence <- ifelse(Solves1$PuzzleSequence == 0, 1, Solves1$PuzzleSequence)
Solves1$LorD <- ifelse(Solves1$LorD == "N", "D", Solves1$LorD)

SolveMod1 <- glmer(Solves ~ (1|SiteID/Subject) + Urbanization, data = Solves1, family = "binomial")
summ(SolveMod1, digits = 3)

SolveMod2 <- glmer(Solves ~ (1|SiteID/Subject) + PuzzleType, data = Solves1, family = "binomial")
summ(SolveMod2, digits = 3)

SolveMod3 <- glmer(Solves ~ (1|SiteID/Subject) + LorD, data = Solves1, family = "binomial")
summ(SolveMod3, digits = 3)

SolveMod4 <- glmer(Solves ~ (1|SiteID/Subject) + GroupSize, data = Solves1, family = "binomial")
summ(SolveMod4, digits = 3)

SolveMod5 <- glmer(Solves ~ (1|SiteID/Subject) + PuzzleSequence, data = Solves1, family = "binomial")
summ(SolveMod5, digits = 3)

#cleanup
remove(SolveMod1)
remove(SolveMod2)
remove(SolveMod3)
remove(SolveMod4)
remove(SolveMod5)

#OK. Now do the whole thing over but with INTRINSIC variables
#These are sex, disease status, and no. previous encounters

#First, create a df that indicates the no. times an individual has appeared
EvSeq <- Event_Data_Red %>%
  dplyr::select(EventIDSubject, Subject)

EvSeq$Seq <- 1




