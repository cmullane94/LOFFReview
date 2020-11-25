library(plyr)
library(tidyverse)

#Countries: Canada, Chile, Ecuador, Iceland, Norway, Vietnam, Fiji, Honduras, Thailand
###Set 'country' equal to one of these values and run the rest of the script
country <- 'Canada'
#country <- 'Chile'
#country <- 'Ecuador'
#country <- 'Iceland'
#country <- 'Norway'
#country <- 'Vietnam'
#country <- 'Fiji'
#country <- 'Honduras'
#country <- 'Thailand'
##########################Specific Gear Type Analysis##########################
#Read in data and setting empty cells as NA
loff <- read.csv(paste('./Data/InputData/LOFF', country, '.csv', sep =''), 
                 na.strings=c('','NA'))

#Specifying classes for later operations
#loff$ExportFishery <- as.character(loff$ExportFishery)
#loff$GearType <- as.character(loff$GearType)
loff$Vessels <- as.numeric(loff$Vessels)
loff$Licenses <- as.numeric(loff$Licenses)
loff$Participants <- as.numeric(loff$Participants)
loff$MarineMammalMortality <- as.numeric(loff$MarineMammalMortality)
#loff$RFMO <- as.character(loff$RFMO)
#loff$AdditionalInfo <- as.character(loff$AdditionalInfo)

#Checking out the number of unique values
unique(loff$GearType)

#Finding the count of each unique gear type 
GearType <- as.data.frame(table(loff$GearType, dnn = list('GearType')), 
                          responseName = "Frequency")

#Changing the class for later
GearType$GearType <- as.character(GearType$GearType)

#Arranging by frequency
GearType <- arrange(GearType, desc(Frequency))

#Finding the total number of vessels, licenses, and
#participants for each unique gear type
VLP <- loff %>% 
  group_by(GearType) %>% 
  summarize(Vessels = sum(Vessels, na.rm = T),
            Licenses = sum(Licenses, na.rm = T),
            Participants = sum(Participants, na.rm = T)) %>%
  mutate(VLPTotal = Vessels + Licenses + Participants) %>%
  arrange(desc(VLPTotal))

#Finding the total average annual mortality for each unique gear type
Mortality <- loff %>% 
  group_by(GearType) %>% 
  summarize(MarineMammalMortality = 
              sum(MarineMammalMortality, na.rm = T)) %>%
  arrange(desc(MarineMammalMortality))

#Finding only the fisheries that do not have a mortality of zero
MortalityNotZero = filter(Mortality, MarineMammalMortality > 0)

#Finding the fisheries that are part of RFMOs
RFMO = filter(loff, !is.na(RFMO))

write.csv(GearType, 
          paste('./Data/OutputData/', country, '/', country, 'GearType.csv', 
                sep = ''), row.names = FALSE)
write.csv(VLP, 
          paste('./Data/OutputData/', country, '/', country, 'VLP.csv', 
                sep = ''), row.names = FALSE)
write.csv(Mortality, 
          paste('./Data/OutputData/', country, '/', country, 'Mortality.csv',
                sep = ''), row.names = FALSE)
write.csv(MortalityNotZero, 
          paste('./Data/OutputData/', country, '/', country, 'MortalityNotZero.csv',
                sep = ''), row.names = FALSE)
write.csv(RFMO, 
          paste('./Data/OutputData/', country, '/', country, 'RFMO.csv', 
                sep = ''), row.names = FALSE)

##########################General Gear Type Analysis##########################

#Copying original dataframes
GearTypeGen <- data.frame(GearType)
VLPGen <- data.frame(VLP)
MortalityGen <- data.frame(Mortality)

#Replacing specific gear types with more general ones 
##########################Create a for loop or function for this##########################
GearTypeGen$GearType[grepl("Seine", GearTypeGen$GearType, ignore.case=TRUE)] <- "Seine"
GearTypeGen$GearType[grepl("Longline", GearTypeGen$GearType, ignore.case=TRUE)] <- "Longline"
GearTypeGen$GearType[grepl("Gillnet", GearTypeGen$GearType, ignore.case=TRUE)] <- "Gillnet"
GearTypeGen$GearType[grepl("Trawl", GearTypeGen$GearType, ignore.case=TRUE)] <- "Trawl"

VLPGen$GearType[grepl("Seine", VLPGen$GearType, ignore.case=TRUE)] <- "Seine"
VLPGen$GearType[grepl("Longline", VLPGen$GearType, ignore.case=TRUE)] <- "Longline"
VLPGen$GearType[grepl("Gillnet", VLPGen$GearType, ignore.case=TRUE)] <- "Gillnet"
VLPGen$GearType[grepl("Trawl", VLPGen$GearType, ignore.case=TRUE)] <- "Trawl"

MortalityGen$GearType[grepl("Seine", MortalityGen$GearType, ignore.case=TRUE)] <- "Seine"
MortalityGen$GearType[grepl("Longline", MortalityGen$GearType, ignore.case=TRUE)] <- "Longline"
MortalityGen$GearType[grepl("Gillnet", MortalityGen$GearType, ignore.case=TRUE)] <- "Gillnet"
MortalityGen$GearType[grepl("Trawl", MortalityGen$GearType, ignore.case=TRUE)] <- "Trawl"

#Grouping the general geartypes and funding sums of other data
GearTypeGen <- GearTypeGen %>% 
  group_by(GearType) %>% 
  summarize(Frequency = sum(Frequency, na.rm = T)) %>%
  arrange(desc(Frequency))

VLPGen <- VLPGen %>% 
  group_by(GearType) %>% 
  summarize(Vessels = sum(Vessels, na.rm = T),
            Licenses = sum(Licenses, na.rm = T),
            Participants = sum(Participants, na.rm = T)) %>%
  mutate(VLPTotal = Vessels + Licenses + Participants) %>%
  arrange(desc(VLPTotal))

MortalityGen <- MortalityGen %>% 
  group_by(GearType) %>% 
  summarize(MarineMammalMortality = sum(MarineMammalMortality, na.rm = T)) %>%
  arrange(desc(MarineMammalMortality))

#Saving files
write.csv(GearTypeGen, 
          paste('./Data/OutputData/', country, '/Gen', country, 'GearType.csv', 
                sep = ''), row.names = FALSE)
write.csv(VLPGen, 
          paste('./Data/OutputData/', country, '/Gen', country, 'VLP.csv', 
                sep = ''), row.names = FALSE)
write.csv(MortalityGen, 
          paste('./Data/OutputData/', country, '/Gen', country, 'Mortality.csv',
                sep = ''), row.names = FALSE)

