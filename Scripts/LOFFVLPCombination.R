library(plyr)
library(tidyverse)

countries <- c('Canada','Chile','Ecuador','Iceland','Norway','Vietnam','Fiji','Honduras','Thailand')

#Read in GearType files for all countries, add a column specifying the country, 
#and arrange the gear type frequency in descending order
for (i in countries) {
  file_name <- paste(i, 'VLP', sep = "")
  assign(file_name, read.csv(paste('./Data/OutputData/', i, '/', i, 'VLP.csv', sep =''), 
                             na.strings=c('','NA')))
  assign(file_name, mutate(get(file_name), "Country" = i))
  assign(file_name, arrange(get(file_name), desc(VLPTotal)))
}

#Combine all dataframes (easier way to do this?)
VLPAll <- rbind(CanadaVLP, ChileVLP, EcuadorVLP, 
                 FijiVLP, HondurasVLP, IcelandVLP, 
                 NorwayVLP, ThailandVLP, VietnamVLP)

#Remove unnessary variables from environment
rm(CanadaVLP, ChileVLP, EcuadorVLP, 
   FijiVLP, HondurasVLP, IcelandVLP, 
   NorwayVLP, ThailandVLP, VietnamVLP)

#Find the VLP per country
VLPPerCountry <- VLPAll %>%
  group_by(Country) %>%
  summarize(Vessels = sum(Vessels, na.rm = T),
            Licenses = sum(Licenses, na.rm = T),
            Participants = sum(Participants, na.rm = T),
            VLPTotal = sum(VLPTotal, na.rm = T)) %>%
  arrange(desc(VLPTotal))

#Find the number of different geartypes in fisheries across countries
VLPAcrossCountry <- VLPAll %>%
  group_by(GearType) %>%
  summarize(Vessels = sum(Vessels, na.rm = T),
            Licenses = sum(Licenses, na.rm = T),
            Participants = sum(Participants, na.rm = T),
            VLPTotal = sum(VLPTotal, na.rm = T)) %>%
  arrange(desc(VLPTotal))

#######################################

#Read in GearType files for all countries, add a column specifying the country, 
#and arrange the gear type frequency in descending order
for (i in countries) {
  file_name <- paste(i, 'GenVLP', sep = "")
  assign(file_name, read.csv(paste('./Data/OutputData/', i, '/Gen', i, 'VLP.csv', sep =''), 
                             na.strings=c('','NA')))
  assign(file_name, mutate(get(file_name), "Country" = i))
  assign(file_name, arrange(get(file_name), desc(VLPTotal)))
}

#Joining all dataframes
GenVLPAll <- rbind(CanadaGenVLP, ChileGenVLP, EcuadorGenVLP,
                    FijiGenVLP, HondurasGenVLP, IcelandGenVLP,
                    NorwayGenVLP, ThailandGenVLP, VietnamGenVLP)

#Remove unnessary variables from environment
rm(CanadaGenVLP, ChileGenVLP, EcuadorGenVLP,
   FijiGenVLP, HondurasGenVLP, IcelandGenVLP,
   NorwayGenVLP, ThailandGenVLP, VietnamGenVLP)

#No grouping by country here; gives the same data frame as above

#Find the number of different geartypes in fisheries across countries
GenVLPAcrossCountry <- GenVLPAll %>%
  group_by(GearType) %>%
  summarize(Vessels = sum(Vessels, na.rm = T),
            Licenses = sum(Licenses, na.rm = T),
            Participants = sum(Participants, na.rm = T),
            VLPTotal = sum(VLPTotal, na.rm = T)) %>%
  arrange(desc(VLPTotal))

#Saving all aggregate gear files to a CSV
files <- c("VLPAll", "VLPPerCountry", "VLPAcrossCountry", "GenVLPAll", "GenVLPAcrossCountry")

for (i in files) {
  #gear_file_name <- paste("FE_2017_", geartype[i], sep = "")
  gear_file_path <- paste("./Data/OutputData/AggregatedData/VLP/", 
                          i, ".csv", sep = "")
  write.csv(get(i), row.names = FALSE,
            file = gear_file_path)
}
