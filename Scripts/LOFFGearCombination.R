library(plyr)
library(tidyverse)

countries <- c('Canada','Chile','Ecuador','Iceland','Norway','Vietnam','Fiji','Honduras','Thailand')

#Read in GearType files for all countries, add a column specifying the country, 
#and arrange the gear type frequency in descending order
for (i in countries) {
  file_name <- paste(i, 'GearType', sep = "")
  assign(file_name, read.csv(paste('./Data/OutputData/', i, '/', i, 'GearType.csv', sep =''), 
            na.strings=c('','NA')))
  assign(file_name, mutate(get(file_name), "Country" = i))
  assign(file_name, arrange(get(file_name), desc(Frequency)))
}

#Combine all dataframes
GearTypeAll <- rbind(CanadaGearType, ChileGearType, EcuadorGearType, 
                     FijiGearType, HondurasGearType, IcelandGearType, 
                     NorwayGearType, ThailandGearType, VietnamGearType)

#Remove unnessary variables from environment
rm(CanadaGearType, ChileGearType, EcuadorGearType, 
    FijiGearType, HondurasGearType, IcelandGearType, 
    NorwayGearType, ThailandGearType, VietnamGearType)

#Find the number of different fisheries in each country
FisheriesPerCountry <- GearTypeAll %>%
  group_by(Country) %>%
  summarize(Frequency = sum(Frequency, na.rm = T)) %>%
  arrange(desc(Frequency))

#Find the number of different geartypes in fisheries across countries
GearTypeAcrossCountry <- GearTypeAll %>%
  group_by(GearType) %>%
  summarize(Frequency = sum(Frequency, na.rm = T)) %>%
  arrange(desc(Frequency))

#######################################

#Read in GearType files for all countries, add a column specifying the country, 
#and arrange the gear type frequency in descending order
for (i in countries) {
  file_name <- paste(i, 'GearTypeGen', sep = "")
  assign(file_name, read.csv(paste('./Data/OutputData/', i, '/Gen', i, 'GearType.csv', sep =''), 
                             na.strings=c('','NA')))
  assign(file_name, mutate(get(file_name), "Country" = i))
  assign(file_name, arrange(get(file_name), desc(Frequency)))
}

GenGearTypeAll <- rbind(CanadaGearTypeGen, ChileGearTypeGen, EcuadorGearTypeGen, 
                     FijiGearTypeGen, HondurasGearTypeGen, IcelandGearTypeGen, 
                     NorwayGearTypeGen, ThailandGearTypeGen, VietnamGearTypeGen)

#Remove unnessary variables from environment
rm(CanadaGearTypeGen, ChileGearTypeGen, EcuadorGearTypeGen, 
  FijiGearTypeGen, HondurasGearTypeGen, IcelandGearTypeGen, 
  NorwayGearTypeGen, ThailandGearTypeGen, VietnamGearTypeGen)

#No grouping by country here; gives the same data frame as above

#Find the number of different general geartypes in fisheries across countries
GenGearTypeAcrossCountry <- GearTypeGenAll %>%
  group_by(GearType) %>%
  summarize(Frequency = sum(Frequency, na.rm = T)) %>%
  arrange(desc(Frequency))

#Saving all aggregate gear files to a CSV
files <- c("GearTypeAll", "FisheriesPerCountry", "GearTypeAcrossCountry", "GenGearTypeAll", "GenGearTypeAcrossCountry")

for (i in files) {
  #gear_file_name <- paste("FE_2017_", geartype[i], sep = "")
  gear_file_path <- paste("./Data/OutputData/AggregatedData/GearType/", 
                          i, ".csv", sep = "")
  write.csv(get(i), row.names = FALSE,
            file = gear_file_path)
}
