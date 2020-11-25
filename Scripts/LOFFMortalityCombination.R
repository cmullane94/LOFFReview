library(plyr)
library(tidyverse)

countries <- c('Canada','Chile','Ecuador','Iceland','Norway','Vietnam','Fiji','Honduras','Thailand')

#Read in GearType files for all countries, add a column specifying the country, 
#and arrange the gear type frequency in descending order
for (i in countries) {
  file_name <- paste(i, 'Mort', sep = "")
  assign(file_name, read.csv(paste('./Data/OutputData/', i, '/', i, 'MortalityNotZero.csv', sep =''), 
                             na.strings=c('','NA')))
  assign(file_name, mutate(get(file_name), "Country" = i))
  assign(file_name, arrange(get(file_name), desc(MarineMammalMortality)))
}

#Combine all dataframes (easier way to do this?)
MortAll <- rbind(CanadaMort, ChileMort, EcuadorMort, 
                     FijiMort, HondurasMort, IcelandMort, 
                     NorwayMort, ThailandMort, VietnamMort)

#Remove unnessary variables from environment
rm(CanadaMort, ChileMort, EcuadorMort, 
   FijiMort, HondurasMort, IcelandMort, 
   NorwayMort, ThailandMort, VietnamMort)

#Find the mortality per country
MortPerCountry <- MortAll %>%
  group_by(Country) %>%
  summarize(MarineMammalMortality = sum(MarineMammalMortality, na.rm = T)) %>%
  arrange(desc(MarineMammalMortality))

#Find the number of different geartypes in fisheries across countries
MortAcrossCountry <- MortAll %>%
  group_by(GearType) %>%
  summarize(MarineMammalMortality = sum(MarineMammalMortality, na.rm = T)) %>%
  arrange(desc(MarineMammalMortality))

#######################################

#Read in GearType files for all countries, add a column specifying the country, 
#and arrange the gear type frequency in descending order
for (i in countries) {
  file_name <- paste(i, 'GenMort', sep = "")
  assign(file_name, read.csv(paste('./Data/OutputData/', i, '/Gen', i, 'Mortality.csv', sep =''), 
                             na.strings=c('','NA')))
  assign(file_name, mutate(get(file_name), "Country" = i))
  assign(file_name, arrange(get(file_name), desc(MarineMammalMortality)))
}

#Joining all dataframes
GenMortAll <- rbind(CanadaGenMort, ChileGenMort, EcuadorGenMort,
                    FijiGenMort, HondurasGenMort, IcelandGenMort,
                    NorwayGenMort, ThailandGenMort, VietnamGenMort)

#Remove null values
GenMortAll <- filter(GenMortAll , MarineMammalMortality > 0)

#Remove unnessary variables from environment
rm(CanadaGenMort, ChileGenMort, EcuadorGenMort,
  FijiGenMort, HondurasGenMort, IcelandGenMort,
  NorwayGenMort, ThailandGenMort, VietnamGenMort)

#No grouping by country here; gives the same data frame as above

#Find the number of different geartypes in fisheries across countries
GenMortAcrossCountry <- GenMortAll %>%
  group_by(GearType) %>%
  summarize(MarineMammalMortality = sum(MarineMammalMortality, na.rm = T)) %>%
  arrange(desc(MarineMammalMortality))

#Saving all aggregate gear files to a CSV
files <- c("MortAll", "MortPerCountry", "MortAcrossCountry", "GenMortAll", "GenMortAcrossCountry")

for (i in files) {
  #gear_file_name <- paste("FE_2017_", geartype[i], sep = "")
  gear_file_path <- paste("./Data/OutputData/AggregatedData/Mortality/", 
                          i, ".csv", sep = "")
  write.csv(get(i), row.names = FALSE,
            file = gear_file_path)
}
