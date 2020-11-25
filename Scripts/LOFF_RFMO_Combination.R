library(plyr)
library(tidyverse)

countries <- c('Canada','Chile','Ecuador','Iceland','Norway','Vietnam','Fiji','Honduras','Thailand')

#Read in GearType files for all countries, add a column specifying the country, 
#and arrange the gear type frequency in descending order
for (i in countries) {
  file_name <- paste(i, 'RFMO', sep = "")
  assign(file_name, read.csv(paste('./Data/OutputData/', i, '/', i, 'RFMO.csv', sep =''), 
                             na.strings=c('','NA')))
  assign(file_name, mutate(get(file_name), "Country" = i))
  assign(file_name, arrange(get(file_name), RFMO))
}

#Combine all dataframes (easier way to do this?)
RFMOAll <- rbind(CanadaRFMO, ChileRFMO, EcuadorRFMO, 
                FijiRFMO, HondurasRFMO, IcelandRFMO, 
                NorwayRFMO, ThailandRFMO, VietnamRFMO)

#Creating a table with RFMO frequencies
RFMOFreq <- as.data.frame(table(RFMOAll$RFMO, dnn = list('RFMO')), 
                          responseName = "Frequency")
RFMOFreq <- arrange(RFMOFreq, desc(Frequency))

#Checking out the total amount of RFMOs
sum(RFMOFreq$Frequency)

#Saving all aggregate gear files to a CSV
files <- c("RFMOAll", "RFMOFreq")

for (i in files) {
  #gear_file_name <- paste("FE_2017_", geartype[i], sep = "")
  gear_file_path <- paste("./Data/OutputData/AggregatedData/RFMO/", 
                          i, ".csv", sep = "")
  write.csv(get(i), row.names = FALSE,
            file = gear_file_path)
}
