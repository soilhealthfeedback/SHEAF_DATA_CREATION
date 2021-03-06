#--SHEAF_eda_model_data_creation.R
#--loads some data, subsets and merges,and outputs a file to Model_data folder.
#--Then this dataset can be used in SEM model analysis
#  Initial regional analysis is for Illinois, Indiana, Iowa, Michigan, Minnesota, Missouri, Ohio, and Wisconsin
#--author: Erich Seamon, University of Idaho
#--date: October 2018

library(rgdal)
library(leaflet)
library(maptools)
library(classInt)
library(leaflet)
library(dplyr)
library(Hmisc)
library(RColorBrewer)
library(raster)
library (RCurl)
library(maptools)
library(tidyr)

SHEAF_model_data_creation_local <- function(crop) {
  
  options(scipen=999)
  
  #CAPITIALIZATION FUNCTION
  
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  
  #AGCENSUS
  
  #agcensus load via url - best if you are NOT on SESYNC rstudio server
  #agcensus <- read.csv("https://nextcloud.sesync.org/index.php/s/THpGDspGXFtLSGF/download")
  
  #agcensus load using csv - use if you ARE on SESYNC Rstudio server
  setwd("/nfs/soilsesfeedback-data/data/agcensus")
  agcensus <- read.csv("Agcensus2012.csv")
  agcensus_metadata <- read.csv("Agcensus2012_metadata.csv")
  
  
  #removes ancillary columns at the end of the agcensus
  agcensus <- agcensus[,1:25]
  
  #AGCENSUS CHANGE COLUMN NAMES, FIX CAPITIALIZATION AND THEN SUBSET TO ONLY STUDY AREA
  colnames(agcensus)[3] <- "State"
  colnames(agcensus)[2] <- "Year"
  colnames(agcensus)[4] <- "County"
  agcensus$State <- tolower(agcensus$State)
  agcensus$State <- sapply(agcensus$State, simpleCap)
  
  agcensus$County <- tolower(agcensus$County)
  agcensus$County <- sapply(agcensus$County, simpleCap)
  
  agcensus <- subset(agcensus, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin")
  
  #AGCENSUS AGGREGATE BASED ON STATE AND COUNTY USING DOLLARS PAID AS THE SUMMARIZED OUTPUT
  agcensus <- aggregate(cbind(agcensus$tile_farms,  agcensus$tile_acres, agcensus$tile_acres_avgfarm, agcensus$ditches_farms,        
                              agcensus$ditches_acres, agcensus$ditches_acres_avgfarm, agcensus$consease_farms, agcensus$consease_acres,       
                              agcensus$consease_avgfarm, agcensus$notill_farms, agcensus$notill_acres, agcensus$notill_avgfarm,       
                              agcensus$constill_farms, agcensus$constill_acres, agcensus$constill_avgfarm, agcensus$convtill_farms,       
                              agcensus$convtill_acres, agcensus$convtill_acres.1, agcensus$cc_farms, agcensus$cc_acres,             
                              agcensus$cc_avgfarm), by=list(agcensus$State, agcensus$Year, agcensus$County), FUN = "sum")
  colnames(agcensus) <- c("State", "Year", "County", "tile_farms",            "tile_acres",           
                          "tile_acres_avgfarm",    "ditches_farms",         "ditches_acres",         "ditches_acres_avgfarm", "consease_farms",        "consease_acres",       
                          "consease_avgfarm",      "notill_farms",          "notill_acres",          "notill_avgfarm",        "constill_farms",        "constill_acres",       
                          "constill_avgfarm",      "convtill_farms",        "convtill_acres",        "convtill_acres.1",      "cc_farms",              "cc_acres",             
                          "cc_avgfarm")
  
  #EQIP
  
  #eqip load via url - best if you are NOT on SESYNC rstudio server
  #eqip <- read.csv("https://nextcloud.sesync.org/index.php/s/bgWSzqdqYDifJwz/download")
  
  #eqip load using csv - use if you ARE on SESYNC Rstudio server
  setwd("/nfs/soilsesfeedback-data/data/eqip")
  eqip <- read.csv("eqip.csv")
  
  
  eqip$County <- tolower(eqip$County)
  eqip$County <- sapply(eqip$County, simpleCap)
  
  #EQIP SUBSET TO ONLY STUDY AREA
  eqip <- subset(eqip, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin")
  
  #EQIP AGGREGATE BASED ON STATE AND COUNTY USING DOLLARS PAID AS THE SUMMARIZED OUTPUT
  eqip <- aggregate(eqip$Dollars.Paid, by=list(eqip$State, eqip$County, eqip$Applied.Amount, eqip$Applied.Year, eqip$practice_name), FUN = "sum")
  colnames(eqip) <- c("State", "County", "Applied.Amount", "Year", "Practice_Name", "Dollars_Paid")
  
  eqip$id <- seq_len(nrow(eqip))
  library(reshape2) ; eqip <- dcast(eqip, State + County + Year ~ Practice_Name, value.var = "Dollars_Paid", sum)
  
  
  #eqip <- spread(eqip, Practice_Name, Dollars_Paid)
  
  
  #eqip load using csv - use if you ARE on SESYNC Rstudio server
  #setwd("/nfs/soilsesfeedback-data/data/eqip")
  #eqip <- read.csv("eqip.csv")
  
  #CENSUS
  
  #census load - best if you are NOT on SESYNC rstudio server
  #census <- read.csv("https://nextcloud.sesync.org/index.php/s/C3jHtLfRToPkrJa/download")
  
  #census load using csv - use if you ARE on SESYNC Rstudio server
  setwd("/nfs/soilsesfeedback-data/data/census")
  census <- read.csv("Census_States_CountyDem.csv")
  
  #CENSUS FIX CAPITIALIZATION AND THEN SUBSET TO ONLY STUDY AREA
  census$State <- tolower(census$State)
  census$State <- sapply(census$State, simpleCap)
  
  census$County <- tolower(census$County)
  census$County <- sapply(census$County, simpleCap)
  
  census <- subset(census, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin")
  
  census$Value <- as.numeric(census$Value)
  #CENSUS AGGREGATE BASED ON STATE AND COUNTY USING DOLLARS PAID AS THE SUMMARIZED OUTPUT
  census <- aggregate(census$Value, by=list(census$State, census$County, census$Year, census$Data.Item), FUN = "sum")
  colnames(census) <- c("State", "County", "Year", "census_Grouping", "census_Value")
  
  library(reshape2) ; census <- dcast(census, State + County + Year ~ census_Grouping, value.var = "census_Value", sum)
  
  #census <- spread(census, census_Grouping, census_Value)
  
  #census load using csv - use if you ARE on SESYNC Rstudio server
  #setwd("/nfs/soilsesfeedback-data/data/census")
  #census <- read.csv("Census_States_CountyDem.csv")
  
  #RMA COMMODITY
  
  #RMA by commodity and damage cause, includes claim counts
  #commodity <- read.csv("https://nextcloud.sesync.org/index.php/s/niLjWBSwmCoxQyC/download")
  
  #commodity load using csv - use if you ARE on SESYNC Rstudio server
  setwd("/nfs/soilsesfeedback-data/data/RMA")
  commodity <- read.csv("commodities.csv")
  
  colnames(commodity) <- c("ID", "Year", "State", "County", "Commodity", "Loss_commodity", "Count_commodity")
  commodity$State <- state.name[match(commodity$State,state.abb)]
  
  #RMA COMMODITY FIX CAPITIALIZATION AND THEN SUBSET TO ONLY STUDY AREA
  commodity <- subset(commodity, State == "Illinois" | State == "Indiana" | State == "Iowa" | 
                        State == "Michigan" | State == "Minnesota" | State == "Missouri" | 
                        State == "Ohio" | State == "Wisconsin" | State =="Nebraska" | State == "Kansas" 
                      | State == "North Dakota" | State == "South Dakota")
  colnames(commodity) <- c("ID", "Year", "State", "County", "Commodity", "Loss_commodity", "Count_commodity")
  
  #RMA DAMAGE
  
  #RMA by damage cause, includes claim counts
  #damage <- read.csv("https://nextcloud.sesync.org/index.php/s/YErYqQYB9PAkmH9/download")
  
  #damage load using csv - use if you ARE on SESYNC Rstudio server
  setwd("/nfs/soilsesfeedback-data/data/RMA")
  damage <- read.csv("commodities_damagecause.csv", strip.white=TRUE)
  
  
  colnames(damage) <- c("ID", "Year", "State", "County", "Commodity", "Damagecause", "Loss_damagecause", "Count_damagecause")
  damage$State <- state.name[match(damage$State,state.abb)]
  
  #RMA DAMAGE FIX CAPITIALIZATION AND THEN SUBSET TO ONLY STUDY AREA
  
  damage <- subset(damage, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin")
  colnames(damage) <- c("ID", "Year", "State", "County", "Commodity", "Damagecause", "Loss_damagecause", "Count_damagecause")
  damage <- aggregate(damage$Loss_damagecause, by=list(damage$Commodity, damage$Damagecause, damage$County, damage$State, damage$Year), FUN = "sum")
  colnames(damage) <- c("Commodity", "Damagecause", "County", "State", "Year", "Loss_damagecause")
  
  
  library(reshape2) ; damage <- dcast(damage, State + County + Year ~ Damagecause, value.var = "Loss_damagecause", sum)
  
  #damage <- spread(damage, Damagecause, Loss_damagecause)
  
  crop_damage <- subset(damage, Commodity == crop)
  
  
  
  
  #--MERGE!
  
  library(tidyverse)
  merge1 <- merge(crop_damage,agcensus, by = c("State", "County", "Year"))
  merge2 <- merge(eqip, merge1, by = c("State", "County", "Year") )
  merge3 <- merge(merge2, census, by = c("State", "County", "Year"))
  
  merge3[is.na(merge3)] <- 0 
  
  #WRITE FILE TO MODEL_DATA FOLDER FOR SEM ANALYSIS
  
  write.csv(merge3, file = paste("/nfs/soilsesfeedback-data/model_data/", crop, "_Model_dataset1.csv", sep=""))
  
}





