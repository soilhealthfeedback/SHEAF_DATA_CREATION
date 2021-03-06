#--SHEAF_eda_model_data_creation_rev2.R
#--loads some data, subsets and merges,and outputs a file to Model_data folder.
#--Then this dataset can be used in SEM model analysis
#  Initial regional analysis is for Illinois, Indiana, Iowa, Michigan, Minnesota, Missouri, Ohio, and Wisconsin
#--author: Erich Seamon, University of Idaho
#--date: October 2018
#
#--USAGE
#
#--SHEAF_model_data_creaton <- (crop, losstype)
#
#--crop = WHEAT, CORN, BARLEY, etc
#--losstype = acres, loss, lossperacre, lossperclaim
#
#--example
#
#--SHEAF_model_data_creation <- ("WHEAT", "acres")
#we are awesome descriptors  

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
library(plyr)



SHEAF_model_data_creation <- function(losstype) {
  
  croplist <- list("CORN", "SOYBEANS")
  
  options(scipen=999)
  
  #CAPITIALIZATION FUNCTION
  
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  
  #ADD NRI
  
  
  
  nri_tfact <- read.csv("https://nextcloud.sesync.org/index.php/s/ESranGDWaMcyDNj/download", strip.white=TRUE)
  nri_tfact$Year <- c("2015")
  
  nri_prime <- read.csv("https://nextcloud.sesync.org/index.php/s/YQCjJzwztpSfwpe/download", strip.white=TRUE)
  nri_lcc <- read.csv("https://nextcloud.sesync.org/index.php/s/RGb2eKkZtLpQ7X9/download", strip.white=TRUE)
  nri_irr <- read.csv("https://nextcloud.sesync.org/index.php/s/8EwQkxxsXa6XaRb/download", strip.white=TRUE)
  nri_eros <- read.csv("https://nextcloud.sesync.org/index.php/s/R8aASsxtMbiebYr/download", strip.white=TRUE)
  nri_dbl <- read.csv("https://nextcloud.sesync.org/index.php/s/tnge8GngoS2ozKg/download", strip.white=TRUE)
  nri_crpcov <- read.csv("https://nextcloud.sesync.org/index.php/s/GKroT2c8kRmHBPX/download", strip.white=TRUE)
  nri_brd <- read.csv("https://nextcloud.sesync.org/index.php/s/CedCm5X2PR6T37x/download", strip.white=TRUE)
  
  nri_combined <-  Reduce(function(x,y) merge(x = x, y = y, by = c("State", "County", "Year", "Fips"), all = TRUE), 
                          list(nri_tfact, nri_prime, nri_lcc, nri_irr, nri_eros, nri_dbl, nri_crpcov, nri_brd))
  
  nri <- subset(nri_combined, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin" | State == "Nebraska"| State == "Kansas" 
                     | State == "North Dakota" | State == "South Dakota")
  
  #---
  
  #AGCENSUS
  
  #agcensus load via url - best if you are NOT on SESYNC rstudio server
  agcensus <- read.csv("https://nextcloud.sesync.org/index.php/s/SFiSow3f4aSTdCK/download")
  
  #agcensus load using csv - use if you ARE on SESYNC Rstudio server
  #setwd("/nfs/soilsesfeedback-data/data/agcensus")
  #agcensus <- read.csv("Agcensus2012.csv")
  #agcensus_metadata <- read.csv("Agcensus2012_metadata.csv")
  
  
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
  
  agcensus <- subset(agcensus, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin" | State == "Nebraska"| State == "Kansas" 
                     | State == "North Dakota" | State == "South Dakota")
  
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
  eqip <- read.csv("https://nextcloud.sesync.org/index.php/s/os5ZxFXAAEgc2y4/download")
  
  #eqip load using csv - use if you ARE on SESYNC Rstudio server
  #setwd("/nfs/soilsesfeedback-data/data/eqip")
  #eqip <- read.csv("eqip.csv")
  
  
  eqip$County <- tolower(eqip$County)
  eqip$County <- sapply(eqip$County, simpleCap)
  
  #EQIP SUBSET TO ONLY STUDY AREA
  eqip <- subset(eqip, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin" | State == "Nebraska"| State == "Kansas" 
                 | State == "North Dakota" | State == "South Dakota")
  
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
  census <- read.csv("https://nextcloud.sesync.org/index.php/s/tnEESyD34JFoNLf/download")
  
  #census load using csv - use if you ARE on SESYNC Rstudio server
  #setwd("/nfs/soilsesfeedback-data/data/census")
  #census <- read.csv("Census_States_CountyDem.csv")
  
  #CENSUS FIX CAPITIALIZATION AND THEN SUBSET TO ONLY STUDY AREA
  census$State <- tolower(census$State)
  census$State <- sapply(census$State, simpleCap)
  
  census$County <- tolower(census$County)
  census$County <- sapply(census$County, simpleCap)
  
  census <- subset(census, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin" | State == "Nebraska"| State == "Kansas" 
                   | State == "North Dakota" | State == "South Dakota")
  
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
  commodity <- read.csv("https://nextcloud.sesync.org/index.php/s/zFwyd64NZJEyrgr/download")
  
  #commodity load using csv - use if you ARE on SESYNC Rstudio server
  #setwd("/nfs/soilsesfeedback-data/data/RMA/RMA_csvfiles")
  #commodity <- read.csv("RMA_combined.csv")
  
  colnames(commodity) <- c("ID", "Year", "State", "County", "Commodity", "Loss", "Count", "Acres", "Lossperacre", "Lossperclaim", "Acresperclaim")
  commodity$State <- state.name[match(commodity$State,state.abb)]
  
  
  
  #RMA COMMODITY FIX CAPITIALIZATION AND THEN SUBSET TO ONLY STUDY AREA
  commodity <- subset(commodity, State == "Illinois" | State == "Indiana" | State == "Iowa" | 
                        State == "Michigan" | State == "Minnesota" | State == "Missouri" | 
                        State == "Ohio" | State == "Wisconsin" | State =="Nebraska" | State == "Kansas" 
                      | State == "North Dakota" | State == "South Dakota")
  #colnames(commodity) <- c("ID", "Year", "State", "County", "Commodity", "Loss_commodity", "Count_commodity")
  
  
  
  commodity_loss_total <- aggregate(list(commodity$Acres, commodity$Loss, commodity$Lossperacre, commodity$Lossperclaim, commodity$Acresperclaim, commodity$Count), by = list(commodity$State, commodity$County, commodity$Year), FUN ="sum")
  colnames(commodity_loss_total) <- c("State", "County", "Year", "RMA_Acres", "RMA_Loss", "RMA_Lossperacre", "RMA_Lossperclaim", "RMA_Acresperclaim", "RMA_Count")
  
  
  
  #RMA DAMAGE
  
  #RMA by damage cause, includes claim counts
  damage <- read.csv("https://nextcloud.sesync.org/index.php/s/W5kztdb5ZkxRptT/download")
  
  #damage load using csv - use if you ARE on SESYNC Rstudio server
  #setwd("/nfs/soilsesfeedback-data/data/RMA/")
  #damage <- read.csv("RMA_damage_combined.csv", strip.white=TRUE)
  
  
  colnames(damage) <- c("ID", "Year", "State", "County", "Commodity", "Damagecause", "Loss", "Count", "Acres", "Lossperacre", "Lossperclaim", "Acresperclaim")
  damage$State <- state.name[match(damage$State,state.abb)]
  
  #RMA DAMAGE FIX CAPITIALIZATION AND THEN SUBSET TO ONLY STUDY AREA
  
  damage <- subset(damage, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin" | State == "Nebraska"| State == "Kansas" 
                   | State == "North Dakota" | State == "South Dakota")
  #colnames(damage) <- c("ID", "Year", "State", "County", "Commodity", "Damagecause", "Loss_damagecause", "Count_damagecause")
  #damage <- aggregate(damage$Loss, by=list(damage$Commodity, damage$Damagecause, damage$County, damage$State, damage$Year), FUN = "sum")
  #colnames(damage) <- c("Commodity", "Damagecause", "County", "State", "Year", "Loss")
  
  
  
  
  library(reshape2) ; damage_loss <- dcast(damage, State + County + Year + Commodity ~ Damagecause, value.var = c("Loss"), sum)
  library(reshape2) ; damage_lossperacre <- dcast(damage, State + County + Year + Commodity ~ Damagecause, value.var = c("Lossperacre"), sum)
  library(reshape2) ; damage_lossperclaim <- dcast(damage, State + County + Year + Commodity ~ Damagecause, value.var = c("Lossperclaim"), sum)
  library(reshape2) ; damage_acres <- dcast(damage, State + County + Year + Commodity ~ Damagecause, value.var = c("Acres"), sum)
 
  #damage <- spread(damage, Damagecause, Loss_damagecause)
  
  crop_damage_acres <- subset(damage_acres, Commodity == croplist)
  crop_damage_loss <- subset(damage_loss, Commodity == croplist)
  crop_damage_lossperacre <- subset(damage_lossperacre, Commodity == croplist)
  crop_damage_lossperclaim <- subset(damage_lossperclaim, Commodity == croplist)
  
  crop_damage_acres <- aggregate(crop_damage_acres[,5:38], by = list(crop_damage_acres$State, crop_damage_acres$County, crop_damage_acres$Year), FUN = 'sum' )
  colnames(crop_damage_acres)[1:3] <- c("State", "County", "Year")
  crop_damage_loss <- aggregate(crop_damage_loss[,5:38], by = list(crop_damage_loss$State, crop_damage_loss$County, crop_damage_loss$Year), FUN = 'sum' )
  colnames(crop_damage_loss)[1:3] <- c("State", "County", "Year")
  crop_damage_lossperacre <- aggregate(crop_damage_lossperacre[,5:38], by = list(crop_damage_lossperacre$State, crop_damage_lossperacre$County, crop_damage_lossperacre$Year), FUN = 'sum' )
  colnames(crop_damage_lossperacre)[1:3] <- c("State", "County", "Year")
   crop_damage_lossperclaim <- aggregate(crop_damage_lossperclaim[,5:38], by = list(crop_damage_lossperclaim$State, crop_damage_lossperclaim$County, crop_damage_lossperclaim$Year), FUN = 'sum' )
   colnames(crop_damage_lossperclaim)[1:3] <- c("State", "County", "Year")
   
  
  #climate - pdsi
  
      pdsi_moderate_drought_2007_2012 <- read.csv("https://nextcloud.sesync.org/index.php/s/TYa9pBNQHBc4efj/download")
   countyFIPS <- read.csv("https://nextcloud.sesync.org/index.php/s/wcFmKrSZW6Pr6D2/download")
   
   pdsi <- merge(pdsi_moderate_drought_2007_2012, countyFIPS, by  = "FIPS")
   
   pdsi$PDSI_TOTALS <- rowSums(pdsi[,c("X2007","X2008","X2009","X2010","X2011", "X2012" )])
   pdsi <- pdsi[,10:12]
   colnames(pdsi) <- c("State", "County", "PDSI_TOTALS")

   
   #cdl
   
   cdl_diversity <- read.csv("https://nextcloud.sesync.org/index.php/s/RWyocfcwpAobCrq/download")
   cdl_div <- merge(cdl_diversity, countyFIPS, by = "FIPS")
   
   library(reshape2) ; cdl_div2 <- dcast(cdl_div, STATE_NAME + NAME ~ Year, value.var = c("CDI"))
   colnames(cdl_div2) <- c("State", "County", "CDI_2011", "CDI_2012")
   

   #cash rent
   
   cash_rent <- read.csv("https://nextcloud.sesync.org/index.php/s/rbGosZCQoqT5S8T/download")
   cash_rent <- cash_rent[,c(2,5,6,7,8,9,10,11,12)]
   colnames(cash_rent) <- c("Year", "RENT_Irrigated_Rent_Cropland", "RENT_NonIrrigated_Rent_Cropland", "RENT_Pastureland", "RENT_average", "State", "County", "RENT_Total_Cropland_Acres", "RENT_Total")
  
   #--MERGE!
  
  library(tidyverse)
  merge1 <- merge(eval(parse(text=paste("crop_damage_", losstype, sep=""))), agcensus, by = c("State", "County", "Year"))
  merge2 <- merge(eqip, merge1, by = c("State", "County", "Year") )
  merge3 <- merge(merge2, alc2, by = c("State", "County"))
  merge4 <- merge(merge3, census, by = c("State", "County", "Year"))
  merge5 <- merge(merge4, nri, by = c("State", "County", "Year"))
  merge6 <- merge(merge5, commodity_loss_total, by = c("State", "County", "Year"))
  merge6a <- merge(merge6, pdsi, by = c("State", "County"))
  merge6b <- merge(merge6a, cash_rent, by = c("State", "County", "Year"))
  merge7 <- merge(merge6b, cdl_div2, by = c("State", "County"))
  
  
  
  merge7[is.na(merge7)] <- 0 
  
  colnames(merge7)[4:33] <- paste("EQIP_", colnames(merge7)[4:33], sep = "")
  colnames(merge7)[34:67] <- paste("RMA_", colnames(merge7)[34:67], sep = "")
  colnames(merge7)[68:89] <- paste("AGCENSUS_", colnames(merge7)[68:89], sep = "")
  colnames(merge7)[91:101] <- paste("CENSUS_", colnames(merge7)[91:101], sep = "")
  colnames(merge7)[101:195] <- paste("NRI_", colnames(merge7)[101:195], sep = "")
  
  #--transformed variables
  
  #dependent variable 1
  #cover crops / total cropland
  
  merge7$AGCENSUS_CC_Cropland_Acres_Ratio <- merge7$AGCENSUS_cc_acres / merge7$AGCENSUS_Cropland_Acres
  
  #WRITE FILE TO MODEL_DATA FOLDER FOR SEM ANALYSIS
  
  #create name of crops used for RMA - example - for Midwest, we are using corn and soybeans
  croplist_name <- paste(croplist[[1]], "_", croplist[[2]], sep="")
  
  library( taRifx )
  #make sure all exogenous variables are numeric and state and county are factors
  merge8 <- japply( merge7[,4:211], which(sapply(merge7[,4:211], class)=="factor"), as.numeric )
  merge9 <- japply( merge8, which(sapply(merge8, class)=="integer"), as.numeric )
  
  merge9 <- cbind(merge7$State, merge7$County, merge7$Year, merge9)
  colnames(merge9)[1:3] <- c("State", "County", "Year")
  merge9$State <- factor(merge9$State)
  merge9$County <- factor(merge9$County)
  
  #replace Inf with zero for RMA_lossperacre
  merge9$RMA_Lossperacre[which(!is.finite(merge9$RMA_Lossperacre))] <- 0

  
  remove_zero_cols <- function(df) {
    rem_vec <- NULL
    for(i in 1:ncol(df)){
      this_sum <- summary(df[,i])
      zero_test <- length(which(this_sum == 0))
      if(zero_test == 6) {
        rem_vec[i] <- names(df)[i]
      }
    }
    features_to_remove <- rem_vec[!is.na(rem_vec)]
    rem_ind <- which(names(df) %in% features_to_remove)
    df <- df[,-rem_ind]
    return(df)
  }
  
  
  merge9 <- remove_zero_cols(merge9)
  
  merge9 <- merge9[,-86] #remove FIPS
  
  scaled_merge10 <- merge9[, -c(1:3)] <- scale(merge9[, -c(1:3)], center = TRUE, scale = TRUE)
  
  
  #write the combined file to the model_data location for SEM
  write.csv(merge9, file = paste("/nfs/soilsesfeedback-data/model_data/MIDWEST_", croplist_name, "_Model_", losstype, "_nonscaled.csv", sep=""))
  write.csv(scaled_merge10, file = paste("/nfs/soilsesfeedback-data/model_data/MIDWEST_", croplist_name, "_Model_", losstype, "_scaled.csv", sep=""))
  
}


