#--SHEAF_eda_model_data_creation.R
#--loads some data, subsets and merges,and outputs a file to Model_data folder.
#--Then this dataset can be used in SEM model analysis
#  Initial regional analysis is for Illinois, Indiana, Iowa, Michigan, Minnesota, Missouri, Ohio, and Wisconsin
#--author: Erich Seamon, University of Idaho
#--updated: August 2019
#
#

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


#alter this list to subset RMA data for only particular commodities.  Also changes the model file output name 
#and includes these commodities in the string
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
  
  #subset nri for only midwest states
  nri <- subset(nri_combined, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin" | State == "Nebraska"| State == "Kansas" 
                     | State == "North Dakota" | State == "South Dakota")
  
  #remove Fips column
  nri <- subset( nri, select = -Fips )
  
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
  
  #subset only to midwest states
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
  
  #converts to lower case and then caps the first letters of words
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
  
  #EQIP-CSP
  
  eqip_csp <- read.csv("https://files.sesync.org/index.php/s/5Rjpp4z5xe7KCC8/download")
  eqip_csp$County <- tolower(eqip_csp$County)
  eqip_csp$County <- sapply(eqip_csp$County, simpleCap)
  eqip_csp <- eqip_csp[,2:8]
  
  #remove extraneous columns other that state, year, county, and variables
  #eqip_csp <- cbind.data.frame(eqip_csp[,4], eqip_csp[,6], eqip_csp[,7:10])
  colnames(eqip_csp) <- c("Year", "State", "County", "EQIP_CSP_Contracts", "EQIP_CSP_CONTRACT_ACRES", "EQIP_CSP_FA_OBLIGATIONS", "EQIP_CSP_FA_PAYMENTS" )
  
  #EQIP-Cons-security
  
  eqip_cons_security <- read.csv("https://files.sesync.org/index.php/s/ZsX6SYFmc8Tsr77/download")
  eqip_cons_security$County <- tolower(eqip_cons_security$County)
  eqip_cons_security$County <- sapply(eqip_cons_security$County, simpleCap)
  eqip_cons_security <- eqip_cons_security[,2:8]
  
  #EQIP-Cons-stewardship
  
  eqip_cons_steward <- read.csv("https://files.sesync.org/index.php/s/oHnCkS5sDFgX44c/download")
  eqip_cons_steward$County <- tolower(eqip_cons_steward$County)
  eqip_cons_steward$County <- sapply(eqip_cons_steward$County, simpleCap)
  eqip_cons_steward <- eqip_cons_steward[,2:8]
  
  
  
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
  
  #subsets to midwest
  census <- subset(census, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin" | State == "Nebraska"| State == "Kansas" 
                   | State == "North Dakota" | State == "South Dakota")
  
  census$Value <- as.numeric(census$Value)
  #CENSUS AGGREGATE BASED ON STATE AND COUNTY USING DOLLARS PAID AS THE SUMMARIZED OUTPUT
  census <- aggregate(census$Value, by=list(census$State, census$County, census$Year, census$Data.Item), FUN = "sum")
  colnames(census) <- c("State", "County", "Year", "census_Grouping", "census_Value")
  
  #reshapes so census groupings are actually columns with their aggregated values as observations - by state, county and year
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
  
  
  #creates four variations of loss outputs - total loss, lossperacres, loss per claim, and acreage
  
  library(reshape2) ; damage_Loss <- dcast(damage, State + County + Year + Commodity ~ Damagecause, value.var = c("Loss"), sum)
  library(reshape2) ; damage_Lossperacre <- dcast(damage, State + County + Year + Commodity ~ Damagecause, value.var = c("Lossperacre"), sum)
  library(reshape2) ; damage_Lossperclaim <- dcast(damage, State + County + Year + Commodity ~ Damagecause, value.var = c("Lossperclaim"), sum)
  library(reshape2) ; damage_Acres <- dcast(damage, State + County + Year + Commodity ~ Damagecause, value.var = c("Acres"), sum)
 
  #damage <- spread(damage, Damagecause, Loss_damagecause)
  
  #subset for only commodities in croplist - set in beginning of script
  

  crop_damage_Acres <- subset(damage_Acres, Commodity == croplist)
  crop_damage_Loss <- subset(damage_Loss, Commodity == croplist)
  crop_damage_Lossperacre <- subset(damage_Lossperacre, Commodity == croplist)
  crop_damage_Lossperclaim <- subset(damage_Lossperclaim, Commodity == croplist)
  
  
  #aggregates each by state and county and year
  
  crop_damage_Acres <- aggregate(crop_damage_Acres[,5:38], by = list(crop_damage_Acres$State, crop_damage_Acres$County, crop_damage_Acres$Year), FUN = 'sum' )
  colnames(crop_damage_Acres)[1:3] <- c("State", "County", "Year")
  crop_damage_Loss <- aggregate(crop_damage_Loss[,5:38], by = list(crop_damage_Loss$State, crop_damage_Loss$County, crop_damage_Loss$Year), FUN = 'sum' )
  colnames(crop_damage_Loss)[1:3] <- c("State", "County", "Year")
  crop_damage_Lossperacre <- aggregate(crop_damage_Lossperacre[,5:38], by = list(crop_damage_Lossperacre$State, crop_damage_Lossperacre$County, crop_damage_Lossperacre$Year), FUN = 'sum' )
  colnames(crop_damage_Lossperacre)[1:3] <- c("State", "County", "Year")
   crop_damage_Lossperclaim <- aggregate(crop_damage_Lossperclaim[,5:38], by = list(crop_damage_Lossperclaim$State, crop_damage_Lossperclaim$County, crop_damage_Lossperclaim$Year), FUN = 'sum' )
   colnames(crop_damage_Lossperclaim)[1:3] <- c("State", "County", "Year")
   
  
   countyFIPS <- read.csv("https://nextcloud.sesync.org/index.php/s/wcFmKrSZW6Pr6D2/download")
   
   countyFIPS$FIPS <- sprintf("%05d",countyFIPS$FIPS)
   
   #LANDUSE CORN BELT!
   
   ag_land_corn <- read.csv("https://nextcloud.sesync.org/index.php/s/P92Df7gYgXKjYXa/download")
   colnames(ag_land_corn) <- c("FIPS", "countycode", "row", "column", "type", "State", "Statecode", "label", "County", "cpubval", "Cropland_Acres", "Percent")
   
   ag_land_corn$Cropland_Acres <- as.numeric(as.character(ag_land_corn$Cropland_Acres))
   
   ag_land_corn2 <- subset(ag_land_corn, Cropland_Acres != "NA")
   
   
   alc <- merge(ag_land_corn2, countyFIPS, by  = "FIPS")
   
   #takes the mean of cropland_acres across all practices by state, county
   alc2 <- aggregate(alc$Cropland_Acres, by = list(alc$STATE_NAME, alc$NAME, alc$FIPS), FUN = 'mean')
   colnames(alc2) <- c("State", "County", "FIPS", "AGCENSUS_Cropland_Acres")
   alc2$FIPS <- sprintf("%05d",alc2$FIPS)
 
   
  #climate - pdsi
  
  pdsi_moderate_drought_2007_2012 <- read.csv("https://nextcloud.sesync.org/index.php/s/TYa9pBNQHBc4efj/download")
   
  #merges pdsi to fips
   pdsi <- merge(pdsi_moderate_drought_2007_2012, countyFIPS, by  = "FIPS")
   
  #sums the number of days of moderate to extreme drought (-2 or below) for 2007 to 2012
   pdsi$PDSI_TOTALS <- rowSums(pdsi[,c("X2007","X2008","X2009","X2010","X2011", "X2012" )])
   
   #removes extraneous columns so we only have state and county and PDSI totals
   pdsi <- pdsi[,10:12]
   colnames(pdsi) <- c("State", "County", "PDSI_TOTALS")

   
   #cdl
   
   cdl_diversity <- read.csv("https://nextcloud.sesync.org/index.php/s/RWyocfcwpAobCrq/download")
   
   #merges with fips
   cdl_div <- merge(cdl_diversity, countyFIPS, by = "FIPS")
   
   #reshapes so we have a CDI 2011 and CDI 2012 for each state/county combo
   library(reshape2) ; cdl_div2 <- dcast(cdl_div, STATE_NAME + NAME ~ Year, value.var = c("CDI"))
   
   #changes column names
   colnames(cdl_div2) <- c("State", "County", "CDI_2011", "CDI_2012")
   

   #cash rent
   
   cash_rent <- read.csv("https://nextcloud.sesync.org/index.php/s/rbGosZCQoqT5S8T/download")
   #cash_rent <- cash_rent[,c(2,5,6,7,8,9,10,11,12)]
   cash_rent <- cash_rent[,c(3,6,7,8,9,10,11,12,13)]
   
   colnames(cash_rent) <- c("Year", "RENT_Irrigated_Rent_Cropland", "RENT_NonIrrigated_Rent_Cropland", "RENT_Pastureland", "RENT_average", "State", "County", "RENT_Total_Cropland_Acres", "RENT_Total")
  
   rented_land <- read.csv("https://nextcloud.sesync.org/index.php/s/wWnWqPHwWXPytmQ/download")
   
   rented_land$FIPS <- sprintf("%05d",rented_land$FIPS)
   
   rented_land <- merge(rented_land, countyFIPS, by  = "FIPS")
   
   rented_land <- rented_land[-29]
   rented_land_revised <- rented_land[,25:30]
   #rented_land_revised <- cbind(rented_land$acres.rented.2012, rented_land$Percent.rented.2012, rented_land$STATE_NAME, rented_land$NAME)
   colnames(rented_land_revised) <- c("RENT_acres.rented.2007", "RENT_acres.rented.2012", "RENT_percent.rented.2007",  "RENT_Percent.rented.2012", "State", "County")
   
   # extreme precip
   
   precip_extreme <- read.csv("https://files.sesync.org/index.php/s/eJzjzcooY3feoHR/download")
   
   #removes unnecessary columns
  precip_extreme <- cbind.data.frame(precip_extreme[,3:4], precip_extreme[,8], precip_extreme[,11:12])
  
  #renames columns
   colnames(precip_extreme) <- c("County", "State", "PRECIP_max", "PRECIP_ave", "PRECIP_cov")
   #
   
   
   #--MERGE!
  
  library(tidyverse)
  
  #merge acensus with RMA damages.  Need to define WHICH losstype: Acres, loss, lossperacre, lossperclaim
  merge1 <- merge(eval(parse(text=paste("crop_damage_", losstype, sep=""))), agcensus, by = c("State", "County", "Year"))
  #merge previous merge with eqip
  merge2 <- merge(eqip, merge1, by = c("State", "County", "Year") )
  #merge previous merge with ag landuse in cornbelt states
  merge3 <- merge(merge2, alc2, by = c("State", "County"))
  #merge previous merge with eqip_csp
  merge3a <- merge(merge3, eqip_csp, by = c("State", "County", "Year"))
  #merge previous merge with eqip_cons_security
  merge3b <- merge(merge3a, eqip_cons_security, by = c("State", "County", "Year"))
  #merge previous merge with eqip_cons_stewardship
  merge3c <- merge(merge3b, eqip_cons_steward, by = c("State", "County", "Year"))
  #merge previous merge with census
  merge4 <- merge(merge3c, census, by = c("State", "County", "Year"))
  #merge previous merge with nri
  merge5 <- merge(merge4, nri, by = c("State", "County", "Year"))
  #merge previous merge with commodity loss totals FOR ALL COMMODITIES
  merge6 <- merge(merge5, commodity_loss_total, by = c("State", "County", "Year"))
  #merge previous merge with PDSI
  merge6a <- merge(merge6, pdsi, by = c("State", "County"))
  #merge previous merge with cash rent
  merge6b <- merge(merge6a, cash_rent, by = c("State", "County", "Year"))
  #merge previous merge with cropland diversity for 2011 and 2012
  merge6c <- merge(merge6b, cdl_div2, by = c("State", "County"))
  #merge with previous merge for rented land revised
  merge7 <- merge(merge6c, rented_land_revised,  by = c("State", "County"))
  #merge previous merge with extreme precip for 2008-2012
  merge7a <- merge(merge7, precip_extreme, by = c("State", "County"))
  
  #removes the FIPS column
  merge7 <- subset( merge7a, select = -FIPS )

  #converts NA to zero
  merge7[is.na(merge7)] <- 0 
  
#this adds a prefix to a set of datasets.  Make sure to change the column strings if you
#happen to add other datasets.
  
  colnames(merge7)[4:33] <- paste("EQIP_", colnames(merge7)[4:33], sep = "")
  colnames(merge7)[34:67] <- paste("RMA_", colnames(merge7)[34:67], sep = "")
  colnames(merge7)[68:88] <- paste("AGCENSUS_", colnames(merge7)[68:88], sep = "")
  colnames(merge7)[102:111] <- paste("CENSUS_", colnames(merge7)[102:111], sep = "")
  colnames(merge7)[113:206] <- paste("NRI_", colnames(merge7)[113:206], sep = "")
  
  #--transformed variables
  
  #dependent variable 1
  #cover crops / total cropland
  
  merge7$AGCENSUS_CC_Cropland_Acres_Ratio <- merge7$AGCENSUS_cc_acres / merge7$AGCENSUS_Cropland_Acres
  
  #create name of crops used for RMA - example - for Midwest, we are using corn and soybeans
  croplist_name <- paste(croplist[[1]], "_", croplist[[2]], sep="")
  
  library( taRifx )
  #make sure all independent variables are numeric and state and county are factors
  merge8 <- japply( merge7[,4:228], which(sapply(merge7[,4:228], class)=="factor"), as.numeric )
  merge9 <- japply( merge8, which(sapply(merge8, class)=="integer"), as.numeric )
  
  #puts independent variables and factors (year, state, county, back together)
  merge9 <- cbind(merge7$State, merge7$County, merge7$Year, merge9)
  
  #makes sure state county year are named correctly and factored
  colnames(merge9)[1:3] <- c("State", "County", "Year")
  merge9$State <- factor(merge9$State)
  merge9$County <- factor(merge9$County)
  
  #replace Inf with zero for RMA_lossperacre
  merge9$RMA_Lossperacre[which(!is.finite(merge9$RMA_Lossperacre))] <- 0

  #removes columns that have zero data for all counties.  An example might be an RMA loss type like Hurricanes
  
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
  
  #Converts cropland acreage ratios for cc and no till to log transformed, to derive a more normal distribution.  Optional
  #and commented out for now
  
  #merge9$AGCENSUS_CC_Cropland_Acres_Ratio <- log10(merge9$AGCENSUS_CC_Cropland_Acres_Ratio)
  #merge9$AGCENSUS_NOTILL_Cropland_Acres_Ratio <- log10(merge9$AGCENSUS_notill_acres/data2$AGCENSUS_Cropland_Acres)
  
  #scales and centers the full output in case thats effective.  Creates a separate file
  
  scaled_merge10 <- merge9[, -c(1:3)] <- scale(merge9[, -c(1:3)], center = TRUE, scale = TRUE)
  
  
  #write the combined file to the model_data location for SEM
  #for both scaled and non-scaled.  TWO files are generated below.
  
  write.csv(merge9, file = paste("/nfs/soilsesfeedback-data/model_data/MIDWEST_", croplist_name, "_Model", "_nonscaled.csv", sep=""))
  write.csv(scaled_merge10, file = paste("/nfs/soilsesfeedback-data/model_data/MIDWEST_", croplist_name, "_Model", "_scaled.csv", sep=""))
  



