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
library(maps)

arrange.vars <- function(data, vars){
  ##stop if not a data.frame (but should work for matrices as well)
  stopifnot(is.data.frame(data))
  
  ##sort out inputs
  data.nms <- names(data)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars
  ##sanity checks
  stopifnot( !any(duplicated(var.nms)), 
             !any(duplicated(var.pos)) )
  stopifnot( is.character(var.nms), 
             is.numeric(var.pos) )
  stopifnot( all(var.nms %in% data.nms) )
  stopifnot( all(var.pos > 0), 
             all(var.pos <= var.nr) )
  
  ##prepare output
  out.vec <- character(var.nr)
  out.vec[var.pos] <- var.nms
  out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
  stopifnot( length(out.vec)==var.nr )
  
  ##re-arrange vars by position
  data <- data[ , out.vec]
  return(data)
}




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
  #ADD FIPS
  
  statefips <- read.csv("https://files.sesync.org/index.php/s/HpxsTFNqzZn7XRt/download")
  statefips$StateFips <- sprintf("%02d", statefips$StateFips)
  
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
  #nri <- subset( nri, select = -Fips )
  nri <- subset(nri, Year == 2012)
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
  
  agcensus <- merge(agcensus, statefips, by=("State"))
  
  
  agcensus$CountyFips <- sprintf("%03d", agcensus$CountyFips)
  
  agcensus$Fips <- paste(agcensus$StateFips, agcensus$CountyFips, sep="")
  
  #subset only to midwest states
  agcensus <- subset(agcensus, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin" | State == "Nebraska"| State == "Kansas" 
                     | State == "North Dakota" | State == "South Dakota")
  
  #AGCENSUS AGGREGATE BASED ON STATE AND COUNTY USING DOLLARS PAID AS THE SUMMARIZED OUTPUT
  agcensus <- aggregate(cbind(agcensus$tile_farms,  agcensus$tile_acres, agcensus$tile_acres_avgfarm, agcensus$ditches_farms,        
                              agcensus$ditches_acres, agcensus$ditches_acres_avgfarm, agcensus$consease_farms, agcensus$consease_acres,       
                              agcensus$consease_avgfarm, agcensus$notill_farms, agcensus$notill_acres, agcensus$notill_avgfarm,       
                              agcensus$constill_farms, agcensus$constill_acres, agcensus$constill_avgfarm, agcensus$convtill_farms,       
                              agcensus$convtill_acres, agcensus$convtill_acres.1, agcensus$cc_farms, agcensus$cc_acres,             
                              agcensus$cc_avgfarm), by=list(agcensus$Year, agcensus$Fips), FUN = "sum")
  colnames(agcensus) <- c("Year", "Fips", "tile_farms",            "tile_acres",           
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
  
  eqip$Fips <- sprintf("%05d", eqip$county_code)
  
  
  #EQIP SUBSET TO ONLY STUDY AREA
  eqip <- subset(eqip, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin" | State == "Nebraska"| State == "Kansas" 
                 | State == "North Dakota" | State == "South Dakota")
  
  #EQIP AGGREGATE BASED ON STATE AND COUNTY USING DOLLARS PAID AS THE SUMMARIZED OUTPUT
  eqip <- aggregate(eqip$Dollars.Paid, by=list(eqip$Fips, eqip$Applied.Amount, eqip$Applied.Year, eqip$practice_name), FUN = "sum")
  colnames(eqip) <- c("Fips", "Applied.Amount", "Year", "Practice_Name", "Dollars_Paid")
  
  eqip$id <- seq_len(nrow(eqip))
  library(reshape2) ; eqip <- dcast(eqip, Fips + Year ~ Practice_Name, value.var = "Dollars_Paid", sum)
  eqip <- subset(eqip, Year == 2012)
  
  #eqip <- spread(eqip, Practice_Name, Dollars_Paid)
  
  
  #eqip load using csv - use if you ARE on SESYNC Rstudio server
  #setwd("/nfs/soilsesfeedback-data/data/eqip")
  #eqip <- read.csv("eqip.csv")
  
  #EQIP-CSP
  
  eqip_csp_revised <- read.csv("https://files.sesync.org/index.php/s/JQysQDBmLMcfrGd/download")
  eqip_csp_revised$Fips <- sprintf("%05d", eqip_csp_revised$county_code)
  
  
  eqip_csp_revised_2010_2012 <- subset(eqip_csp_revised, fy == 2010 | fy == 2011 | fy == 2012)
  eq_eqip_fa <- aggregate(eqip_csp_revised_2010_2012$EQIP_FA_PAYMENTS, by=list(eqip_csp_revised_2010_2012$Fips), FUN = "sum" )
  eq_cstp <- aggregate(eqip_csp_revised_2010_2012$CStP_FA_Payments, by=list(eqip_csp_revised_2010_2012$Fips), FUN = "sum" )
  colnames(eq_eqip_fa) <- c("Fips", "EQIP_FA_PAYMENTS_total")
  colnames(eq_cstp) <- c("Fips", "CStP_FA_Payments_total")
  eq_combined <- cbind(eq_eqip_fa, eq_cstp[,2])
  colnames(eq_combined) <- c("Fips", "EQIP_FA_PAYMENTS_total", "CStP_FA_Payments_total")
  eq_combined[is.na(eq_combined)] <- 0 
  eq_combined$EQIP_cstp_combined <- eq_combined$EQIP_FA_PAYMENTS_total + eq_combined$CStP_FA_Payments_total
  
  eq_combined <- merge(eq_combined, countyFIPS, by = "Fips")
  
  eq_combined <- subset(eq_combined, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin" | State == "Nebraska"| State == "Kansas" 
                          | State == "North Dakota" | State == "South Dakota")
  
  eq_combined <- eq_combined[-(5:7)]
  
  #eq_combined$County <- tolower(eq_combined$County)
  #eq_combined$County <- sapply(eq_combined$County, simpleCap)
  
  #eqip_csp <- read.csv("https://files.sesync.org/index.php/s/5Rjpp4z5xe7KCC8/download")
  #eqip_csp$County <- tolower(eqip_csp$County)
  #eqip_csp$County <- sapply(eqip_csp$County, simpleCap)
  #eqip_csp <- eqip_csp[,2:8]
  
  #remove extraneous columns other that state, year, county, and variables
  #eqip_csp <- cbind.data.frame(eqip_csp[,4], eqip_csp[,6], eqip_csp[,7:10])
  #colnames(eqip_csp) <- c("Year", "State", "County", "EQIP_CSP_Contracts", "EQIP_CSP_CONTRACT_ACRES", "EQIP_CSP_FA_OBLIGATIONS", "EQIP_CSP_FA_PAYMENTS" )
  
  #EQIP-Cons-security
  
  #eqip_cons_security <- read.csv("https://files.sesync.org/index.php/s/ZsX6SYFmc8Tsr77/download")
  #eqip_cons_security$County <- tolower(eqip_cons_security$County)
  #eqip_cons_security$County <- sapply(eqip_cons_security$County, simpleCap)
  #eqip_cons_security <- eqip_cons_security[,2:8]
  
  #EQIP-Cons-stewardship
  
  #eqip_cons_steward <- read.csv("https://files.sesync.org/index.php/s/oHnCkS5sDFgX44c/download")
  #eqip_cons_steward$County <- tolower(eqip_cons_steward$County)
  #eqip_cons_steward$County <- sapply(eqip_cons_steward$County, simpleCap)
  #eqip_cons_steward <- eqip_cons_steward[,2:8]
  
  
  
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
  
  census$State.ANSI <- sprintf("%02d", census$State.ANSI)
  census$County.ANSI <- sprintf("%03d", census$County.ANSI)
  census$Fips <- paste(census$State.ANSI, census$County.ANSI, sep="")
  
  #subsets to midwest
  census <- subset(census, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin" | State == "Nebraska"| State == "Kansas" 
                   | State == "North Dakota" | State == "South Dakota")
  
  census$Value <- as.numeric(census$Value)
  #CENSUS AGGREGATE BASED ON STATE AND COUNTY USING DOLLARS PAID AS THE SUMMARIZED OUTPUT
  census <- aggregate(census$Value, by=list(census$Fips, census$Year, census$Data.Item), FUN = "sum")
  colnames(census) <- c("Fips", "Year", "census_Grouping", "census_Value")
  
  #reshapes so census groupings are actually columns with their aggregated values as observations - by state, county and year
  library(reshape2) ; census <- dcast(census, Fips + Year ~ census_Grouping, value.var = "census_Value", sum)
  
  census <- subset(census, Year == 2012)
  
  
  #census <- spread(census, census_Grouping, census_Value)
  
  #census load using csv - use if you ARE on SESYNC Rstudio server
  #setwd("/nfs/soilsesfeedback-data/data/census")
  #census <- read.csv("Census_States_CountyDem.csv")
  
  #RMA COMMODITY
  
  countyFIPS <- read.csv("https://nextcloud.sesync.org/index.php/s/wcFmKrSZW6Pr6D2/download")
  colnames(countyFIPS) <- c("ID", "State", "County", "Fips")
  countyFIPS$Fips <- sprintf("%05d",countyFIPS$Fips)
  
  
  #RMA by commodity and damage cause, includes claim counts
  commodity <- read.csv("https://nextcloud.sesync.org/index.php/s/zFwyd64NZJEyrgr/download")

  
  #commodity load using csv - use if you ARE on SESYNC Rstudio server
  #setwd("/nfs/soilsesfeedback-data/data/RMA/RMA_csvfiles")
  #commodity <- read.csv("RMA_combined.csv")
  
  colnames(commodity) <- c("ID", "Year", "State", "County", "Commodity", "Loss", "Count", "Acres", "Lossperacre", "Lossperclaim", "Acresperclaim")
  commodity$State <- state.name[match(commodity$State,state.abb)]
  commodity <- merge(commodity, countyFIPS, by=c("State", "County"))
  
  
  
  #RMA COMMODITY FIX CAPITIALIZATION AND THEN SUBSET TO ONLY STUDY AREA
  commodity <- subset(commodity, State == "Illinois" | State == "Indiana" | State == "Iowa" | 
                        State == "Michigan" | State == "Minnesota" | State == "Missouri" | 
                        State == "Ohio" | State == "Wisconsin" | State =="Nebraska" | State == "Kansas" 
                      | State == "North Dakota" | State == "South Dakota")
  #colnames(commodity) <- c("ID", "Year", "State", "County", "Commodity", "Loss_commodity", "Count_commodity")
  
  
  
  commodity_loss_total <- aggregate(list(commodity$Acres, commodity$Loss, commodity$Lossperacre, commodity$Lossperclaim, commodity$Acresperclaim, commodity$Count), by = list(commodity$Fips, commodity$Year), FUN ="sum")
  colnames(commodity_loss_total) <- c("Fips", "Year", "RMA_Acres", "RMA_Loss", "RMA_Lossperacre", "RMA_Lossperclaim", "RMA_Acresperclaim", "RMA_Count")
  commodity_loss_total <- subset(commodity_loss_total, Year == 2012)
  
  
  #RMA DAMAGE
  
  #RMA by damage cause, includes claim counts
  damage <- read.csv("https://nextcloud.sesync.org/index.php/s/W5kztdb5ZkxRptT/download")
  
  #damage load using csv - use if you ARE on SESYNC Rstudio server
  #setwd("/nfs/soilsesfeedback-data/data/RMA/")
  #damage <- read.csv("RMA_damage_combined.csv", strip.white=TRUE)
  
  
  colnames(damage) <- c("ID", "Year", "State", "County", "Commodity", "Damagecause", "Loss", "Count", "Acres", "Lossperacre", "Lossperclaim", "Acresperclaim")
  damage$State <- state.name[match(damage$State,state.abb)]
    damage <- merge(damage, countyFIPS, by=c("State", "County"))
    
  #RMA DAMAGE FIX CAPITIALIZATION AND THEN SUBSET TO ONLY STUDY AREA
  
  damage <- subset(damage, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin" | State == "Nebraska"| State == "Kansas" 
                   | State == "North Dakota" | State == "South Dakota")
  #colnames(damage) <- c("ID", "Year", "State", "County", "Commodity", "Damagecause", "Loss_damagecause", "Count_damagecause")
  #damage <- aggregate(damage$Loss, by=list(damage$Commodity, damage$Damagecause, damage$County, damage$State, damage$Year), FUN = "sum")
  #colnames(damage) <- c("Commodity", "Damagecause", "County", "State", "Year", "Loss")
  
  
  #creates four variations of loss outputs - total loss, lossperacres, loss per claim, and acreage
  
  library(reshape2) ; damage_Loss <- dcast(damage, Fips + Year + Commodity ~ Damagecause, value.var = c("Loss"), sum)
  library(reshape2) ; damage_Lossperacre <- dcast(damage, Fips + Year + Commodity ~ Damagecause, value.var = c("Lossperacre"), sum)
  library(reshape2) ; damage_Lossperclaim <- dcast(damage, Fips + Year + Commodity ~ Damagecause, value.var = c("Lossperclaim"), sum)
  library(reshape2) ; damage_Acres <- dcast(damage, Fips + Year + Commodity ~ Damagecause, value.var = c("Acres"), sum)
 
  #damage <- spread(damage, Damagecause, Loss_damagecause)
  
  #subset for only commodities in croplist - set in beginning of script
  

  crop_damage_Acres <- subset(damage_Acres, Commodity == croplist)
  crop_damage_Loss <- subset(damage_Loss, Commodity == croplist)
  crop_damage_Lossperacre <- subset(damage_Lossperacre, Commodity == croplist)
  crop_damage_Lossperclaim <- subset(damage_Lossperclaim, Commodity == croplist)
  
  
  #aggregates each by state and county and year
  
  crop_damage_Acres <- aggregate(crop_damage_Acres[,4:37], by = list(crop_damage_Acres$Fips, crop_damage_Acres$Year), FUN = 'sum' )
  colnames(crop_damage_Acres)[1:2] <- c("Fips", "Year")
  crop_damage_Loss <- aggregate(crop_damage_Loss[,4:37], by = list(crop_damage_Loss$Fips,  crop_damage_Loss$Year), FUN = 'sum' )
  colnames(crop_damage_Loss)[1:2] <- c("Fips", "Year")
  crop_damage_Lossperacre <- aggregate(crop_damage_Lossperacre[,4:37], by = list(crop_damage_Lossperacre$Fips,  crop_damage_Lossperacre$Year), FUN = 'sum' )
  colnames(crop_damage_Lossperacre)[1:2] <- c("Fips", "Year")
   crop_damage_Lossperclaim <- aggregate(crop_damage_Lossperclaim[,4:37], by = list(crop_damage_Lossperclaim$Fips,  crop_damage_Lossperclaim$Year), FUN = 'sum' )
   colnames(crop_damage_Lossperclaim)[1:2] <- c("Fips", "Year")
   
   
   crop_damage_Acres <- subset(crop_damage_Acres, Year == 2012)
   crop_damage_Loss <- subset(crop_damage_Loss, Year == 2012)
   crop_damage_Lossperacre <- subset(crop_damage_Lossperacre, Year == 2012)
   crop_damage_Lossperclaim <- subset(crop_damage_Lossperclaim, Year == 2012)
   
   #countyFIPS <- read.csv("https://nextcloud.sesync.org/index.php/s/wcFmKrSZW6Pr6D2/download")
   
   #countyFIPS$FIPS <- sprintf("%05d",countyFIPS$FIPS)
   
   #LANDUSE CORN BELT!
   
   ag_land_corn_IN <- read.csv("https://files.sesync.org/index.php/s/G9gPNofZSgQHK7y/download")
   colnames(ag_land_corn_IN) <- c("StateFips", "CountyFips", "StateCountyName", "Description", "Acres")
   ag_land_corn_IN$Acres <- as.numeric(ag_land_corn_IN$Acres)
   
   ag_land_corn_IA <- read.csv("https://files.sesync.org/index.php/s/fBjo8rScMqgb7w9/download")
   colnames(ag_land_corn_IA) <- c("StateFips", "CountyFips", "StateCountyName", "Description", "Acres")
   ag_land_corn_IA$Acres <- as.numeric(ag_land_corn_IA$Acres)
   
   ag_land_corn_IL <- read.csv("https://files.sesync.org/index.php/s/nasXnagFAMC5Lio/download")
   colnames(ag_land_corn_IL) <- c("StateFips", "CountyFips", "StateCountyName", "Description", "Acres")
   ag_land_corn_IL$Acres <- as.numeric(ag_land_corn_IL$Acres)
   
   ag_land_corn_MN <- read.csv("https://files.sesync.org/index.php/s/FFcwPBdc5nGJLmE/download")
   colnames(ag_land_corn_MN) <- c("StateFips", "CountyFips", "StateCountyName", "Description", "Acres")
   ag_land_corn_MN$Acres <- as.numeric(ag_land_corn_MN$Acres)
   
   ag_land_corn_MI <- read.csv("https://files.sesync.org/index.php/s/kJbR3GtGyHiLr8L/download")
   colnames(ag_land_corn_MI) <- c("StateFips", "CountyFips", "StateCountyName", "Description", "Acres")
   ag_land_corn_MI$Acres <- as.numeric(ag_land_corn_MI$Acres)
   
   ag_land_corn_KS <- read.csv("https://files.sesync.org/index.php/s/8r8yPptfAKWm7kj/download")
   colnames(ag_land_corn_KS) <- c("StateFips", "CountyFips", "StateCountyName", "Description", "Acres")
   ag_land_corn_KS$Acres <- as.numeric(ag_land_corn_KS$Acres)
   
   ag_land_corn_NE <- read.csv("https://files.sesync.org/index.php/s/c5wXofrWryXXfjd/download")
   colnames(ag_land_corn_NE) <- c("StateFips", "CountyFips", "StateCountyName", "Description", "Acres")
   ag_land_corn_NE$Acres <- as.numeric(ag_land_corn_NE$Acres)
   
   ag_land_corn_MO <- read.csv("https://files.sesync.org/index.php/s/6Q4NNxETBBwPdQ5/download")
   colnames(ag_land_corn_MO) <- c("StateFips", "CountyFips", "StateCountyName", "Description", "Acres")
   ag_land_corn_MO$Acres <- as.numeric(ag_land_corn_MO$Acres)
   
   ag_land_corn_WI <- read.csv("https://files.sesync.org/index.php/s/NCNmpLBP2nQLN8Q/download")
   colnames(ag_land_corn_WI) <- c("StateFips", "CountyFips", "StateCountyName", "Description", "Acres")
   ag_land_corn_WI$Acres <- as.numeric(ag_land_corn_WI$Acres)
   
   ag_land_corn_SD <- read.csv("https://files.sesync.org/index.php/s/3YtXDXTkEELNGXB/download")
   colnames(ag_land_corn_SD) <- c("StateFips", "CountyFips", "StateCountyName", "Description", "Acres")
   ag_land_corn_SD$Acres <- as.numeric(ag_land_corn_SD$Acres)
   
   ag_land_corn_OH <- read.csv("https://files.sesync.org/index.php/s/mtQmqa2EmXXP9Ai/download")
   colnames(ag_land_corn_OH) <- c("StateFips", "CountyFips", "StateCountyName", "Description", "Acres")
   ag_land_corn_OH$Acres <- as.numeric(ag_land_corn_OH$Acres)
   
   ag_land_corn_ND <- read.csv("https://files.sesync.org/index.php/s/Gf5JWTjAX92JxTr/download")
   colnames(ag_land_corn_ND) <- c("StateFips", "CountyFips", "StateCountyName", "Description", "Acres")
   ag_land_corn_ND$Acres <- as.numeric(ag_land_corn_ND$Acres)
   
   ag_land_corn_merged <- rbind(ag_land_corn_IN, ag_land_corn_IA, ag_land_corn_IL, ag_land_corn_MN, ag_land_corn_MI, ag_land_corn_KS, ag_land_corn_NE, ag_land_corn_MO, ag_land_corn_WI, ag_land_corn_SD, ag_land_corn_OH, ag_land_corn_ND)

   colnames(ag_land_corn_merged) <- c("StateFips", "CountyFips", "StateCountyName", "Description", "Acres")
   
   ag_land_corn_merged$StateCountyName <- as.character(ag_land_corn_merged$StateCountyName)
   ag_land_corn_merged$Acres <- as.numeric(ag_land_corn_merged$Acres)
   
   ag_land_corn_merged$StateFips <- sprintf("%02d", ag_land_corn_merged$StateFips)
   ag_land_corn_merged$CountyFips <- sprintf("%03d", ag_land_corn_merged$CountyFips)
   ag_land_corn_merged$FIPS <- paste(ag_land_corn_merged$StateFips, ag_land_corn_merged$CountyFips, sep="")
   alc_new <- cbind(ag_land_corn_merged[,4:6])
   
   AGCENSUS_county_covercrop_acres <- subset(alc_new, Description == "Land planted to a cover crop (excluding CRP), Acres, 2012")
   colnames(AGCENSUS_county_covercrop_acres) <- c("Description", "Cover_Acres", "Fips")
   AGCENSUS_county_covercrop_acres <- cbind.data.frame(AGCENSUS_county_covercrop_acres[,2:3])
   
   AGCENSUS_county_notill_acres <- subset(alc_new, Description == "Land on which no-till practices were used, Acres, 2012")
   colnames(AGCENSUS_county_notill_acres) <- c("Description", "Notill_Acres", "Fips")
   AGCENSUS_county_notill_acres <- cbind.data.frame(AGCENSUS_county_notill_acres[,2:3])
   
   AGCENSUS_county_conservationtill_acres <- subset(alc_new, Description == "Land on which conservation tillage was used, Acres, 2012")
   colnames(AGCENSUS_county_conservationtill_acres) <- c("Description", "Conservationtill_Acres", "Fips")
   AGCENSUS_county_conservationtill_acres <- cbind.data.frame(AGCENSUS_county_conservationtill_acres[,2:3])
   
   AGCENSUS_county_conventionaltill_acres <- subset(alc_new, Description == "Land on which conventional tillage was used, Acres, 2012")
   colnames(AGCENSUS_county_conventionaltill_acres) <- c("Description", "Conventionaltill_Acres", "Fips")
   AGCENSUS_county_conventionaltill_acres <- cbind.data.frame(AGCENSUS_county_conventionaltill_acres[,2:3])
   
   AGCENSUS_ac1 <- cbind.data.frame(AGCENSUS_county_covercrop_acres, AGCENSUS_county_notill_acres, AGCENSUS_county_conservationtill_acres, AGCENSUS_county_conventionaltill_acres )
   AGCENSUS_ac2 <- cbind.data.frame(AGCENSUS_ac1[,1], AGCENSUS_ac1[,3], AGCENSUS_ac1[,5], AGCENSUS_ac1[,7:8])
   colnames(AGCENSUS_ac2) <- c("Cover_Acres", "Notill_Acres", "Conservationtill_Acres", "Conventionaltill_Acres", "Fips")
   
   
   #total cropland acres
   
   
   
   totalcropland <- read.csv("https://files.sesync.org/index.php/s/Ly4TyC3RipXdkSG/download")
   
   totalcropland$state_fips_code <- sprintf("%02d", totalcropland$state_fips_code)
   totalcropland$county_code <- sprintf("%03d", totalcropland$county_code)
   totalcropland$Fips <- paste(totalcropland$state_fips_code, totalcropland$county_code, sep="")
   
   totalcropland <- cbind.data.frame(totalcropland[,13], totalcropland[,21], totalcropland[,41])
   colnames(totalcropland) <- c("Year", "Cropland_Acres", "Fips")
   
   totalcropland$Cropland_Acres <- as.numeric(gsub(",","",totalcropland$Cropland_Acres))
   
   totalcropland <- subset(totalcropland, Year == 2012)
   
   totalcropland <- merge(totalcropland, countyFIPS, by = "Fips")
   
   totalcropland <- subset(totalcropland, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin" | State == "Nebraska"| State == "Kansas" 
                  | State == "North Dakota" | State == "South Dakota")
   
   totalcropland <- totalcropland[-(4:6)]
   
   
   #alc_new <- aggregate(ag_land_corn_merged$Acres, by=list(ag_land_corn_merged$StateCountyName), FUN = "sum")
   
   #ag_land_corn <- read.csv("https://nextcloud.sesync.org/index.php/s/P92Df7gYgXKjYXa/download")
   #colnames(ag_land_corn) <- c("FIPS", "countycode", "row", "column", "type", "State", "Statecode", "label", "County", "cpubval", "Cropland_Acres", "Percent")
   
   #ag_land_corn$Cropland_Acres <- as.numeric(as.character(ag_land_corn$Cropland_Acres))
   
   #ag_land_corn2 <- subset(ag_land_corn, Cropland_Acres != "NA")
   
   
   #alc <- merge(ag_land_corn2, countyFIPS, by  = "FIPS")
   
   #takes the mean of cropland_acres across all practices by state, county
   #alc2 <- aggregate(alc$Cropland_Acres, by = list(alc$STATE_NAME, alc$NAME, alc$FIPS), FUN = 'mean')
   #colnames(alc2) <- c("State", "County", "FIPS", "AGCENSUS_Cropland_Acres")
   #alc2$FIPS <- sprintf("%05d",alc2$FIPS)
 
   
  #climate - pdsi
  
  pdsi_moderate_drought_2007_2012 <- read.csv("https://nextcloud.sesync.org/index.php/s/TYa9pBNQHBc4efj/download")
   colnames(pdsi_moderate_drought_2007_2012) <- c("ID", "2007", "2008", "2009", "2010", "2011", "2012", "Fips")
   
  #merges pdsi to fips
   #pdsi <- merge(pdsi_moderate_drought_2007_2012, countyFIPS, by  = "Fips")
   pdsi <- pdsi_moderate_drought_2007_2012
   
  #sums the number of days of moderate to extreme drought (-2 or below) for 2007 to 2012
   pdsi$PDSI_TOTALS <- rowSums(pdsi[,c("2007","2008","2009","2010","2011", "2012" )])
   
   #removes extraneous columns so we only have state and county and PDSI totals
   pdsi <- pdsi[,8:9]
   colnames(pdsi) <- c("Fips", "PDSI_TOTALS")
   
   pdsi <- merge(pdsi, countyFIPS, by = "Fips")
   
   pdsi <- subset(pdsi, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin" | State == "Nebraska"| State == "Kansas" 
                      | State == "North Dakota" | State == "South Dakota")
   
   pdsi <- pdsi[-(3:5)]
   
   #female operators
   
   femaleop <- read.csv("https://files.sesync.org/index.php/s/fZLfsteAExAg5PJ/download")
   femaleop <- subset(femaleop, Year == "2012")   
   femaleop <- femaleop[-1]
   femaleop <- femaleop[-3]
   
   femaleop <- merge(femaleop, countyFIPS, by=c("State", "County"))
   femaleop <- cbind.data.frame(femaleop[,3:5], femaleop[,7])
   colnames(femaleop) <- c("all_operators", "female_operators", "percent_female", "Fips")
   
   femaleop <- merge(femaleop, countyFIPS, by = "Fips")
   
   femaleop <- subset(femaleop, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin" | State == "Nebraska"| State == "Kansas" 
                           | State == "North Dakota" | State == "South Dakota")
   
   femaleop <- femaleop[-(5:7)]
   
   #diversity index
   

   racediversity <- read.csv("https://files.sesync.org/index.php/s/a2Hrd75kAeTWNaD/download")
   colnames(racediversity) <- c("ID", "entropy", "Fips", "Year")
   racediversity <- subset(racediversity, Year == "2012")
   
   
   racediversity$Fips <- as.character(racediversity$Fips)
   #racediversity <- merge(countyFIPS, racediversity, by=c("FIPS"))
   racediversity <- cbind.data.frame(racediversity[,2], racediversity[,3])
   colnames(racediversity) <- c("RACE_Entropy", "Fips")
   
   
   racediversity <- merge(racediversity, countyFIPS, by = "Fips")
   
   racediversity <- subset(racediversity, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin" | State == "Nebraska"| State == "Kansas" 
                 | State == "North Dakota" | State == "South Dakota")
   
   #
   
   #
   #HDI
  HDI <- read.csv("https://files.sesync.org/index.php/s/FGn3SZsZj7cLGEB/download")
   
  
  
  HDI$County <- gsub( " County", "", as.character(HDI$County))
  HDI <- HDI[-1]
  HDI <- HDI[-4]
  
  HDI <- HDI[-3]
  
  HDI <- merge(HDI, countyFIPS, by=c("State", "County"))
  HDI <- cbind.data.frame(HDI[,3:6], HDI[,8])
  colnames(HDI) <- c("Health.Index", "Education.Index", "Income.Index", "WB.Index", "Fips")
  
  HDI <- merge(HDI, countyFIPS, by = "Fips")
  
  HDI <- subset(HDI, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin" | State == "Nebraska"| State == "Kansas" 
                           | State == "North Dakota" | State == "South Dakota")
  HDI <- HDI[-(6:8)]
  
  
  #farmproductioncosts
  
  
  FARMCOSTS <- read.csv("https://files.sesync.org/index.php/s/cK5Xgp7Axqx3zs6/download")
  #FARMCOSTS <- FARMCOSTS[-1]
  FARMCOSTS$long_state <- as.character(FARMCOSTS$long_state)
  FARMCOSTS$county <- as.character(FARMCOSTS$county)
  
  FARMCOSTS$State <- sapply(FARMCOSTS$long_state, simpleCap)
  FARMCOSTS$County <- sapply(FARMCOSTS$county, simpleCap)
  FARMCOSTS <- FARMCOSTS[-68]
  FARMCOSTS <- FARMCOSTS[-68]
  FARMCOSTS <- FARMCOSTS[-68]
  FARMCOSTS <- FARMCOSTS[-68]
  FARMCOSTS <- FARMCOSTS[-68]
  FARMCOSTS <- FARMCOSTS[-69]
  
  #FARMCOSTS <- merge(FARMCOSTS, countyFIPS, by=c("State", "County"))
  #FARMCOSTS <- cbind.data.frame(FARMCOSTS[,3:69], FARMCOSTS[,71])
  colnames(FARMCOSTS)[1] <- "Fips"
  
  #FARMCOSTS <- merge(FARMCOSTS, countyFIPS, by = "Fips")
  
  FARMCOSTS <- subset(FARMCOSTS, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin" | State == "Nebraska"| State == "Kansas" 
                | State == "North Dakota" | State == "South Dakota")
  
  FARMCOSTS <- FARMCOSTS[-(68)]
  
   #cdl
  
  countyFIPS <- read.csv("https://nextcloud.sesync.org/index.php/s/wcFmKrSZW6Pr6D2/download")
  colnames(countyFIPS) <- c("ID", "State", "County", "Fips")
  countyFIPS$Fips <- sprintf("%05d",countyFIPS$Fips)
  
   
   cdl_diversity <- read.csv("https://files.sesync.org/index.php/s/RWyocfcwpAobCrq/download")
   colnames(cdl_diversity)[2] <- "Fips"
   cdl_diversity$Fips <- sprintf("%05d", cdl_diversity$Fips)
   
   cdl_div2 <- cdl_diversity
   #merges with fips
   
   
   #reshapes so we have a CDI 2011 and CDI 2012 for each state/county combo
   #library(reshape2) ; cdl_div2 <- dcast(cdl_div, Fips ~ Year, value.var = c("CDI"))
   
   cdl_div2 <- cbind.data.frame(cdl_div2[,2], cdl_div2[,4])
   #changes column names
   colnames(cdl_div2) <- c("Fips", "CDI_2012")
   
   cdl_div2 <- merge(cdl_div2, countyFIPS, by = "Fips")
   
   
   cdl_div2 <- subset(cdl_div2, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin" | State == "Nebraska"| State == "Kansas" 
                       | State == "North Dakota" | State == "South Dakota")
   cdl_div2 <- cdl_div2[,1:2]

   #cash rent
   countyFIPS <- read.csv("https://nextcloud.sesync.org/index.php/s/wcFmKrSZW6Pr6D2/download")
   colnames(countyFIPS) <- c("ID", "State", "County", "Fips")
   countyFIPS$Fips <- sprintf("%05d",countyFIPS$Fips)
   #countyFIPS <- read.csv("https://nextcloud.sesync.org/index.php/s/wcFmKrSZW6Pr6D2/download")
   
   #countyFIPS$FIPS <- sprintf("%05d",countyFIPS$FIPS)
   
   
   
   cash_rent <- read.csv("https://nextcloud.sesync.org/index.php/s/rbGosZCQoqT5S8T/download")
   cash_rent <- merge(cash_rent, countyFIPS, by=c("State", "County"))
   
   #cash_rent <- cash_rent[,c(2,5,6,7,8,9,10,11,12)]
   cash_rent <- cash_rent[,c(5,8,9,10,11,12,13,15)]
   
   #cash_rent <- merge(cash_rent, countyFIPS, by=c("State", "County"))
   
   
   colnames(cash_rent) <- c("Year", "RENT_Irrigated_Rent_Cropland", "RENT_NonIrrigated_Rent_Cropland", "RENT_Pastureland", "RENT_average", "RENT_Total_Cropland_Acres", "RENT_Total", "Fips")
  cash_rent <- subset(cash_rent, Year == 2012)
  
  
   #rented land
  
   rented_land <- read.csv("https://nextcloud.sesync.org/index.php/s/wWnWqPHwWXPytmQ/download")
   
   rented_land$Fips <- sprintf("%05d",rented_land$FIPS)
   
   rented_land <- merge(rented_land, countyFIPS, by  = "Fips")
   
   rented_land <- subset(rented_land, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin" | State == "Nebraska"| State == "Kansas" 
                    | State == "North Dakota" | State == "South Dakota")
   
   
  rented_land <- rented_land[-(2:25)]
   rented_land_revised <- rented_land[,1:5]
   #rented_land_revised <- cbind(rented_land$acres.rented.2012, rented_land$Percent.rented.2012, rented_land$STATE_NAME, rented_land$NAME)
   colnames(rented_land_revised) <- c("Fips", "RENT_acres.rented.2007", "RENT_acres.rented.2012", "RENT_percent.rented.2007",  "RENT_Percent.rented.2012")
   
   
   
   # extreme precip
   
   precip_extreme <- read.csv("https://files.sesync.org/index.php/s/eJzjzcooY3feoHR/download")
   
   #removes unnecessary columns
  precip_extreme <- cbind.data.frame(precip_extreme[,2], precip_extreme[,8], precip_extreme[,11:12])
  
  #renames columns
   colnames(precip_extreme) <- c("Fips", "PRECIP_max", "PRECIP_ave", "PRECIP_cov")
   
   #merges with fips
   precip_extreme <- merge(precip_extreme, countyFIPS, by = "Fips")
   
   precip_extreme <- subset(precip_extreme, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin" | State == "Nebraska"| State == "Kansas" 
                         | State == "North Dakota" | State == "South Dakota")
   
   
   #
   
   #revised RMA
   
  RMA_revised <- read.csv("https://files.sesync.org/index.php/s/LDCrcXRgy2Pwp3F/download")
 RMA_revised <- RMA_revised[-1]
 RMA_revised <- RMA_revised[-1]
 RMA_revised <- RMA_revised[-9]
 RMA_revised$fips <- sprintf("%05d",RMA_revised$fips)
colnames(RMA_revised)[1] <- "Fips"
 
#soils

soils <- read.csv("https://files.sesync.org/index.php/s/z3fZWwXHnBAD8TG/download")
colnames(soils)[2] <- "Fips"




soils <- merge(soils, countyFIPS, by = "Fips")

soils <- subset(soils, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin" | State == "Nebraska"| State == "Kansas" 
                         | State == "North Dakota" | State == "South Dakota")

soils <- soils[-2]
soils <- soils[-(8:10)]

#--agcensus tables provided by J. Arbuckle in October 2019.  

agcensus_table1 <- read.csv2("https://files.sesync.org/index.php/s/tFRELytxbWAgyMY/download", header = TRUE, sep = ",")
agcensus_table1 <- agcensus_table1[-2]
agcensus_table1 <- agcensus_table1[-2]
agcensus_table1 <- agcensus_table1[-2]

colnames(agcensus_table1) <- c("Fips", "Type", "Value" )

agcensus_table1 <- subset(agcensus_table1, Type == "Commodity credit corporation loans \\ Total ($1,000, 2012)" | 
                            Type == "Government payments \\ Total received \\ Amount from conservation reserve, wetlands reserve, farmable wetlands, and conservation reserve enhancement programs ($1,000, 2012)" | 
                            Type == "Government payments \\ Total received \\ Amount from conservation reserve, wetlands reserve, farmable wetlands, and conservation reserve enhancement programs \\ Average per farm (dollars, 2012)" | 
                            Type == "Government payments \\ Total received \\ Amount from other federal farm programs ($1,000, 2012)" | 
                            Type == "Government payments \\ Total received \\ Amount from other federal farm programs \\ Average per farm (dollars, 2012)"  | 
                            Type == "Government payments \\ Total received \\ Average per farm (dollars, 2012)")

levels(agcensus_table1$Type)[levels(agcensus_table1$Type)=="Commodity credit corporation loans \\ Total ($1,000, 2012)"] <- "corp_loans_2012"
levels(agcensus_table1$Type)[levels(agcensus_table1$Type)=="Government payments \\ Total received \\ Amount from conservation reserve, wetlands reserve, farmable wetlands, and conservation reserve enhancement programs ($1,000, 2012)"] <- "payments_reserve_total_2012"
levels(agcensus_table1$Type)[levels(agcensus_table1$Type)=="Government payments \\ Total received \\ Amount from conservation reserve, wetlands reserve, farmable wetlands, and conservation reserve enhancement programs \\ Average per farm (dollars, 2012)"] <- "payments_reserve_aveperfarm_2012"
levels(agcensus_table1$Type)[levels(agcensus_table1$Type)=="Government payments \\ Total received \\ Amount from other federal farm programs ($1,000, 2012)"] <- "payments_fedfarmprograms_total_2012"
levels(agcensus_table1$Type)[levels(agcensus_table1$Type)=="Government payments \\ Total received \\ Amount from other federal farm programs \\ Average per farm (dollars, 2012)"] <- "payments_fedfarmprograms_aveperfarm_2012"
levels(agcensus_table1$Type)[levels(agcensus_table1$Type)=="Government payments \\ Total received \\ Average per farm (dollars, 2012)"] <- "payments_received_aveperfarm_2012"

agcensus_table1$Type <- factor(agcensus_table1$Type)
agcensus_table1$Value <- as.numeric(agcensus_table1$Value)

#agcensus_table1 <- merge(agcensus_table1, countyFIPS, by = "Fips")

library(reshape2) ; agcensus_table1 <- dcast(agcensus_table1, Fips ~ Type, value.var = "Value", sum)


#state summaries

agcensus_table2 <- read.csv2("https://files.sesync.org/index.php/s/jc6nCzbqHqanzoD/download", header = TRUE, sep = ",")

agcensus_table2 <- agcensus_table2[-2]
agcensus_table2 <- agcensus_table2[-2]
agcensus_table2 <- agcensus_table2[-2]

colnames(agcensus_table2) <- c("Fips", "Type", "Value" )

agcensus_table2 <- subset(agcensus_table2, Type == "Land in farms \\ Average size of farm (acres)" | 
                            Type == "Land in farms \\ Median size of farm (acres)" | 
                            Type == "Estimated market value of land and buildings \\ Average per farm (dollars)" | 
                            Type == "Estimated market value of land and buildings \\ Average per acre (dollars)"  | 
                            Type == "Estimated market value of all machinery and equipment \\ Average per farm (dollars)" |
                            Type == "Market value of agricultural products sold (see text) \\ Average per farm (dollars)" |
  Type == "Market value of agricultural products sold (see text) \\ Crops, including nursery and greenhouse crops ($1,000)" |
  Type == "Market value of agricultural products sold (see text) \\ Livestock, poultry, and their products ($1,000)" |
  Type == "Total income from farm-related sources, gross before taxes and expenses (see text) ($1,000)" |
  Type == "Total farm production expenses ($1,000)" |
  Type == "Total farm production expenses \\ Average per farm (dollars)" |
  Type == "Net cash farm income of operation (see text) \\ Average per farm (dollars)")
  

levels(agcensus_table2$Type)[levels(agcensus_table2$Type)=="Land in farms \\ Average size of farm (acres)"] <- "farmland_avesizefarm"
levels(agcensus_table2$Type)[levels(agcensus_table2$Type)=="Land in farms \\ Median size of farm (acres)"] <- "farmland_mediansizefarm"
levels(agcensus_table2$Type)[levels(agcensus_table2$Type)=="Estimated market value of land and buildings \\ Average per farm (dollars)"] <- "marketvalue_aveperfarm"
levels(agcensus_table2$Type)[levels(agcensus_table2$Type)=="Estimated market value of land and buildings \\ Average per acre (dollars)"] <- "marketvalue_aveperacre"
levels(agcensus_table2$Type)[levels(agcensus_table2$Type)=="Estimated market value of all machinery and equipment \\ Average per farm (dollars)"] <- "marketvalue_equip_aveperfarm"
levels(agcensus_table2$Type)[levels(agcensus_table2$Type)=="Market value of agricultural products sold (see text) \\ Average per farm (dollars)"] <- "marketvalue_agproducts_aveperfarm"
levels(agcensus_table2$Type)[levels(agcensus_table2$Type)=="Market value of agricultural products sold (see text) \\ Crops, including nursery and greenhouse crops ($1,000)"] <- "marketvalue_agproducts_crops"
levels(agcensus_table2$Type)[levels(agcensus_table2$Type)=="Market value of agricultural products sold (see text) \\ Livestock, poultry, and their products ($1,000)"] <- "marketvalue_agproducts_livestock"
levels(agcensus_table2$Type)[levels(agcensus_table2$Type)=="Total income from farm-related sources, gross before taxes and expenses (see text) ($1,000)"] <- "income_farmsources_gross"
levels(agcensus_table2$Type)[levels(agcensus_table2$Type)=="Total farm production expenses ($1,000)"] <- "farm_expenses"
levels(agcensus_table2$Type)[levels(agcensus_table2$Type)=="Total farm production expenses \\ Average per farm (dollars)"] <- "farmproduction_aveperfarm"
levels(agcensus_table2$Type)[levels(agcensus_table2$Type)=="Net cash farm income of operation (see text) \\ Average per farm (dollars)"] <- "netincome_aveperfarm"



agcensus_table2$Type <- factor(agcensus_table2$Type)
agcensus_table2$Value <- as.numeric(agcensus_table2$Value)

#agcensus_table2 <- merge(agcensus_table2, countyFIPS, by = "Fips")

library(reshape2) ; agcensus_table2 <- dcast(agcensus_table2, Fips ~ Type, value.var = "Value", sum)


   #--MERGE!
  
  library(tidyverse)

 losstype <- "Acres"
  
  #merge acensus with RMA damages.  Need to define WHICH losstype: Acres, loss, lossperacre, lossperclaim
  merge1 <- merge(eval(parse(text=paste("crop_damage_", losstype, sep=""))), agcensus, by = c("Fips", "Year"), all=T)
  #merge previous merge with eqip
  merge2 <- merge(eqip, merge1, by = c("Fips", "Year"), all=T)
  #merge previous merge with ag landuse in cornbelt states
  merge3 <- merge(merge2, AGCENSUS_ac2, by = c("Fips"), all=T)
  merge3a <- merge(merge3, totalcropland, by = c("Fips"), all=T)
  #merge previous merge with eqip_csp
  merge3c <- merge(merge3a, eq_combined, by = c("Fips"))
  #merge previous merge with census
  merge4 <- merge(merge3c, census, by = c("Fips"), all=T)
  #merge previous merge with nri
  merge5 <- merge(merge4, nri, by = c("Fips"), all=T)
  #merge previous merge with commodity loss totals FOR ALL COMMODITIES
  merge6 <- merge(merge5, commodity_loss_total, by = c("Fips"), all=T)
  #merge previous merge with PDSI
  merge6a <- merge(merge6, pdsi, by = c("Fips"), all=T)
  #merge previous merge with cash rent
  merge6b <- merge(merge6a, cash_rent, by = c("Fips"), all=T)
  #merge previous merge with cropland diversity for 2011 and 2012
  merge6c <- merge(merge6b, cdl_div2, by = c("Fips"), all=T)
  #merge with previous merge for rented land revised
  merge7 <- merge(merge6c, rented_land_revised,  by = c("Fips"), all=T)
  #merge previous merge with extreme precip for 2008-2012
  merge7a <- merge(merge7, precip_extreme, by = c("Fips"), all=T)
  merge7b <- merge(merge7a, HDI, by=c("Fips"), all=T)
  merge7c <- merge(merge7b, racediversity, by=c("Fips"), all=T)
  merge7d <- merge(merge7c, femaleop, by=c("Fips"), all=T)
  merge7e <- merge(merge7d, FARMCOSTS, by=c("Fips"), all=T)
  merge7f <- merge(merge7e, RMA_revised, by=c("Fips"), all=T)
  merge7f1 <- merge(merge7f, soils, by=c("Fips"), all=T)
  merge7f2 <- merge(merge7f1, agcensus_table1, by=c("Fips"), all=T)
  merge7f3 <- merge(merge7f2, agcensus_table2, by=c("Fips"), all=T)
  
  
  
  merge7f <- subset(merge7f3, Year.x == "2012")
  
  
  merge7f <- merge(merge7f, countyFIPS, by=c("Fips"))
  
  merge7f <- subset(merge7f, State.y == "Illinois" | State.y == "Indiana" | State.y == "Iowa" | 
           State.y == "Michigan" | State.y == "Minnesota" | State.y == "Missouri" | 
           State.y == "Ohio" | State.y == "Wisconsin" | State.y =="Nebraska" | State.y == "Kansas" 
         | State.y == "North Dakota" | State.y == "South Dakota")
  
  #removes the FIPS column
  #merge7 <- subset( merge7e, select = -FIPS )

  #converts NA to zero
  #merge7f[is.na(merge7f)] <- 0 
  
  merge7 <- merge7f
  
#removing a bunch of extraneous columns that are mostly ID, and duplicate state and county and year columns
  
  merge7 <- merge7[, -c(92,97,108,109,110,205,213,228,229,230,236,237,238,339)]
  
  
 # merge7 <- merge7[-92]
#  merge7 <- merge7[-96]
#  merge7 <- merge7[-106]
#  merge7 <- merge7[-106]
#  merge7 <- merge7[-106]
#  merge7 <- merge7[-200]
#  merge7 <- merge7[-207]
#  merge7 <- merge7[-221]
#  merge7 <- merge7[-221]
#  merge7 <- merge7[-221]
#  merge7 <- merge7[-226] 
#  merge7 <- merge7[-226] 
#  merge7 <- merge7[-226]
#  merge7 <- merge7[-308]
  
 
  
  
  colnames(merge7)[326] <- "State"
  colnames(merge7)[327] <- "County"
  
  
  colnames(merge7)[2] <- "Year"
  
  #this adds a prefix to a set of datasets.  Make sure to change the column strings if you
  #happen to add other datasets.
  
  colnames(merge7)[3:32] <- paste("EQIP_", colnames(merge7)[3:32], sep = "")
  colnames(merge7)[33:66] <- paste("RMA_", colnames(merge7)[33:66], sep = "")
  colnames(merge7)[67:92] <- paste("AGCENSUS_", colnames(merge7)[67:92], sep = "")
  colnames(merge7)[93:95] <- paste("NRCS_", colnames(merge7)[93:95], sep = "")
  
  colnames(merge7)[96:105] <- paste("CENSUS_", colnames(merge7)[96:105], sep = "")
  colnames(merge7)[106:199] <- paste("NRI_", colnames(merge7)[106:199], sep = "")
  colnames(merge7)[221:224] <- paste("HDI_", colnames(merge7)[221:224], sep = "")
  colnames(merge7)[226:228] <- paste("FEMALE_", colnames(merge7)[226:228], sep = "")
  colnames(merge7)[229:294] <- paste("FARMCOSTS_", colnames(merge7)[229:294], sep = "")
  colnames(merge7)[295:301] <- paste("RMA_revised_", colnames(merge7)[295:301], sep = "")
  colnames(merge7)[302:307] <- paste("SOILS_", colnames(merge7)[302:307], sep = "")
  colnames(merge7)[308:313] <- paste("PAYMENTS_", colnames(merge7)[308:313], sep = "")
  colnames(merge7)[314:325] <- paste("FARMVALUE_", colnames(merge7)[314:325], sep = "")
  
  
  
  #--transformed variables
  
  #dependent variable 1
  #cover crops / total cropland
  
  merge7$AGCENSUS_Cover_Acres_Ratio <- merge7$AGCENSUS_Cover_Acres / merge7$AGCENSUS_Cropland_Acres
  merge7$AGCENSUS_Notill_Ratio <- merge7$AGCENSUS_Notill_Acres / merge7$AGCENSUS_Cropland_Acres
  merge7$AGCENSUS_Multitill_Ratio <- (merge7$AGCENSUS_Notill_Acres + merge7$AGCENSUS_Conservationtill_Acres) / merge7$AGCENSUS_Cropland_Acres
  
  merge7 <- arrange.vars(merge7, c("State"=3, "County"=4))
  
  
  #create name of crops used for RMA - example - for Midwest, we are using corn and soybeans
  croplist_name <- paste(croplist[[1]], "_", croplist[[2]], sep="")
  
  library( taRifx )
  #make sure all independent variables are numeric and state and county are factors
  merge8 <- japply( merge7[,5:330], which(sapply(merge7[,5:330], class)=="factor"), as.numeric )
  merge9 <- japply( merge8, which(sapply(merge8, class)=="integer"), as.numeric )
  
  #puts independent variables and factors (year, state, county, back together)
  merge9 <- cbind(merge7$Fips, merge7$State, merge7$County, merge7$Year, merge9)
  
  #makes sure state county year are named correctly and factored
  colnames(merge9)[1:4] <- c("Fips", "State", "County", "Year")
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
  merge9a <- merge9

  
  
  scaled_merge10 <- merge9[, -c(1:4)] <- scale(merge9[, -c(1:4)], center = TRUE, scale = TRUE)
  scaled_merge10 <- cbind(merge9$Fips, merge9$State, merge9$County, merge9$Year, scaled_merge10)
  colnames(scaled_merge10)[1:4] <- c("Fips", "State", "County", "Year")
  
  #write the combined file to the model_data location for SEM
  #for both scaled and non-scaled.  TWO files are generated below.
  
  write.csv(merge9a, file = paste("/nfs/soilsesfeedback-data/Model_data/MIDWEST_", croplist_name, "_Model2", "_nonscaled_new.csv", sep=""))
  write.csv(scaled_merge10, file = paste("/nfs/soilsesfeedback-data/Model_data/MIDWEST_", croplist_name, "_Model2", "_scaled_new.csv", sep=""))
  

  