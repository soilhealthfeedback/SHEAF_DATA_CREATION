#--SHEAF_eqip_map.R
#--loads some eqip dataset and merges, combines with spatial data for visualization
#--author: Erich Seamon, University of Idaho
#--date: October 2018
#
#-YEARS: 2004 thru 2018
#
# PRACTICES
#Forage and Biomass Planting                  Integrated Pest Management (IPM)            
#Residue Management, No-Till/Strip Till       Terrace                                     
#Prescribed Grazing                           Conservation Crop Rotation                  
#Grassed Waterway                             Residue Management, Seasonal                
#Residue Management, Mulch Till               Riparian Forest Buffer                      
#Filter Strip                                 Mulching                                    
#Cover Crop                                   Conservation Cover                          
#Windbreak/Shelterbelt Establishment          Hedgerow Planting                           
#Stripcropping                                Stripcropping, Field                        
#Riparian Herbaceous Cover                    Contour Buffer Strips                       
#Residue Management, Ridge Till               Transition to Organic Production            
#Long Term No. Till                           Riparian Buffers - Vegetative               
#Vegetative Barrier                           Residue and Tillage Management, No-Till     
#Contour Orchard and Other Perennial Crops    Alley Cropping                              
#Silvopasture Establishment                   Herbaceous Wind Barriers                    
#Residue and Tillage Management, Ridge Till   Residue and Tillage Management, Reduced Till
#Multi-Story Cropping                         Strip - Intercropping                       
#Restoration of Compacted Soils              
#
#USAGE: 
#  SHEAF_eqip_map(2014, "Vegetative Barrier")

SHEAF_eqip_map <- function(year,practice) {

library(rgdal)
library(leaflet)
library(maptools)
library(classInt)
library(leaflet)
library(dplyr)
library(RColorBrewer)
library(raster)


  
 simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


#EQIP---

#eqip load via url - best if you are NOT on SESYNC rstudio server
eqip <- read.csv("https://nextcloud.sesync.org/index.php/s/bgWSzqdqYDifJwz/download")

#eqip load using csv - use if you ARE on SESYNC Rstudio server
#setwd("/nfs/soilsesfeedback-data/data/eqip")
#eqip <- read.csv("eqip.csv")

#OR YOU MAY LOAD THE RDS FILE WHICH IS FASTER
#setwd("/nfs/soilsesfeedback-data/data/eqip")
#eqip <- readRDS("Eqip.rds")

#----

#SUPRESS WARNINGS FOR READSHAPEPOLY DEPRECATION---

oldw <- getOption("warn")
options(warn = -1)


#LOAD SPATIAL COUNTY DATA FOR THE ENTIRE US from URL

temp <- tempfile()
download.file("https://nextcloud.sesync.org/index.php/s/paxKXxFGnZaHbbN/download",temp)
outDir<-"/tmp"
unzip(temp,exdir=outDir)

counties_conus <- readShapePoly('/tmp/UScounties_conus.shp',
                                proj4string=CRS
                                ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

options(warn = oldw)

#---

#AGGREGATING EQIP - THIS CODE AGGREGATES EQIP DATA BY STATE, COUNTY, PLANNED YEAR, AND PRACTICE NAME---

eqip_aggregated <- aggregate(eqip$Dollars.Paid, by=list(eqip$State, eqip$County, eqip$planned_year, eqip$practice_name), FUN = "sum")
colnames(eqip_aggregated) <- c("State", "County", "Year", "Practice_Name", "Dollars_Paid")

#USING THE NEWLY AGGREGATED EQIP FILE, SUBSET BASED ON PRACTICE AND YEAR, AND THEN RE-AGGREGATE BY STATE AND COUNTY  
#THIS EXAMPLE SUBSETS FOR 2010, FOR RESIDUE MANAGEMENT, NO TILL/STRIP TILL, AND CONSERVATION COVER

eqip_practice <- subset(eqip_aggregated, Practice_Name %in% c(practice) & Year == year)
eqip_practice <- aggregate(eqip_practice$Dollars_Paid, by = list(eqip_practice$State, eqip_practice$County, eqip_practice$Year, eqip_practice$Practice_Name), FUN = "sum")
colnames(eqip_practice) <- c("State", "County", "Year", "Practice_Name", "Dollars_Paid")

#--need to deal with units ft vs acres
#eqip_ft <- subset(xx_eqip, units == "ft")
#eqip_ft$units

#----


#DEAL WITH ALIGNING COUNTY NAMES EXACTLY AS THEY ARE IN THE SPATIAL FILE.  ALL LOWER EXCEPT FOR FIRST LETTER---

eqip_practice$County <- tolower(eqip_practice$County)
eqip_practice$County <- sapply(eqip_practice$County, simpleCap)

#----

#RENAME STATE COLUMN TO MATCH WITH SPATIAL FILE---

colnames(eqip_practice)[2] <- "NAME"
colnames(eqip_practice)[1] <- "STATE_NAME"

#----

#NOW LETS MERGE THE FILES SO WE CAN PLOT MAPS AS NEEDED---

m <- merge(counties_conus, eqip_practice, by=c("STATE_NAME", "NAME"), duplicateGeoms = TRUE)

#----

#SET UP COLOR PALETTE---

palz1 <- brewer.pal(9, "GnBu")

palz <- colorRampPalette(palz1)

#----

#SET NA TO ZERO AND MAKE NUMERIC---

m$Dollars_Paid[is.na(m$Dollars_Paid)] <- 0 
m$Dollars_Paid <- as.numeric(m$Dollars_Paid)

#----


#SET UP THE INTERVALS FOR THE COLOR PALETTE USING A HIEARCHICAL CLUSTERING MECHANSIM TO DIVIDE THE VARIABLE THAT IS DISPLAYED---
m <- subset(m, Dollars_Paid != 0)
palData <- classIntervals(eval(parse(text=paste("m$", "Dollars_Paid", sep=""))), style="jenks")
colors <- findColours(palData, palz(100))

#----

#ASSIGN A COLOR USING THE PALETTE BY A RANGE---

pal2 <- colorNumeric(rev(brewer.pal(9, "Spectral")), na.color = "#ffffff",
                     domain = eval(parse(text=paste("m$", "Dollars_Paid", sep=""))))

#----


#SET THE EXTENT OF THE MAP---

exte <- as.vector(extent(counties_conus))

#----

#SET UP LABELING SO IF WE HOVER WE SEE INFORMATION---

label <- paste(sep = "<br/>", m$STATE_NAME, round(eval(parse(text=paste("m$", "Dollars_Paid", sep=""))), 0))
markers <- data.frame(label)
labs <- as.list(eval(parse(text=paste("m$", "Dollars_Paid", sep=""))))

#----

#NOW USE LEAFLET TO ACTUALLY DRAW THE MAP---

leaflet(data = m) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal2(eval(parse(text=paste("m$", "Dollars_Paid", sep="")))), popup = markers$label,  weight = 1) %>%
  addLegend(pal = pal2, values = ~eval(parse(text=paste("m$", "Dollars_Paid", sep=""))), opacity = 1, title = NULL,
            position = "bottomright")

#----

}

