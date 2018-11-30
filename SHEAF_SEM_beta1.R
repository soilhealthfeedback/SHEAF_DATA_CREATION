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


agcensus_download <- read.csv("https://nextcloud.sesync.org/index.php/s/THpGDspGXFtLSGF/download")

#removes ancillary columns at the end of the agcensus_download
agcensus_download <- agcensus_download[,1:25]

eqip <- read.csv("https://nextcloud.sesync.org/index.php/s/bgWSzqdqYDifJwz/download")

commodity <- read.csv("https://dmine.io/waf/USDA/USDA/crop_indemnity_originals_aggregated/commodities.csv")
xx_damage <- read.csv("/dmine/data/USDA/crop_indemnity_originals_aggregated/commodities_damagecause.csv")

#want to save the eqip data as an RDS file for faster usage?
#saveRDS(eqip, file = "Eqip.rds")


#load spatial county data

setwd("/dmine/data/counties/")

counties_conus <- readShapePoly('UScounties_conus.shp',
                                proj4string=CRS
                                ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

xx_eqip2 <- aggregate(xx_eqip$Dollars.Paid, by=list(xx_eqip$State, xx_eqip$County, xx_eqip$planned_year, xx_eqip$practice_name), FUN = "sum")
colnames(xx_eqip2) <- c("State", "County", "Year", "Practice_Name", "Dollars_Paid")
#xx_eqip3 <- subset(xx_eqip2, Practice_Name == "Residue Management, No-Till/Strip Till" & Planned_Year == "2010")

xx_eqip3a <- subset(xx_eqip2, Practice_Name %in% c("Residue Management, No-Till/Strip Till", "Conservation Cover") & Year == 2010)
xx_eqip3 <- aggregate(xx_eqip3a$Dollars_Paid, by = list(xx_eqip3a$State, xx_eqip3a$County), FUN = "sum")
colnames(xx_eqip3) <- c("State", "County", "Dollars_Paid")

#--need to deal with units ft vs acres
#eqip_ft <- subset(xx_eqip, units == "ft")
#eqip_ft$units



xx_eqip3$County <- tolower(xx_eqip3$County)
xx_eqip3$County <- capitalize(xx_eqip3$County)

colnames(xx_eqip3)[2] <- "NAME"
colnames(xx_eqip3)[1] <- "STATE_NAME"

m <- merge(counties_conus, xx_eqip3, by=c("STATE_NAME", "NAME"))

palz1 <- brewer.pal(9, "GnBu")

palz <- colorRampPalette(palz1)

m$Dollars_Paid[is.na(m$Dollars_Paid)] <- 0 
m$Dollars_Paid <- as.numeric(m$Dollars_Paid)


palData <- classIntervals(eval(parse(text=paste("m$", "Dollars_Paid", sep=""))), style="hclust")
colors <- findColours(palData, palz(100))


#pal <- colorNumeric(palette = c("white", "orange", "darkorange", "red", "darkred"),
#                    domain = eval(parse(text=paste("m$",input$agcensuscontrols , sep=""))))

pal2 <- colorNumeric(brewer.pal(9, "GnBu"), na.color = "#ffffff",
                     domain = eval(parse(text=paste("m$", "Dollars_Paid", sep=""))))



exte <- as.vector(extent(counties_conus))

label <- paste(sep = "<br/>", m$STATE_NAME, round(eval(parse(text=paste("m$", "Dollars_Paid", sep=""))), 0))
markers <- data.frame(label)
labs <- as.list(eval(parse(text=paste("m$", "Dollars_Paid", sep=""))))


leaflet(data = m) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal2(eval(parse(text=paste("m$", "Dollars_Paid", sep="")))), popup = markers$label,  weight = 1) %>%
  addLegend(pal = pal2, values = ~eval(parse(text=paste("m$", "Dollars_Paid", sep=""))), opacity = 1, title = NULL,
            position = "bottomright")




