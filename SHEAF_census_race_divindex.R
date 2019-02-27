library(pheatmap)
library(dplyr)
library(diverse)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

census <- read.csv("https://nextcloud.sesync.org/index.php/s/iEpDeymRJG6oKFr/download")

colnames(census)[4] <- "State"
colnames(census)[8] <- "County"
census$County <- as.character(census$County)
census$State <- tolower(census$State)
census$State <- sapply(census$State, simpleCap)
census$County <- tolower(census$County)
census$County <- sapply(census$County, simpleCap)

countyFIPS <- read.csv("https://nextcloud.sesync.org/index.php/s/wcFmKrSZW6Pr6D2/download")


countyFIPS$FIPS <- sprintf("%05d",countyFIPS$FIPS)
colnames(countyFIPS) <- c("ID", "State", "County", "FIPS")

census2 <- plyr::join(census, countyFIPS, by  = c("State", "County"))

census3 <- census2 %>%
  select(FIPS, Year, Data.Item, Value) 
census4 <- na.omit(census3) 
census5 <- census4[ which(census4$Data.Item == c("OPERATORS, AMERICAN INDIAN OR ALASKA NATIVE - NUMBER OF OPERATORS", 
                                                 "OPERATORS, BLACK OR AFRICAN AMERICAN - NUMBER OF OPERATORS",
                                                 "OPERATORS, HISPANIC - NUMBER OF OPERATORS",
                                                 "OPERATORS, MULTI-RACE - NUMBER OF OPERATORS",
                                                 "OPERATORS, ASIAN - NUMBER OF OPERATORS",
                                                 "OPERATORS, NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER - NUMBER OF OPERATORS",
                                                 "OPERATORS, WHITE - NUMBER OF OPERATORS")), ]



census2002 <- census5[ which(census5$Year=='2002'), ] #select for only 2002  
                                                     #eliminate rows with NAs in FIPS
census2002 <- census2002 %>%
  select(FIPS, Data.Item, Value)                     #reduce to 3 variables

census2007 <- census5[ which(census5$Year=='2007'), ] #select for only 2002  
#eliminate rows with NAs in FIPS
census2007 <- census2007 %>%
  select(FIPS, Data.Item, Value)                     #reduce to 3 variables

census2012 <- census5[ which(census5$Year=='2012'), ] #select for only 2002  
#eliminate rows with NAs in FIPS
census2012 <- census2012 %>%
  select(FIPS, Data.Item, Value)                     #reduce to 3 variables

div2002 <- diversity(census2002, type = "entropy")   #saves diversity measures as dataframe
div2007 <- diversity(census2007, type = "entropy")   #entropy = the Shannon entropy per entity measure, denoted by "H"
div2012 <- diversity(census2012, type = "entropy")

div2002$FIPS <- rownames(div2002)
div2007$FIPS <- rownames(div2007)
div2012$FIPS <- rownames(div2012)

div2002$year <- 2002
div2007$year <- 2007
div2012$year <- 2012

div2002_div2007 <- rbind(div2002, div2007, by  = "FIPS")
racediv <- rbind(div2002_div2007, div2012, by  = "FIPS")
head(racediv)

racediv[] <- lapply(racediv, function(x) as.numeric(as.character(x))) # convert to numberic variables
summary(racediv)

write.csv(racediv, file = paste("/nfs/soilsesfeedback-data/data/census/racediversity.csv", sep=""))

