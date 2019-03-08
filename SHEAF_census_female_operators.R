
library(dplyr)
library(tidyr)

## Rscript to transform Census data 
## Create percent female operators by extracting those rows for All operators, and Female operators
## Note: Don't need to merge with FIPS, just create FIPS from ANSI columns
## NB2: ISsues b/t county names for NASS vs FIPS file from SESYNC (!!!)
## Last updated 20190307 JJR




## Read census data and transform variables for pecent female operators

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


#convert ansi fields to integer for adding leading zeros
census$State.ANSI <- as.integer(census$State.ANSI)
census$County.ANSI <- as.integer(census$County.ANSI)

#add leading zeros to make state 2 digits and county 3 digits
census$State.ANSI <- sprintf("%02d",census$State.ANSI)
census$County.ANSI <- sprintf("%03d",census$County.ANSI)

#create a FIPS column
census$FIPS <- paste(census$State.ANSI, census$County.ANSI, sep="")



#countyFIPS <- read.csv("https://nextcloud.sesync.org/index.php/s/wcFmKrSZW6Pr6D2/download")


#countyFIPS$FIPS <- sprintf("%05d",countyFIPS$FIPS)
#colnames(countyFIPS) <- c("ID", "State", "County", "FIPS")



#census2 <- full_join(census %>% filter(!State %in% c("Alaska", "Hawaii")), countyFIPS, by  = c("State", "County"))

## check NA
colSums(is.na(census))




## jsut fips and year
census_female_operators <- census %>% 
  
  # clean value column
  
  mutate(value_trim=str_trim(Value)) %>%
  mutate(Value2=as.character(value_trim)) %>%
  ## clean up value column
  ## only select numbers in value column
  filter(Value2 != "(D)" & Value2 != "(Z)" & Value2 != "(NA)") %>%
  # remove commas from number values and convert to R numeric class
  mutate(Value2 = as.numeric(str_remove(Value2, ","))) %>%
  select(Year, State, County, FIPS, Data.Item, Value2) %>%
  
  # filter what you want
  filter(Data.Item %in% c("OPERATORS, (ALL) - NUMBER OF OPERATORS",  "OPERATORS, (ALL), FEMALE - NUMBER OF OPERATORS")) %>%
  spread(Data.Item, Value2) %>%
  rename(female_operators=`OPERATORS, (ALL), FEMALE - NUMBER OF OPERATORS`) %>%
  rename(all_operators=`OPERATORS, (ALL) - NUMBER OF OPERATORS`) %>%
  mutate(percent_female=as.integer(female_operators)/as.integer(all_operators))


write.csv(census_female_operators, file="census_percent_female_operators.csv", row.names=FALSE)



