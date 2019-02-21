#SEM BETA
#Need to construct a SEM Measurement Model in our first phase of SEM development
#A Measurement model examines the relationships of manifest (observed) variables to their latent variables.
#we "Saturate" the SEM model intially in order to 

#library(shiny)
#library(shinyAce)
library(psych)
library(lavaan)
library(semPlot)
#library(shinyjs)

#scaled data
data2 <- read.csv("/nfs/soilsesfeedback-data/Model_data/MIDWEST_CORN_SOYBEANS_Model_acres_scaled.csv")
#data2 <- data1[,-87] #remove FIPS
data2 <- data2[,-1] #remove ID

#non-scaled data
data1 <- read.csv("/nfs/soilsesfeedback-data/Model_data/MIDWEST_CORN_SOYBEANS_Model_acres_nonscaled.csv")
data1 <- data1[,-88] #remove FIPS
data1 <- data1[,-1] #remove ID


SHEAFModel <- '

#Latent Variables
#ex: Weather is measured by PDSI_Totals + RMA_Acres

WEATHER =~ PDSI_TOTALS + RMA_Acres
DIVERSITY =~ RENT_Total + CDI_2012 + CDI_2011

#Management =~ Residue.and.Tillage.Management..No.Till + Residue.and.Tillage.Management..Reduced.Till
#EQIP =~ Conservation.Crop.Rotation + Cover.Crop 

GENDER =~ CENSUS_OPERATORS...ALL...FEMALE...NUMBER.OF.OPERATORS + CENSUS_OPERATORS...ALL....NUMBER.OF.OPERATORS
RACE =~ CENSUS_OPERATORS..AMERICAN.INDIAN.OR.ALASKA.NATIVE...NUMBER.OF.OPERATORS + CENSUS_OPERATORS..ASIAN...NUMBER.OF.OPERATORS + 
CENSUS_OPERATORS..BLACK.OR.AFRICAN.AMERICAN...NUMBER.OF.OPERATORS + CENSUS_OPERATORS..HISPANIC...NUMBER.OF.OPERATORS + CENSUS_OPERATORS..MULTI.RACE...NUMBER.OF.OPERATORS +
CENSUS_OPERATORS..NATIVE.HAWAIIAN.OR.OTHER.PACIFIC.ISLANDER...NUMBER.OF.OPERATORS + CENSUS_OPERATORS..WHITE...NUMBER.OF.OPERATORS

# regressions

# residual correlations
WEATHER ~~ RMA_Count
DIVERSITY ~~ RENT_Total
AGCENSUS_CC_Cropland_Acres_Ratio ~ DIVERSITY + GENDER + RACE + WEATHER + RMA_Count + RENT_Total'
#y3 ~~ y7
#y4 ~~ y8
#y6 ~~ y8


SHEAFModel_CFA <- ' WEATHER =~ PDSI_TOTALS + RMA_Acres
                  DIVERSITY =~ RENT_Total + CDI_2012 + CDI_2011
                  GENDER =~ CENSUS_OPERATORS...ALL...FEMALE...NUMBER.OF.OPERATORS + CENSUS_OPERATORS...ALL....NUMBER.OF.OPERATORS
                  RACE =~ CENSUS_OPERATORS..AMERICAN.INDIAN.OR.ALASKA.NATIVE...NUMBER.OF.OPERATORS + CENSUS_OPERATORS..ASIAN...NUMBER.OF.OPERATORS + 
CENSUS_OPERATORS..BLACK.OR.AFRICAN.AMERICAN...NUMBER.OF.OPERATORS + CENSUS_OPERATORS..HISPANIC...NUMBER.OF.OPERATORS + CENSUS_OPERATORS..MULTI.RACE...NUMBER.OF.OPERATORS +
CENSUS_OPERATORS..NATIVE.HAWAIIAN.OR.OTHER.PACIFIC.ISLANDER...NUMBER.OF.OPERATORS + CENSUS_OPERATORS..WHITE...NUMBER.OF.OPERATORS '


#CFA

fit <- lavaan::cfa(SHEAFModel_CFA, data=data2)
summary(fit, fit.measures = TRUE)

#fiml = full information maximum liklihood
#fit <- cfa(model = myModel, data = data, missing = "fiml")
#summary(fit, fit.measures = TRUE)

#SEM

fit <- lavaan::sem(model = SHEAFModel, data=data2, std.lv =TRUE)
summary(fit, fit.measures = TRUE)

summary(fit, standardized=TRUE)

semPlot::semPaths(fit, "cons", style = "mx", label.cex = 1.5, nCharNodes = 10, what = "std", layout="tree", curvePivot = TRUE, edge.label.cex=.7)



