#SEM Measurement Model
#Need to construct a SEM Measurement Model in our first phase of SEM development
#A Measurement model examines the relationships of manifest (observed) variables to their latent variables.
#we "Saturate" the SEM model intially in order to 

#library(shiny)
#library(shinyAce)
library(psych)
library(lavaan)
library(semPlot)
#library(shinyjs)






data1 <- read.csv("/nfs/soilsesfeedback-data/Model_data/MIDWEST_CORN_SOYBEANS_Model_acres.csv")
data1 <- data1[,-87] #remove FIPS
data1 <- data1[,-1] #remove ID

data1_nonscaled <- read.csv("/nfs/soilsesfeedback-data/Model_data/MIDWEST_CORN_SOYBEANS_Model_acres_nonscaled.csv")
data1_nonscaled <- data1_nonscaled[,-87] #remove FIPS
data1_nonscaled <- data1_nonscaled[,-1] #remove ID


data2 <- data1[,4:185]
data2 <- data.frame(data2)

data2_nonscaled <- data1_nonscaled[,4:185]
data2_nonscaled <- data.frame(data2_nonscaled)

chisq.test(data2_nonscaled) 



myModel <- '
# measurement model

#Latent Variables
#ex: Weather is measured by PDSI_Totals + RMA_Acres

WEATHER =~ PDSI_TOTALS + RMA_Acres
DIVERSITY =~ notill_farms + constill_farms
Management =~ Residue.and.Tillage.Management..No.Till + Residue.and.Tillage.Management..Reduced.Till
EQIP =~ Conservation.Crop.Rotation + Cover.Crop 

GENDER =~ OPERATORS...ALL...FEMALE...NUMBER.OF.OPERATORS + OPERATORS...ALL....NUMBER.OF.OPERATORS
RACE =~ OPERATORS..AMERICAN.INDIAN.OR.ALASKA.NATIVE...NUMBER.OF.OPERATORS + OPERATORS..ASIAN...NUMBER.OF.OPERATORS + 
OPERATORS..BLACK.OR.AFRICAN.AMERICAN...NUMBER.OF.OPERATORS + OPERATORS..HISPANIC...NUMBER.OF.OPERATORS + OPERATORS..MULTI.RACE...NUMBER.OF.OPERATORS +
OPERATORS..NATIVE.HAWAIIAN.OR.OTHER.PACIFIC.ISLANDER...NUMBER.OF.OPERATORS + OPERATORS..WHITE...NUMBER.OF.OPERATORS

# regressions

WEATHER ~ Diversity + Management
AgCensus ~ Management + Weather + Race + Gender + Diversity
AgCensus <~ Weather
# residual correlations
notill_farms ~~ constill_farms
Drought ~~ Heat
DIVERSITY ~~ RMA_Count
Conservation.Crop.Rotation ~~ Cover.Crop 
AGCENSUS_CC_Cropland_Acres_Ratio ~ Diversity + Gender + Race + Weather + RMA_Count'
#y3 ~~ y7
#y4 ~~ y8
#y6 ~~ y8


#fit <- cfa(model = myModel, data = data, missing = "fiml")
#summary(fit, fit.measures = TRUE)

fit <- sem(model = myModel, data=data1, std.lv =TRUE)

#summary(fit, standardized=TRUE)

semPaths(fit, "std", style = "mx", label.cex = 1.5, nCharNodes = 10, what = "cons", layout="tree", curvePivot = TRUE, edge.label.cex=.7)



