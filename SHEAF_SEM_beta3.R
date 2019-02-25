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

#transform dependent by log10
data2$AGCENSUS_CC_Cropland_Acres_Ratio <- log10(data2$AGCENSUS_CC_Cropland_Acres_Ratio)
data2$AGCENSUS_NOTILL_Cropland_Acres_Ratio <- log10(data2$AGCENSUS_notill_acres/data2$AGCENSUS_Cropland_Acres)

#non-scaled data
data1 <- read.csv("/nfs/soilsesfeedback-data/Model_data/MIDWEST_CORN_SOYBEANS_Model_acres_nonscaled.csv")
data1 <- data1[,-88] #remove FIPS
data1 <- data1[,-1] #remove ID


shapiro.test(data2$AGCENSUS_CC_Cropland_Acres_Ratio)
shapiro.test(data2$AGCENSUS_NOTILL_Cropland_Acres_Ratio)


#-model formulation

SHEAFModel_CFA <- ' WEATHER =~ PDSI_TOTALS + RMA_Acres
                  DIVERSITY =~ RENT_Total + CDI_2012 + CDI_2011
GENDER =~ CENSUS_OPERATORS...ALL...FEMALE...NUMBER.OF.OPERATORS + CENSUS_OPERATORS...ALL....NUMBER.OF.OPERATORS
RACE =~ CENSUS_OPERATORS..AMERICAN.INDIAN.OR.ALASKA.NATIVE...NUMBER.OF.OPERATORS + CENSUS_OPERATORS..ASIAN...NUMBER.OF.OPERATORS + 
CENSUS_OPERATORS..BLACK.OR.AFRICAN.AMERICAN...NUMBER.OF.OPERATORS + CENSUS_OPERATORS..HISPANIC...NUMBER.OF.OPERATORS + CENSUS_OPERATORS..MULTI.RACE...NUMBER.OF.OPERATORS +
CENSUS_OPERATORS..NATIVE.HAWAIIAN.OR.OTHER.PACIFIC.ISLANDER...NUMBER.OF.OPERATORS + CENSUS_OPERATORS..WHITE...NUMBER.OF.OPERATORS '




SHEAFModel_cc <- '

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

SHEAFModel_NOTILL <- '

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
AGCENSUS_NOTILL_Cropland_Acres_Ratio ~ DIVERSITY + GENDER + RACE + WEATHER + RMA_Count + RENT_Total'
#y3 ~~ y7
#y4 ~~ y8
#y6 ~~ y8




#CFA

fit_cfa <- lavaan::cfa(SHEAFModel_CFA, data=data2)
fit_notill <- lavaan::sem(model = SHEAFModel_NOTILL, data=data2, std.lv =TRUE)
fit_cc <- lavaan::sem(model = SHEAFModel_cc, data=data2, std.lv =TRUE)




#standardized estimates
semPlot::semPaths(fit, "std",  bifactor = "g", fade = FALSE, style = "lisrel", label.cex = 1.5, nCharNodes = 10, what = "std", layout="tree3", curvePivot = TRUE, edge.label.cex=.85)

semPlot::semPaths(fit_cfa, "std",   bifactor = "g",  fade = FALSE, style = "lisrel", label.cex = 1.5, nCharNodes = 10, what = "std", layout="tree2", curvePivot = TRUE, edge.label.cex=.85)

#standardized estimates
semPlot::semPaths(fit, "std", style = "mx", label.cex = 1.5, nCharNodes = 10, what = "std", layout="tree", curvePivot = TRUE, edge.label.cex=.7, mar = c(3, 1, 5, 1))

semPaths(fit, "eq", as.expression = "edges", mar = c(3, 1, 5, 1))


# Compare models:
layout(t(1:2))
semPaths(fit_notill, "std", title = FALSE)
title("Lavaan model 1", line = 3)
semPaths(fit_cc, "std", title = FALSE)
title("Lavaan model 2", line = 3)

#---fit indices and standardized summaries
#change fit to examine differing models

summary(fit, fit.measures = TRUE)
summary(fit, standardized=TRUE)

