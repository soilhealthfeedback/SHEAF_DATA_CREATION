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
data2 <- read.csv("/nfs/soilsesfeedback-data/Model_data/MIDWEST_CORN_SOYBEANS_Model2_lossperacre_scaled.csv")
#data2 <- data1[,-87] #remove FIPS
data2 <- data2[,-1] #remove ID

#transform dependent by log10
#data2$AGCENSUS_CC_Cropland_Acres_Ratio <- log10(data2$AGCENSUS_CC_Cropland_Acres_Ratio)
#data2$AGCENSUS_NOTILL_Cropland_Acres_Ratio <- log10(data2$AGCENSUS_notill_acres/data2$AGCENSUS_Cropland_Acres)

data2$RENT_Total <- data2$RENT_average * data2$RENT_acres.rented.2012
data2$RENT_Inverse_percent.rented.2012 <- 1-data2$RENT_acres.rented.2012

#non-scaled data
#data1 <- read.csv("/nfs/soilsesfeedback-data/Model_data/MIDWEST_CORN_SOYBEANS_Model_acres_nonscaled.csv")
#data1 <- data1[,-88] #remove FIPS
#data1 <- data1[,-1] #remove ID

hist(data2$AGCENSUS_NOTILL_Cropland_Acres_Ratio)
hist(data2$AGCENSUS_CC_Cropland_Acres_Ratio)

#normalizing dependent variables
shapiro.test(data2$AGCENSUS_CC_Cropland_Acres_Ratio)
shapiro.test(data2$AGCENSUS_NOTILL_Cropland_Acres_Ratio)

colnames(data2)[74] <- "CENSUS_FEMALE"
colnames(data2)[75] <- "CENSUS_ALL"
colnames(data2)[76] <- "CENSUS_INDIAN"
colnames(data2)[77] <- "CENSUS_ASIAN"
colnames(data2)[78] <- "CENSUS_BLACK"
colnames(data2)[79] <- "CENSUS_FEMALE2"
colnames(data2)[80] <- "CENSUS_HISPANIC"
colnames(data2)[81] <- "CENSUS_MULTIRACE"
colnames(data2)[82] <- "CENSUS_HAWAIIAN"
colnames(data2)[83] <- "CENSUS_WHITE"

#-model formulation

SHEAFModel_CFA <- ' WEATHER =~ PDSI_TOTALS + RMA_Acres
                  DIVERSITY =~ RENT_Total + CDI_2012 + CDI_2011
GENDER =~ CENSUS_FEMALE + CENSUS_ALL
RACE =~ CENSUS_INDIAN + CENSUS_ASIAN + 
CENSUS_BLACK + CENSUS_HISPANIC + CENSUS_MULTIRACE +
CENSUS_HAWAIIAN + CENSUS_WHITE '




SHEAFModel_cc <- '

#Latent Variables
#ex: Weather is measured by PDSI_Totals + RMA_Acres

WEATHER =~ PDSI_TOTALS + RMA_Acres
DIVERSITY =~ RENT_Total + CDI_2012 + CDI_2011 + RENT_Inverse_percent.rented.2012



#Management =~ Residue.and.Tillage.Management..No.Till + Residue.and.Tillage.Management..Reduced.Till
#EQIP =~ Conservation.Crop.Rotation + Cover.Crop 

GENDER =~ CENSUS_FEMALE + CENSUS_ALL
RACE =~ CENSUS_INDIAN + CENSUS_ASIAN + 
CENSUS_BLACK + CENSUS_HISPANIC + CENSUS_MULTIRACE +
CENSUS_HAWAIIAN + CENSUS_WHITE 

# regressions

# residual correlations
WEATHER ~~ RMA_Count
DIVERSITY ~~ RENT_Total
AGCENSUS_CC_Cropland_Acres_Ratio ~ DIVERSITY + GENDER + RACE + WEATHER + RMA_Count + RENT_Total'


SHEAFModel_NOTILL <- '

#Latent Variables
#ex: Weather is measured by PDSI_Totals + RMA_Acres

WEATHER =~ PDSI_TOTALS + RMA_Acres
DIVERSITY =~ RENT_Total + CDI_2012 + CDI_2011

#Management =~ Residue.and.Tillage.Management..No.Till + Residue.and.Tillage.Management..Reduced.Till
#EQIP =~ Conservation.Crop.Rotation + Cover.Crop 

GENDER =~ CENSUS_FEMALE + CENSUS_ALL
RACE =~ CENSUS_INDIAN + CENSUS_ASIAN + 
CENSUS_BLACK + CENSUS_HISPANIC + CENSUS_MULTIRACE +
CENSUS_HAWAIIAN + CENSUS_WHITE 

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

semPlot::semPaths(fit_cfa, "std",   bifactor = "g",  fade = FALSE, style = "lisrel", label.cex = 1.5, nCharNodes = 10, what = "std", layout="tree2", curvePivot = TRUE, edge.label.cex=.85)
summary(fit_cfa, fit.measures = TRUE)

semPlot::semPaths(fit_cc, "std",  bifactor = "g", fade = FALSE, style = "lisrel", label.cex = 1.5, nCharNodes = 10, what = "std", layout="tree3", curvePivot = TRUE, edge.label.cex=.85)
summary(fit_cc, fit.measures = TRUE)

semPlot::semPaths(fit_notill, bifactor = "g", "std",  fade = FALSE, style = "lisrel", label.cex = 1.5, nCharNodes = 10, what = "std", layout="tree3", curvePivot = TRUE, edge.label.cex=.85)
summary(fit_notill, fit.measures = TRUE)


# Compare models:
layout(t(1:2))
semPaths(fit_notill, "std", title = FALSE)
title("No till", line = 3)
semPaths(fit_cc, "std", title = FALSE)
title("Cover crop", line = 3)

#---fit indices and standardized summaries
#change fit to examine differing models


