#library(shiny)
#library(shinyAce)
library(psych)
library(lavaan)
library(semPlot)
#library(shinyjs)




data <- read.csv("/nfs/soilsesfeedback-data/Model_data/WHEAT_Model_dataset1.csv")

myModel <- '
# measurement model
Water_Availability =~ Drought + Heat
Tillage =~ notill_farms + constill_farms
Management =~ Residue.and.Tillage.Management..No.Till + Residue.and.Tillage.Management..Reduced.Till
Practices =~ Conservation.Crop.Rotation + Cover.Crop
# regressions
Water_Availability ~ Tillage + Management
Management ~ Practices + Water_Availability
# residual correlations
notill_farms ~~ constill_farms
Drought ~ Heat
Tillage ~~ Practices
Conservation.Crop.Rotation ~~ Cover.Crop'

#y3 ~~ y7
#y4 ~~ y8
#y6 ~~ y8


#fit <- cfa(model = myModel, data = data, missing = "fiml")
#summary(fit, fit.measures = TRUE)

fit <- sem(model = myModel, data=data)

#summary(fit, standardized=TRUE)

semPaths(fit,what="std",layout="circle")



