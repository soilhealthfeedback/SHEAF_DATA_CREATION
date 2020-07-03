library("lavaan")
library("semPlot")
library("DescTools")
library("MASS")
library("car")

###nonscaled data###
soilnonscaled<-read.csv("C:\\Users\\kjone\\OneDrive\\Desktop\\Soil health team\\soilnonscaled.csv", header=T)
##no-till models
#Model 1 - beta conceptual model
model1<-'#latent variables
Water_Stress =~ PDSI_TOTALS + RMA_Droughtperacre + RMA_Failure_Irrig_Supplyperacre
Water_Surplus =~ PRECIP_max + RMA_Excess_Moistureperacre + RMA_Floodperacre + RMA_Hailperacre
Land_Use_Diversity =~ CDI_2012 + NRI_Irr_Cropland_Estimate + NRI_Prime_Pastureland_Estimate + NRI_Irr_Pastureland_Estimate+ NRI_Prime_Rangeland_Estimate +NRI_Prime_CRP_Estimate
Resource_Rich_County =~ NRCS_EQIP_FA_PAYMENTS_peracre + NRCS_CStP_FA_Payments_peracre + RMA_Lossperacre + RMA_Acresperclaim
High_Cost_County =~ RENT_average + +FEMALE_Total_farm_production_expense_farms_2012
#regressions
Land_Use_Diversity ~ Water_Stress + Water_Surplus + RENT_Percent.rented.2012
High_Cost_County ~ HDI_Education.Index +  HDI_Income.Index + HDI_Health.Index + RACE_Entropy
Resource_Rich_County ~ High_Cost_County + HDI_Education.Index + HDI_Income.Index + HDI_Health.Index + RENT_Percent.rented.2012 + RACE_Entropy
AGCENSUS_percent_notill ~ Resource_Rich_County + High_Cost_County+ Land_Use_Diversity + HDI_Health.Index + HDI_Income.Index + HDI_Education.Index + RACE_Entropy + FEMALE_percent_female + RENT_Percent.rented.2012 + Water_Stress + Water_Surplus  
#covariance
HDI_Health.Index ~~ HDI_Income.Index
HDI_Health.Index ~~ HDI_Education.Index
HDI_Health.Index ~~ RACE_Entropy
HDI_Income.Index ~~ HDI_Education.Index
HDI_Income.Index ~~ RACE_Entropy
HDI_Education.Index ~~ RACE_Entropy
High_Cost_County ~~ RENT_Percent.rented.2012
Water_Stress ~~ Resource_Rich_County
Water_Surplus ~~ Resource_Rich_County'

fitmodel1<-sem(model1,data=soilnonscaled)
summary(fitmodel1,standardized=T,fit.measures=T)
parameterestimates(fitmodel1)


#CFA for each factor
model2<-'Water_Stress =~ PDSI_TOTALS + RMA_Droughtperacre + RMA_Failure_Irrig_Supplyperacre'
fitmodel2<-cfa(model2, data=soilnonscaled)
summary(fitmodel2,fit.measures=T)

waterstress<-soilnonscaled[c("PDSI_TOTALS","RMA_Droughtperacre","RMA_Failure_Irrig_Supplyperacre")]
waterstresscor<-cor(waterstress)
waterstresscor

model3<-'Water_Surplus =~ PRECIP_max + RMA_Excess_Moistureperacre + RMA_Floodperacre + RMA_Hailperacre'
fitmodel3<-cfa(model3, data=soilnonscaled)
summary(fitmodel3,fit.measures=T)

watersurplus<-soilnonscaled[c("PRECIP_max","RMA_Excess_Moistureperacre","RMA_Floodperacre","RMA_Hailperacre")]
watersurpluscor<-cor(watersurplus)
watersurpluscor

model4<-'Land_Use_Diversity =~ CDI_2012 + NRI_Irr_Cropland_Estimate + NRI_Prime_Pastureland_Estimate + NRI_Irr_Pastureland_Estimate+ NRI_Prime_Rangeland_Estimate +NRI_Prime_CRP_Estimate'
fitmodel4<-cfa(model4, data=soilnonscaled)
summary(fitmodel4,fit.measures=T)

landusediv<-soilnonscaled[c("CDI_2012","NRI_Irr_Cropland_Estimate","NRI_Prime_Pastureland_Estimate","NRI_Irr_Pastureland_Estimate","NRI_Prime_Rangeland_Estimate","NRI_Prime_CRP_Estimate")]
landusedivcor<-cor(landusediv)
landusedivcor

model5<-'Resource_Rich_County =~ NRCS_EQIP_FA_PAYMENTS_peracre + NRCS_CStP_FA_Payments_peracre + RMA_Lossperacre + RMA_Acresperclaim'
fitmodel5<-cfa(model5, data=soilnonscaled)
summary(fitmodel5,fit.measures=T, standardized=T)

resourcerich<-soilnonscaled[c("NRCS_EQIP_FA_PAYMENTS_peracre","NRCS_CStP_FA_Payments_peracre","RMA_Lossperacre","RMA_Acresperclaim")]
resourcerichcor<-cor(resourcerich)
resourcerichcor

model6<-'High_Cost_County =~ RENT_average + FEMALE_Total_farm_production_expense_farms_2012 + HDI_Income.Index'
fitmodel6<-cfa(model6, data=soilnonscaled)
summary(fitmodel6,fit.measures=T, standardized=T)
soilnonscaled$RMA_L

#new model with one latent factor
model7<-'
#latent factors
High_Cost_County =~ RENT_average + +FEMALE_Total_farm_production_expense_farms_2012 + HDI_Income.Index
#regressions
CDI_2012 ~ PDSI_TOTALS + RMA_Droughtperacre + PRECIP_ave + RMA_Floodperacre + RENT_Percent.rented.2012
NRI_Prime_Pastureland_Estimate ~ PDSI_TOTALS + RMA_Droughtperacre + PRECIP_ave + RMA_Floodperacre + RENT_Percent.rented.2012
NRI_Prime_Rangeland_Estimate ~ PDSI_TOTALS + RMA_Droughtperacre + PRECIP_ave + RMA_Floodperacre + RENT_Percent.rented.2012
High_Cost_County ~ HDI_Education.Index + HDI_Health.Index + RACE_Entropy
NRCS_EQIP_cstp_combined_peracre ~ High_Cost_County + HDI_Education.Index + HDI_Health.Index + RENT_Percent.rented.2012 + RACE_Entropy
RMA_Lossperacre ~ High_Cost_County + HDI_Education.Index + HDI_Health.Index + RENT_Percent.rented.2012 + RACE_Entropy
AGCENSUS_percent_notill ~ NRCS_EQIP_cstp_combined_peracre + RMA_Lossperacre + High_Cost_County + CDI_2012 + NRI_Prime_Pastureland_Estimate + NRI_Prime_Rangeland_Estimate + HDI_Health.Index + HDI_Education.Index + RACE_Entropy + FEMALE_percent_female + RENT_Percent.rented.2012  
#covariance
HDI_Health.Index ~~ HDI_Education.Index
HDI_Health.Index ~~ RACE_Entropy
HDI_Education.Index ~~ RACE_Entropy
High_Cost_County ~~ RENT_Percent.rented.2012'

fitmodel7<-sem(model7,data=soilnonscaled)
summary(fitmodel7,standardized=T,fit.measures=T)
parameterestimates(fitmodel1)


model8<-'#latent factors
  High_Cost_County =~ RENT_average + FEMALE_Total_farm_production_expense_farms_2012 + HDI_Income.Index
#regressions

CDI_2012 ~ PDSI_TOTALS + RMA_Droughtperacre + PRECIP_ave + RMA_Floodperacre + RENT_Percent.rented.2012
High_Cost_County ~ HDI_Education.Index + HDI_Health.Index + RACE_Entropy
NRCS_EQIP_cstp_combined_peracre ~ PDSI_TOTALS + PRECIP_ave + HDI_Education.Index + HDI_Health.Index + RENT_Percent.rented.2012 + RACE_Entropy + NRI_Prime_Pastureland_Estimate + NRI_Prime_Rangeland_Estimate
RMA_Lossperacre ~ PDSI_TOTALS + PRECIP_ave + HDI_Education.Index + HDI_Health.Index + RENT_Percent.rented.2012 + RACE_Entropy + NRI_Prime_Pastureland_Estimate + NRI_Prime_Rangeland_Estimate

AGCENSUS_percent_notill ~ NRCS_EQIP_cstp_combined_peracre + RMA_Lossperacre + High_Cost_County + CDI_2012 + NRI_Prime_Pastureland_Estimate + NRI_Prime_Rangeland_Estimate + HDI_Health.Index + HDI_Education.Index + RACE_Entropy + FEMALE_percent_female + RENT_Percent.rented.2012  
#covariance
HDI_Health.Index ~~ HDI_Education.Index
HDI_Health.Index ~~ RACE_Entropy
HDI_Education.Index ~~ RACE_Entropy
High_Cost_County ~~ RENT_Percent.rented.2012
RMA_Lossperacre ~~ High_Cost_County
NRCS_EQIP_cstp_combined_peracre ~~ High_Cost_County'

fitmodel8<-sem(model8,data=soilnonscaled)
summary(fitmodel8,standardized=T,fit.measures=T)
parameterestimates(fitmodel8)


model9<-'#latent factors
High_Cost_County =~ RENT_average + FEMALE_Total_farm_production_expense_farms_2012 + HDI_Income.Index
#regressions
High_Cost_County ~ HDI_Education.Index + HDI_Health.Index + RACE_Entropy
CDI_2012 ~ PDSI_TOTALS + PRECIP_ave + RENT_Percent.rented.2012 + High_Cost_County
NRCS_EQIP_cstp_combined_peracre ~ PDSI_TOTALS + PRECIP_ave + HDI_Education.Index + HDI_Health.Index + RENT_Percent.rented.2012 + RACE_Entropy
RMA_Lossperacre ~ PDSI_TOTALS + PRECIP_ave

AGCENSUS_percent_notill ~ NRCS_EQIP_cstp_combined_peracre + RMA_Lossperacre + High_Cost_County + CDI_2012 + HDI_Health.Index + HDI_Education.Index + RACE_Entropy + FEMALE_percent_female + RENT_Percent.rented.2012  
#covariance
HDI_Health.Index ~~ HDI_Education.Index
HDI_Health.Index ~~ RACE_Entropy
HDI_Education.Index ~~ RACE_Entropy
High_Cost_County ~~ RENT_Percent.rented.2012'

fitmodel9<-sem(model9,data=soilnonscaled)
summary(fitmodel9,standardized=T,fit.measures=T)

conservation<-soilnonscaled[c("NRCS_EQIP_cstp_combined_peracre","AGCENSUS_percent_notill","AGCENSUS_percent_cc")]
conservationcor<-cor(conservation)
conservationcor
summary(soilnonscaled$NRCS_EQIP_cstp_combined_peracre)
skewness(soilnonscaled$AGCENSUS_percent_notill)

cor(soilnonscaled$RMA_Lossperacre,soilnonscaled$PDSI_TOTALS)

####scaled data###
soilscaled<-read.csv("C:\\Users\\kjone\\OneDrive\\Desktop\\Soil health team\\soilscaled.csv", header=T)
##no-till models
#beta conceptual model
model10<-'#latent variables
Water_Stress =~ PDSI_TOTALS + RMA_Drought + RMA_Failure_Irrig_Supply
Water_Surplus =~ PRECIP_max + RMA_Excess_Moisture + RMA_Flood + RMA_Hail
Land_Use_Diversity =~ CDI_2012 + NRI_Irr_Cropland_Estimate + NRI_Prime_Pastureland_Estimate + NRI_Irr_Pastureland_Estimate+ NRI_Prime_Rangeland_Estimate +NRI_Prime_CRP_Estimate
Resource_Rich_County =~ NRCS_EQIP_FA_PAYMENTS_total + NRCS_CStP_FA_Payments_total + RMA_Lossperacre + RMA_Acresperclaim
High_Cost_County =~ RENT_average + +FEMALE_Total_farm_production_expense_farms_2012
#regressions
Land_Use_Diversity ~ Water_Stress + Water_Surplus + RENT_Percent.rented.2012
High_Cost_County ~ HDI_Education.Index +  HDI_Income.Index + HDI_Health.Index + RACE_Entropy
Resource_Rich_County ~ High_Cost_County + HDI_Education.Index + HDI_Income.Index + HDI_Health.Index + RENT_Percent.rented.2012 + RACE_Entropy
AGCENSUS_notill_acres ~ Resource_Rich_County + High_Cost_County+ Land_Use_Diversity + HDI_Health.Index + HDI_Income.Index + HDI_Education.Index + RACE_Entropy + FEMALE_percent_female + RENT_Percent.rented.2012 + Water_Stress + Water_Surplus  
#covariance
HDI_Health.Index ~~ HDI_Income.Index
HDI_Health.Index ~~ HDI_Education.Index
HDI_Health.Index ~~ RACE_Entropy
HDI_Income.Index ~~ HDI_Education.Index
HDI_Income.Index ~~ RACE_Entropy
HDI_Education.Index ~~ RACE_Entropy
High_Cost_County ~~ RENT_Percent.rented.2012
Water_Stress ~~ Resource_Rich_County
Water_Surplus ~~ Resource_Rich_County'

fitmodel10<-sem(model10,data=soilscaled)
summary(fitmodel10,standardized=T,fit.measures=T)

#CFA with each factor
model11<-'Water_Stress =~ PDSI_TOTALS + RMA_Drought + RMA_Failure_Irrig_Supply'
fitmodel11<-cfa(model11, data=soilscaled)
summary(fitmodel11,fit.measures=T)

waterstressscaled<-soilscaled[c("PDSI_TOTALS","RMA_Drought","RMA_Failure_Irrig_Supply")]
waterstressscaledcor<-cor(waterstressscaled)
waterstressscaledcor

model12<-'Water_Surplus =~ PRECIP_max + RMA_Excess_Moisture + RMA_Flood + RMA_Hail'
fitmodel12<-cfa(model12, data=soilscaled)
summary(fitmodel12,fit.measures=T)

watersurplus<-soilnonscaled[c("PRECIP_max","RMA_Excess_Moistureperacre","RMA_Floodperacre","RMA_Hailperacre")]
watersurpluscor<-cor(watersurplus)
watersurpluscor

model13<-'Land_Use_Diversity =~ CDI_2012 + NRI_Irr_Cropland_Estimate + NRI_Prime_Pastureland_Estimate + NRI_Prime_Rangeland_Estimate +NRI_Prime_CRP_Estimate'
fitmodel13<-cfa(model13, data=soilscaled)
summary(fitmodel13,fit.measures=T)

landusedivscaled<-soilscaled[c("CDI_2012","NRI_Irr_Cropland_Estimate","NRI_Prime_Pastureland_Estimate","NRI_Prime_Rangeland_Estimate","NRI_Prime_CRP_Estimate")]
landusedivscaledcor<-cor(landusedivscaled)
landusedivscaledcor

model14<-'Resource_Rich_County =~ NRCS_EQIP_FA_PAYMENTS_total + NRCS_CStP_FA_Payments_total + RMA_Lossperacre + RMA_Acresperclaim'
fitmodel14<-cfa(model14, data=soilscaled)
summary(fitmodel14,fit.measures=T, standardized=T)

resourcerichscaled<-soilscaled[c("NRCS_EQIP_FA_PAYMENTS_total","NRCS_CStP_FA_Payments_total","RMA_Lossperacre","RMA_Acresperclaim")]
resourcerichscaledcor<-cor(resourcerichscaled)
resourcerichscaledcor

model15<-'High_Cost_County =~ RENT_average + FEMALE_Total_farm_production_expense_farms_2012 + HDI_Income.Index'
fitmodel15<-cfa(model15, data=soilscaled)
summary(fitmodel15,fit.measures=T, standardized=T)

##EFA with scaled data
#correlation matrix
exogcor<-soilscaled[c("PDSI_TOTALS","RMA_Drought","RMA_Failure_Irrig_Supply","PRECIP_max","RMA_Excess_Moisture","RMA_Flood","RMA_Hail",
                      "CDI_2012","NRI_Irr_Cropland_Estimate","NRI_Prime_Pastureland_Estimate","NRI_Prime_Rangeland_Estimate","NRI_Prime_CRP_Estimate",
                      "NRCS_EQIP_FA_PAYMENTS_total","NRCS_CStP_FA_Payments_total","RMA_Lossperacre","RMA_Acresperclaim",
                      "RENT_average","FEMALE_Total_farm_production_expense_farms_2012","HDI_Income.Index",
                      "HDI_Health.Index","HDI_Education.Index","RACE_Entropy","FEMALE_percent_female","RENT_Percent.rented.2012")]
soilscaledcor<-cor(exogcor)
soilscaledcor
skewness(exogcor)

#EFA
parallel <- fa.parallel(exogcor, fm = 'wls', fa = 'fa')
sevenfactor <- fa(exogcor,nfactors = 7,rotate = "oblimin",fm="wls")
print(sevenfactor)

#try five factors
fivefactor <- fa(efa3,nfactors = 5,rotate = "oblimin",fm="wls")
print(fivefactor)
print(fivefactor$loadings,cutoff = 0.3)
print(fourfactor$loadings,cutoff = 0.3)

#try six factors
sixfactor2 <- fa(efa3,nfactors = 6,rotate = "oblimin",fm="wls")
print(sixfactor2)
print(sixfactor2$loadings,cutoff = 0.3)

#try two factors
twofactor<-fa(efa3,nfactors = 2,rotate = "oblimin",fm="wls")
print(twofactor$loadings,cutoff = 0.3)

##New model with EFA factor
model16<-'SustainableInvestment=~CDI_2012 + RENT_average + RENT_Percent.rented.2012 +FEMALE_percent_female'
fitmodel16<-cfa(model16, data=soilscaled)
summary(fitmodel16,fit.measures=T, standardized=T)


model17<-'
#latent factors
SustainableInvestment=~CDI_2012 + RENT_average + RENT_Percent.rented.2012 + FEMALE_percent_female
#regressions
NRCS_EQIP_cstp_combined ~ PDSI_TOTALS + PRECIP_max + NRI_Irr_Cropland_Estimate + NRI_Prime_Pastureland_Estimate + NRI_Prime_Rangeland_Estimate
AGCENSUS_percent_notill ~ Commodification + NRCS_EQIP_cstp_combined + RMA_Lossperacre + FEMALE_Total_farm_production_expense_farms_2012 + HDI_Income.Index
#covariance
SustainableInvestment ~~ HDI_Income.Index'

fitmodel17<-sem(model17, data=soilscaled)
summary(fitmodel17,fit.measures=T, standardized=T)
semPaths(fitmodel17, what="std", whatLabels="std", layout="tree", intercepts=F, residuals=F, nCharNodes=0, filetype="R", label.color="black")

model18<-'
#latent factors
SustainableInvestment=~CDI_2012 + RENT_average + RENT_Percent.rented.2012 + FEMALE_percent_female
#regressions
NRCS_EQIP_cstp_combined ~ PDSI_TOTALS + PRECIP_max + NRI_Irr_Cropland_Estimate + NRI_Prime_Pastureland_Estimate + NRI_Prime_Rangeland_Estimate
AGCENSUS_percent_notill ~ SustainableInvestment + NRCS_EQIP_cstp_combined + FEMALE_Total_farm_production_expense_farms_2012 + HDI_Income.Index
#covariance
SustainableInvestment ~~ HDI_Income.Index'

fitmodel18<-sem(model18, data=soilscaled)
summary(fitmodel18,fit.measures=T, standardized=T)

model19<-'
#latent factors
SustainableInvestment=~CDI_2012 + RENT_average + RENT_Percent.rented.2012 + FEMALE_percent_female
#regressions
NRCS_EQIP_cstp_combined ~ NRI_Irr_Cropland_Estimate + NRI_Prime_Pastureland_Estimate + NRI_Prime_Rangeland_Estimate
AGCENSUS_percent_notill ~ SustainableInvestment + NRCS_EQIP_cstp_combined + FEMALE_Total_farm_production_expense_farms_2012 + HDI_Income.Index
#covariance
SustainableInvestment ~~ HDI_Income.Index'

fitmodel19<-sem(model19, data=soilscaled)
summary(fitmodel19,fit.measures=T, standardized=T)
semPaths(fitmodel19, what="std", whatLabels="std", layout="tree", intercepts=F, residuals=F, nCharNodes=0, filetype="R", label.color="black")

model20<-'
#latent factors
SustainableInvestment=~CDI_2012 + RENT_average + RENT_Percent.rented.2012 + FEMALE_percent_female
#regressions
NRCS_EQIP_cstp_combined ~ NRI_Irr_Cropland_Estimate + NRI_Prime_Pastureland_Estimate + NRI_Prime_Rangeland_Estimate
AGCENSUS_percent_notill ~ SustainableInvestment + NRCS_EQIP_cstp_combined + FEMALE_Total_farm_production_expense_farms_2012 + HDI_Income.Index + PDSI_TOTALS + PRECIP_max
#covariance
SustainableInvestment ~~ HDI_Income.Index'

fitmodel20<-sem(model20, data=soilscaled)
summary(fitmodel20,fit.measures=T, standardized=T)

#best fit no-till model
model21<-'
#latent factors
SustainableInvestment=~CDI_2012 + RENT_average + RENT_Percent.rented.2012 + FEMALE_percent_female
#regressions
NRCS_EQIP_cstp_combined ~ NRI_Irr_Cropland_Estimate + NRI_Prime_Rangeland_Estimate
AGCENSUS_percent_notill ~ SustainableInvestment + NRCS_EQIP_cstp_combined + FEMALE_Total_farm_production_expense_farms_2012 + HDI_Income.Index
#covariance
SustainableInvestment ~~ HDI_Income.Index'

fitmodel21<-sem(model21, data=soilscaled)
summary(fitmodel21,fit.measures=T, standardized=T)

#constill model
model22<-'#latent factors
SustainableInvestment=~CDI_2012 + RENT_average + RENT_Percent.rented.2012 + FEMALE_percent_female
#regressions
NRCS_EQIP_cstp_combined ~ NRI_Irr_Cropland_Estimate + NRI_Prime_Rangeland_Estimate
AGCENSUS_percent_constill ~ SustainableInvestment + NRCS_EQIP_cstp_combined + FEMALE_Total_farm_production_expense_farms_2012 + HDI_Income.Index
#covariance
SustainableInvestment ~~ HDI_Income.Index'

fitmodel22<-sem(model22, data=soilscaled)
summary(fitmodel22,fit.measures=T,standardized=T)

#noandconstill model
skewness(soilscaled$AGCENSUS_percent_cc)

model23<-'#latent factors
SustainableInvestment=~CDI_2012 + RENT_average + RENT_Percent.rented.2012 + FEMALE_percent_female
#regressions
NRCS_EQIP_cstp_combined ~ NRI_Irr_Cropland_Estimate + NRI_Prime_Rangeland_Estimate
AGCENSUS_percent_noandconstill ~ SustainableInvestment + NRCS_EQIP_cstp_combined + FEMALE_Total_farm_production_expense_farms_2012 + HDI_Income.Index
#covariance
SustainableInvestment ~~ HDI_Income.Index'

fitmodel23<-sem(model23, data=soilscaled)
summary(fitmodel23,fit.measures=T,standardized=T)

#print a model
semPaths(fitmodel21, what="std", whatLabels="std", layout="tree", intercepts=F, residuals=F, nCharNodes=0, filetype="R", label.color="black", edge.label.cex = 1, label.cex=5)

#Box Cox to deal with skewed cover crop data
soilscaled$AGCENSUS_percent_cc_transformed<-BoxCox(soilscaled$AGCENSUS_percent_cc, lambda=.33)
skewness(soilscaled$AGCENSUS_percent_cc_transformed)

model21<-'
#latent factors
SustainableInvestment=~CDI_2012 + RENT_average + RENT_Percent.rented.2012 + FEMALE_percent_female
#regressions
NRCS_EQIP_cstp_combined ~ NRI_Irr_Cropland_Estimate + NRI_Prime_Rangeland_Estimate
AGCENSUS_percent_notill ~ SustainableInvestment + NRCS_EQIP_cstp_combined_peracre + FEMALE_Total_farm_production_expense_farms_2012 + HDI_Income.Index
#covariance
SustainableInvestment ~~ HDI_Income.Index'

fitmodel21<-sem(model21, data=soilnonscaled)
summary(fitmodel21,fit.measures=T, standardized=T)
