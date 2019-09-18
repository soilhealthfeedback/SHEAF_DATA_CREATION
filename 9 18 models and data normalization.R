#fit model with scaled data
soilscaled<-read.csv("C:\\Users\\kjone\\OneDrive\\Desktop\\Soil health team\\MIDWEST_CORN_SOYBEANS_Model2_scaled_new.csv", header=T)
soilnonscaled<-read.csv("C:\\Users\\kjone\\OneDrive\\Desktop\\Soil health team\\MIDWEST_CORN_SOYBEANS_Model2_nonscaled_new.csv", header=T)
#reduce skew in dependent variables
#cover crops
skewness(soilnonscaled$AGCENSUS_Cover_Acres_Ratio, na.rm=T)
summary(soilnonscaled$AGCENSUS_Cover_Acres_Ratio)
h<-hist(soilnonscaled$AGCENSUS_Cover_Acres_Ratio)
h
soilnonscaled$AGCENSUS_Cover_Acres_Ratio[soilnonscaled$AGCENSUS_Cover_Acres_Ratio>=1] <- NA
skewness(soilnonscaled$AGCENSUS_Cover_Acres_Ratio, na.rm=T)
summary(soilnonscaled$AGCENSUS_Cover_Acres_Ratio)
# to find optimal lambda
lambda = BoxCox.lambda(soilnonscaled$AGCENSUS_Cover_Acres_Ratio)
lambda
# now to transform vector
soilnonscaled$AGCENSUS_Cover_Acres_Ratio_transformed<-BoxCox(soilnonscaled$AGCENSUS_Cover_Acres_Ratio, lambda=-.1)
skewness(soilnonscaled$AGCENSUS_Cover_Acres_Ratio_transformed, na.rm=T)
summary(soilnonscaled$AGCENSUS_Cover_Acres_Ratio_transformed)
hist(soilscaled$AGCENSUS_Cover_Acres_Ratio_transformed)

#no till
skewness(soilnonscaled$AGCENSUS_Notill_Ratio, na.rm=T)
summary(soilnonscaled$AGCENSUS_Notill_Ratio)
h<-hist(soilnonscaled$AGCENSUS_Notill_Ratio)
h
soilnonscaled$AGCENSUS_Notill_Ratio[soilnonscaled$AGCENSUS_Notill_Ratio>=.5] <- NA
skewness(soilnonscaled$AGCENSUS_Notill_Ratio, na.rm=T)
summary(soilnonscaled$AGCENSUS_Notill_Ratio)
# to find optimal lambda
lambda = BoxCox.lambda(soilnonscaled$AGCENSUS_Notill_Ratio)
lambda
# now to transform vector
soilnonscaled$AGCENSUS_Notill_Ratio_transformed<-BoxCox(soilnonscaled$AGCENSUS_Notill_Ratio, lambda=-.1)
skewness(soilnonscaled$AGCENSUS_Notill_Ratio_transformed, na.rm=T)
summary(soilnonscaled$AGCENSUS_Notill_Ratio_transformed)
hist(soilscaled$AGCENSUS_Notill_Ratio_transformed)

#multitill
skewness(soilnonscaled$AGCENSUS_Multitill_Ratio, na.rm=T)
summary(soilnonscaled$AGCENSUS_Multitill_Ratio)
h<-hist(soilnonscaled$AGCENSUS_Multitill_Ratio)
h
soilnonscaled$AGCENSUS_Multitill_Ratio[soilnonscaled$AGCENSUS_Multitill_Ratio>=.8] <- NA
skewness(soilnonscaled$AGCENSUS_Multitill_Ratio, na.rm=T)
summary(soilnonscaled$AGCENSUS_Multitill_Ratio)
# to find optimal lambda
lambda = BoxCox.lambda(soilnonscaled$AGCENSUS_Multitill_Ratio)
lambda
# now to transform vector
soilnonscaled$AGCENSUS_Multitill_Ratio_transformed<-BoxCox(soilnonscaled$AGCENSUS_Multitill_Ratio, lambda=-.1)
skewness(soilnonscaled$AGCENSUS_Multitill_Ratio_transformed, na.rm=T)
summary(soilnonscaled$AGCENSUS_Multitill_Ratio_transformed)
hist(soilnonscaled$AGCENSUS_Multitill_Ratio_transformed)
hist(soilscaled$AGCENSUS_Multitill_Ratio_transformed)

#add new outcomes to soilscaled data
soilscaled$AGCENSUS_Cover_Acres_Ratio_transformed<-soilnonscaled$AGCENSUS_Cover_Acres_Ratio_transformed
soilscaled$AGCENSUS_Notill_Ratio_transformed<-soilnonscaled$AGCENSUS_Notill_Ratio_transformed
soilscaled$AGCENSUS_Multitill_Ratio_transformed<-soilnonscaled$AGCENSUS_Multitill_Ratio_transformed


#new model
model50<-'
#latent variables
Water_Stress=~PDSI_TOTALS + RMA_revised_loss_cost_hot_dry
Water_Surplus=~PRECIP_max + RMA_revised_loss_cost_wet
Gov_Subsidy_Dependence=~RMA_revised_total_indem_2012 + NRCS_EQIP_FA_PAYMENTS_total + NRCS_CStP_FA_Payments_total
Input_Costs=~FARMCOSTS_Chemicals_1000Doll_2012 + FARMCOSTS_Fert_lime_soilcond_purchased_1000dolls_2012 + FARMCOSTS_FeedExpenses_1000Dolls_2012 + FARMCOSTS_Property_taxes_paid_1000Dolls_2012
Land_Costs=~RENT_average + RENT_Percent.rented.2012 + FARMCOSTS_cashrent_land_building_pasture_1000Doll_2012 + FARMCOSTS_Interest_Expenses_1000Dolls_2012
Equipment_Services=~FARMCOSTS_gas_fuel_oil_purchased_1000Dolls_2012 + FARMCOSTS_Utilities_expenses_1000Dolls_2012 + FARMCOSTS_Depreciation_expenses_1000Doll_2012 + FARMCOSTS_rent_lease_machinery_equipment_Expense_1000Dolls_2012 + FARMCOSTS_hired_labor_expenses_1000Dolls_2012 + FARMCOSTS_Customwork_hauling_expenses_1000Doll_2012 + FARMCOSTS_ContractLaborExpense_1000Doll_2012
Input_Dependence=~Input_Costs + Land_Costs + Equipment_Services
#regressions
CDI_2012~Water_Stress + Water_Surplus
Gov_Subsidy_Dependence~Input_Dependence
AGCENSUS_Notill_Ratio_transformed~CDI_2012 + Gov_Subsidy_Dependence + Water_Stress + Water_Surplus + HDI_Health.Index + HDI_Income.Index + HDI_Education.Index +
RACE_Entropy + FEMALE_percent_female + RMA_revised_loss_cost
#covariance
HDI_Health.Index ~~ HDI_Income.Index
HDI_Health.Index ~~ HDI_Education.Index
HDI_Income.Index ~~ HDI_Education.Index
Water_Stress ~~ Water_Surplus
RACE_Entropy ~~ FEMALE_percent_female
RMA_revised_total_indem_2012 ~~ RMA_revised_loss_cost
'
fitmodel50<-sem(model50, data=soilscaled)
summary(fitmodel50,fit.measures=T,standardized=T)

#CFA with each latent factor
#Water Stress with loss
model51<-'Water_Stress=~PDSI_TOTALS + RMA_revised_loss_cost_hot_dry'
fitmodel51<-cfa(model51, data=soilscaled)
summary(fitmodel51,fit.measures=T)

waterstressscaled<-soilscaled[c("PDSI_TOTALS","RMA_revised_loss_cost_hot_dry")]
waterstressscaledcor<-cor(waterstressscaled, use="complete.obs")
waterstressscaledcor

#Water stress with loss ratio
model52<-'Water_Stress=~PDSI_TOTALS + RMA_revised_mean_loss_ratio_hot_dry'
fitmodel52<-cfa(model52, data=soilscaled)
summary(fitmodel52,fit.measures=T)

waterstressscaled<-soilscaled[c("PDSI_TOTALS","RMA_revised_mean_loss_ratio_hot_dry")]
waterstressscaledcor<-cor(waterstressscaled, use="complete.obs")
waterstressscaledcor

#Water surplus with loss
model53<-'Water_Surplus=~PRECIP_max + RMA_revised_loss_cost_wet'
fitmodel53<-cfa(model53, data=soilscaled)
summary(fitmodel53,fit.measures=T)

watersurplusscaled<-soilscaled[c("PRECIP_max","RMA_revised_loss_cost_wet")]
watersurplusscaledcor<-cor(watersurplusscaled, use="complete.obs")
watersurplusscaledcor

#Water surplus with loss ratio
model54<-'Water_Surplus=~PRECIP_max + RMA_revised_mean_loss_ratio_wet'
fitmodel54<-cfa(model54, data=soilscaled)
summary(fitmodel54,fit.measures=T)

watersurplusscaled<-soilscaled[c("PRECIP_max","RMA_revised_mean_loss_ratio_wet")]
watersurplusscaledcor<-cor(watersurplusscaled, use="complete.obs")
watersurplusscaledcor

#Government Subsidy Dependence
model55<-'Gov_Subsidy_Dependence=~RMA_revised_total_indem_2012 + NRCS_EQIP_FA_PAYMENTS_total + NRCS_CStP_FA_Payments_total'
fitmodel55<-cfa(model55, data=soilscaled)
summary(fitmodel55,fit.measures=T)

govsubscaled<-soilscaled[c("RMA_revised_total_indem_2012","NRCS_EQIP_FA_PAYMENTS_total","NRCS_CStP_FA_Payments_total")]
govsubscaledcor<-cor(govsubscaled, use="complete.obs")
govsubscaledcor

#Input costs
model56<-'Input_Costs=~FARMCOSTS_Chemicals_1000Doll_2012 + FARMCOSTS_Fert_lime_soilcond_purchased_1000dolls_2012 + FARMCOSTS_FeedExpenses_1000Dolls_2012 + FARMCOSTS_Property_taxes_paid_1000Dolls_2012'
fitmodel56<-cfa(model56, data=soilscaled)
summary(fitmodel56,fit.measures=T)

#Land costs
model57<-'Land_Costs=~RENT_average + RENT_Percent.rented.2012 + FARMCOSTS_cashrent_land_building_pasture_1000Doll_2012 + FARMCOSTS_Interest_Expenses_1000Dolls_2012'
fitmodel57<-cfa(model57,data=soilscaled)
summary(fitmodel57,fit.measures=T)

landcostsscaled<-soilscaled[c("RENT_average","RENT_Percent.rented.2012","FARMCOSTS_cashrent_land_building_pasture_1000Doll_2012","FARMCOSTS_Interest_Expenses_1000Dolls_2012")]
landcostsscaledcor<-cor(landcostsscaled, use="complete.obs")
landcostsscaledcor

#modify land costs to remove percent rented land
model58<-'Land_Costs=~RENT_average + FARMCOSTS_cashrent_land_building_pasture_1000Doll_2012 + FARMCOSTS_Interest_Expenses_1000Dolls_2012'
fitmodel58<-cfa(model58,data=soilscaled)
summary(fitmodel58,fit.measures=T)

#Equipment services
model59<-'Equipment_Services=~FARMCOSTS_gas_fuel_oil_purchased_1000Dolls_2012 + FARMCOSTS_Utilities_expenses_1000Dolls_2012 + FARMCOSTS_Depreciation_expenses_1000Doll_2012 + FARMCOSTS_rent_lease_machinery_equipment_Expense_1000Dolls_2012 + FARMCOSTS_hired_labor_expenses_1000Dolls_2012 + FARMCOSTS_Customwork_hauling_expenses_1000Doll_2012 + FARMCOSTS_ContractLaborExpense_1000Doll_2012'
fitmodel59<-cfa(model59,data=soilscaled)
summary(fitmodel59,fit.measures=T)

#Input dependence
model60<-'
Input_Costs=~FARMCOSTS_Chemicals_1000Doll_2012 + FARMCOSTS_Fert_lime_soilcond_purchased_1000dolls_2012 + FARMCOSTS_FeedExpenses_1000Dolls_2012 + FARMCOSTS_Property_taxes_paid_1000Dolls_2012
Land_Costs=~RENT_average + FARMCOSTS_cashrent_land_building_pasture_1000Doll_2012 + FARMCOSTS_Interest_Expenses_1000Dolls_2012
Equipment_Services=~FARMCOSTS_gas_fuel_oil_purchased_1000Dolls_2012 + FARMCOSTS_Utilities_expenses_1000Dolls_2012 + FARMCOSTS_Depreciation_expenses_1000Doll_2012 + FARMCOSTS_rent_lease_machinery_equipment_Expense_1000Dolls_2012 + FARMCOSTS_hired_labor_expenses_1000Dolls_2012 + FARMCOSTS_Customwork_hauling_expenses_1000Doll_2012 + FARMCOSTS_ContractLaborExpense_1000Doll_2012
Input_Dependence=~Input_Costs + Land_Costs + Equipment_Services'
fitmodel60<-cfa(model60,data=soilscaled)
summary(fitmodel60,fit.measures=T)

##Water stress, water surplus and government subsidies don't hang together##
##Updated input costs, equipment costs and land costs - latent construct not great##

#new model
model61<-'
#latent variables
Input_Costs=~FARMCOSTS_Chemicals_1000Doll_2012 + FARMCOSTS_Fert_lime_soilcond_purchased_1000dolls_2012 + FARMCOSTS_FeedExpenses_1000Dolls_2012 + FARMCOSTS_Property_taxes_paid_1000Dolls_2012
Land_Costs=~RENT_average + FARMCOSTS_cashrent_land_building_pasture_1000Doll_2012 + FARMCOSTS_Interest_Expenses_1000Dolls_2012
Equipment_Services=~FARMCOSTS_gas_fuel_oil_purchased_1000Dolls_2012 + FARMCOSTS_Utilities_expenses_1000Dolls_2012 + FARMCOSTS_Depreciation_expenses_1000Doll_2012 + FARMCOSTS_rent_lease_machinery_equipment_Expense_1000Dolls_2012 + FARMCOSTS_hired_labor_expenses_1000Dolls_2012 + FARMCOSTS_Customwork_hauling_expenses_1000Doll_2012 + FARMCOSTS_ContractLaborExpense_1000Doll_2012

#regressions
CDI_2012~PDSI_TOTALS + RMA_revised_loss_cost_hot_dry + PRECIP_max + RMA_revised_loss_cost_wet

RMA_revised_total_indem_2012~Input_Costs + Land_Costs + Equipment_Services

NRCS_EQIP_cstp_combined~Input_Costs + Land_Costs + Equipment_Services

AGCENSUS_Cover_Acres_Ratio_transformed~CDI_2012 + NRCS_EQIP_cstp_combined + RMA_revised_total_indem_2012 + PDSI_TOTALS + RMA_revised_loss_cost_hot_dry + PRECIP_max + RMA_revised_loss_cost_wet + HDI_Health.Index + HDI_Income.Index + HDI_Education.Index +
RACE_Entropy + FEMALE_percent_female

#covariance
HDI_Health.Index ~~ HDI_Income.Index
HDI_Health.Index ~~ HDI_Education.Index
HDI_Income.Index ~~ HDI_Education.Index
RACE_Entropy ~~ FEMALE_percent_female
Input_Costs ~~ Land_Costs
Input_Costs ~~ Equipment_Services
Land_Costs ~~ Equipment_Services'

fitmodel61<-sem(model61,data=soilscaled)
summary(fitmodel61, fit.measures=T, standardized=T)
