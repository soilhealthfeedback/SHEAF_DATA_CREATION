EXPLORATORY DATA ANALYSIS AND DATA EXTRACTION
=======

The function below combines all datasets into one data frame.  datasets that have factors that are pertinent 
(farming practices, types of ag census categories), are transformed into columns.  In this case, there are some values that are NA 
for some factored columns, which means that, for that county/year/state combo, there was no value for that factor.

Running the function generates a dataset that is placed in the /soilsesfeedback-data/Model_data folder.  This folder ONLY contains datasets generated
using this function.  After generation, the dataset can then be called using the SHEAF_SEM model code, for analysis.  It can also be used in the 
SHEAF SEM web tool - which runs an SEM model on a selected dataset (http://soilhealthfeedback.org/sheaf-structural-equation-modeling-dashboard-alpha/)


**SHEAF_model_data_creation.R:**  this function generates a crop-specific model output, by combining and aggregating a fixed list of datasets.  
As we increase model functionality, we will increase the datasets merged and aggregated. The function is used as such:

    SHEAF_model_data_creation("WHEAT")
    
    For now, the modeled dataset is crop specific, but we can extend this to include all commodities.  In that case, we would need to delineate
    unique columns for each crop (e.g. WHEAT_Dollars.Paid, WHEAT_Drought, WHEAT_Vegetative Barrier).  This will result in very large number of variables, 
    but that may not be an issue if it is scientifically valid.
