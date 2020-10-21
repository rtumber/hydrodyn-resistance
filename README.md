# hydrodyn-resistance
## Edx Project Two - Predict Residual Resistance from measurements taken based on Hull design

### Summary
Data taken from yacht hydrodynamics dataset at https://archive.ics.uci.edu/ml/datasets/Yacht+Hydrodynamics
The goal was to predict residual resistance from various hull geometry coefficients and Froude number

Code writen using R 3.6.2, dataset and datasset explanation downloaded from source, imported to Rstudio and organised into a format that allowed for exploration and modelling.

Set split into training and test sets.
Assessed using RMSE, ensembles of both linear and non-linear regression methods used for modelling on training set. Factors used in modelling identified by visualisation and mathematical inference.
More successful modelling methods retained at early stage and futher factors added.
Final model used to predict values on testing set and performance assessed using RMSE.

File FullScript.R is the script used to download and process the data, create the model and assess the performance.
File hydrodyn_resistance_report.Rmd is R markdown for project report.
