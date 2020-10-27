if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")

library(data.table)
library(tidyverse)
library(rvest)
library(dplyr)
library(caret)
library(Rborist)
library(randomForest)
library(e1071)


#Data Import

#datafile address https://archive.ics.uci.edu/ml/machine-learning-databases/00243/yacht_hydrodynamics.data

data_file <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00243/yacht_hydrodynamics.data", data_file)

import_data <- fread(data_file)

#Data has imported but what are the column names

html <- read_html("https://archive.ics.uci.edu/ml/datasets/Yacht+Hydrodynamics")
html_text(html)

#Brief glance through the data using the below to separate the html nodes to individual lines, this allows us to 
#locate the information we require in relation to other html nodes.

str_split(html,">")

#This indicates the column headings and other potentially useful data are located in the second table in the
#document.
#These data are extracted by first isolating the table containing the data
tabs <- html %>% html_nodes("table")
tab2 <- tabs[[2]]

doc_text <- html_text(tab2)
#Looking at the text, we can split by \n to begin separating useful data
split_doc <- as.data.table(str_split(doc_text, "\n"))
#Examination indicates first bit of useful data at position 5
split_doc <- split_doc[5:(nrow(split_doc))]
#Following this another look shows what we may be looking for stops at row 35
split_doc <- split_doc[1:35]

#After several miscellaneous edits combined with examining the results as we go results in the a data table of
#useful information in a friendly format
split_doc <- split_doc[-2:-3]
split_doc <- split_doc[-3:-22]
split_doc <- split_doc[c(-5,-8)]
split_doc <- split_doc[-9]
#(Above edits could have gone in as one line but was working stepwise to avoid removing anything that needed to 
#be retained)

print(split_doc)
#Attribute information required for labelling the data in row 8, this can be split by \r and then a regex to remove
#the number.
att_inf <- str_split(split_doc[8], "\r", simplify = TRUE)
print(att_inf)
att_inf <- str_replace(att_inf, "\\d. ", "")
#Entries 1, 8 are not required for column headers and 10 is blank so are removed after 1 and 8 are stored elsewhere
att_1_6 <- att_inf[1]
att_7 <- att_inf[8]
att_inf <- att_inf[c(-1, -8, -10)]

#The remaining values are assigned as the import_data column names
colnames(import_data) <- att_inf

#Data prepared
str(import_data[1:3,])
#All 7 variables are numeric
#According to the data downloaded from the UCI Machine Learning Repository, variables one to six are described by 
#the extracted text stored in att_1_6.
print(att_1_6)
#They consist of hull geometry coefficients and the Froude number
#Variable seven is described by att_7
print(att_7)
#This variable is the residual resistance per unit weight of displacement. We will attempt to predict this variable

#Variables 1-6 are now investigated to assess their use in predicting variable 7. For clarity, the columns are first
#given user-friendly names, with these names being added to att_inf so it can serve as a key
att_inf <- as.data.table(att_inf) %>%
  mutate(short_names = c("lcb", "cp", "dlr", "bt", "lb", "fn", "resid_reswei"))
colnames(import_data) <- c("lcb", "cp", "dlr", "bt", "lb", "fn", "resid_reswei")


#Data Exploration

#Data table structure
str(import_data)
#To begin we look at the mean & standard deviation of the factor we want to predict
import_data %>% summarize(mean = mean(resid_reswei), std_dev = sd(resid_reswei))
#The variation in the values indicates either there are one or more factors in the dataset that influence 
#the residual resistance by weight, the factors that influence the residual resistance are not in the dataset or
#that the values are random.
#By looking at the other factors in the dataset we will attempt to determine which, if any, of them influence the 
#result. Stating from the first and working through we look at how the residual resistance changes with each factor
#value.

#1.	Longitudinal position of the center of buoyancy, lcb.
import_data %>%
  group_by(lcb) %>%
  summarise(resid_reswei) %>%
  ggplot(aes(lcb, resid_reswei)) +
  geom_point()

#The plot shows while there are few different values for the Longitudinal position of the center of buoyancy, for 
#each one there are many different residual resistance values. Additionally there appear to be some clusters of
#values, a possible indication of a relationship with another factor or factors. One final note on this chart, it
#looks like no matter the value of Longitudinal Position, the residual resistance values tend to be closer to zero 
#and not down to random variation

#2.	Prismatic coefficient, cp
import_data %>%
  group_by(cp) %>%
  summarise(resid_reswei) %>%
  ggplot(aes(cp, resid_reswei)) +
  geom_point()

#3. Length-displacement ratio, dlr
import_data %>%
  group_by(dlr) %>%
  summarise(resid_reswei) %>%
  ggplot(aes(dlr, resid_reswei)) +
  geom_point()

#4. Beam-draft ratio, bt
import_data %>%
  group_by(bt) %>%
  summarise(resid_reswei) %>%
  ggplot(aes(bt, resid_reswei)) +
  geom_point()

#5. Length-beam ratio, lb
import_data %>%
  group_by(lb) %>%
  summarise(resid_reswei) %>%
  ggplot(aes(lb, resid_reswei)) +
  geom_point()

#The four scatterplots above for the 2nd to 5th variables follow a similar pattern as that for the first, a 
#range of residual resistances for individual variable values with no obvious correlation observed and residual 
#resistances being weighted towards zero. The clustering observed in the first plot persists and could be down to 
#the controlled variation in experimental values used in the course of data gathering.

#6. Froude number, fn
import_data %>%
  group_by(fn) %>%
  summarise(resid_reswei) %>%
  ggplot(aes(fn, resid_reswei)) +
  geom_point()

#This plot appears to show this variable as having a bearing on the residual resistance. The shape could be related
#to the equation by which the froude number is calculated, Fr = u/sqrt(gL). It could be the square root element that
#is influencing the shape when the equation is rearranged away from Froude number as the subject. It should be
#noted, there is still variation in residual resistance for identical Froude numbers, indicating this is not the
#only factor to influence the residual resistance. So, while the plots for the previous factors did not give any
#strong indications of a relationship with the residual resistance, they should not be discounted entirely.

cor(import_data$resid_reswei, import_data$lcb)
cor(import_data$resid_reswei, import_data$cp)
cor(import_data$resid_reswei, import_data$dlr)
cor(import_data$resid_reswei, import_data$bt)
cor(import_data$resid_reswei, import_data$lb)
cor(import_data$resid_reswei, import_data$fn)

#The above correlation values confirm that observed in the scatterplots, there is no direct relationship between the
#variables on their own and the residual resistance with the exception of the final variable, the Froude Number.

#A correlation and covariance matrix is now constructed to assess relationships between factors and residual 
#resistance to look for any relationships not seen by comparing individual factors to the residual resistance.

cor_mat <- cor(import_data) %>% round(digits = 2)
cov_mat <- cov(import_data) %>% round(digits = 2)
print(cor_mat)
print(cov_mat)

#For the most part there is little in the way of correlation and covariance between the factors and the residual 
#resistance, with the exception of the Froude number, which we already identified as being significant. This result
#does not assist in explaining the variation in residual resistance for a constant Froude Number.
#However, in order for the experiment data to be at all useful we must assume that all parameters, with the 
#exception of those in the import_data set, have remained the same so the variation observed must either be down to
#random variation or down to one, some or all of the remaining factors.

#Returning to these factors we now calculate the average residual resistance across each range of factor values, 
#looking again for any relationships

#1.	Longitudinal position of the center of buoyancy
import_data %>%
  group_by(lcb) %>%
  summarise(res = mean(resid_reswei)) %>%
  ggplot(aes(lcb, res)) +
  geom_point()

#2.	Prismatic coefficient
import_data %>%
  group_by(cp) %>%
  summarise(res = mean(resid_reswei)) %>%
  ggplot(aes(cp, res)) +
  geom_point()

#3. Length-displacement ratio
import_data %>%
  group_by(dlr) %>%
  summarise(res = mean(resid_reswei)) %>%
  ggplot(aes(dlr, res)) +
  geom_point()

#4. Beam-draft ratio
import_data %>%
  group_by(bt) %>%
  summarise(res = mean(resid_reswei)) %>%
  ggplot(aes(bt, res)) +
  geom_point()

#5. Length-beam ratio
import_data %>%
  group_by(lb) %>%
  summarise(res = mean(resid_reswei)) %>%
  ggplot(aes(lb, res)) +
  geom_point()

#An examination of the graphs produced indicates a possible relationship between the Prismatic coefficient and the 
#residual resistance. While not entirely clear, further support for a relationship between the Prismatic 
#Coeffecient and the Residual Resistance can be obtained by looking at the equations for both the Coefficient, 
#which may show a relationship, and the Froude number, which displays a strong relataionship.

#The equation for calculating the Prismatic Coefficient is C_p = V/L_wl * A_m (where C_p is the Prismatic 
#Coefficient, V is Volume, L_wl is length at the waterline and A_m is Cross sectional area)

#The equation for calculating the Froude number is Fn_l = u / sqrt(g * L_wl) (where Fn_l is the Froude Number, 
#u is relative flow velocity, g is the acceleration due to gravity, L_wl is length at the waterline)

#u is not defined so we must consider it constant and g is also constant. This means the only remaining part of the
#equation for calculating the Froude Number is L_wl and must have a bearing on residual resistance.

#Relating this to Prismatic coefficient we see L_wl is a common factor in the above equations, so where there is a
#relationship with residual resistance for one factor, it is likely to continue for another. 

#For this reason this factor will be included in creating a prediction model.

#We now return to the data behind the chart comparing the Froude number with the Residual resistance.
import_data %>%
  group_by(fn) %>%
  summarise(resid_reswei) %>%
  ggplot(aes(fn, resid_reswei)) +
  geom_point()

#To ease examination the average and standard deviation resistance is taken for each Froude number  
froude_data <- import_data %>%
  group_by(fn) %>%
  summarise(resid_reswei)

froude_data <- froude_data %>%
  group_by(fn) %>%
  summarise(mean_res = mean(resid_reswei), sd_res = sd(resid_reswei))

froude_data %>%
  gather(stat, val, 'mean_res', 'sd_res') %>%
  ggplot() +
  geom_point(aes(fn, val, color = stat))

#The mean resistance (blue) retains an exponential appearance, standard deviation (red) doesn't look to follow.
#This relationship is clearly non linear so it presents us two options when it comes to modelling. Either use a 
#non-linear regression method or attempt to straighten the curve and factor this into a linear regression model. 

#The curve is partially straightened, producing a linear relationship, by using log values of Froude number to 
#predict log values of Residual Resistance. While not completely straight this could prove a useful approximation
#given it would be unlikely for the this process to produce a straight line where there are other influences on the
#resistance.

import_data %>%
  group_by(fn) %>%
  summarise(resid_reswei) %>%
  mutate(log_fn= log(fn), log_res= log(resid_reswei)) %>%
  ggplot(aes(log_fn, log_res)) +
  geom_point()

#Above is unclear, use mean values to simplify

froude_data %>% mutate(log_fn= log(fn), log_res= log(mean_res)) %>%
  ggplot() +
  geom_point(aes(log_fn, log_res))

#Before proceeding it is important to return to the four factors that did not appear to have any relationship with
#the residual resistance. It should first be noted for all factors the number of data points was low, making it
#challenging to observe any relationships. While we were unable to determine a positive relationship with the
#remaining factors, we should not discount them from further analysis, the reasons for which are outlined below

#1.	Longitudinal position of the center of buoyancy, lcb.
#The longitudinal centre of buoyancy is at the centre of the underwater volume, that is, the volume of the hull that
#lies beneath the waterline. Since this relates to volume below the waterline it could be argued that the length of
#the hull at the waterline is important which may indicate a relationship with resistance as it has previously.

#3. Length-displacement ratio, dlr
#The length-displacement ratio describes how heavy a craft is in relation to its length at the waterline.
#Defined as dlr = (displacement(lb)/2240)/((0.01*L_wl)^3), L_wl, length at the waterline once again has a bearing 
#on the calculated value, in the same way it does for Prismatic Coefficient and Froude Number.

#4. Beam-draft ratio, bt
#It would be unwise to pivot all arguements solely on the length of a hull at the waterline. The beam-draft ratio,
#the ratio of the beam, the width of the ship at the widest point at the waterline, and the draft, the distance
#between the waterline and deepest part of the hull, provides us a rough idea of the cross sectional area of 
#submerged hull. If we look back to the equation for calculating the Prismatic Coefficient, C_p = V/L_wl * A_m, 
#A_m represented the cross sectional area. While this by no means proves a link to the residual resistance, it 
#could be argued that there are reasonable grounds to consider the Beam-draft ratio as having some influence on the
#residual resistance

#5. Length-beam ratio, lb
#The Length-beam ratio describes the rough shape of the hull at the waterline. If we consider the length at the 
#waterline and the beam measurement in the context of the other dataset factors, and we are happy that they, in part 
#or as a whole contribute to the residual resistance, it must follow that this ratio also has an effect.

#With the above in mind we are left with a choice, do we produce a model that discounts these factors, based on a 
#lack of evidence, a model that includes all these factors or do we produce a model that adds them stepwise and
#retains more control over the training.

#The structured approach is to first model the factors Froude Number followed by Prismatic Coefficient using
#three different linear regression models, followed by three models that do not use linear regression, at each 
#stage the performance of the models will be assessed and after modelling Prismatic coefficient, the best 
#performing retained. 
#Following this, we will add the remaining factors lcb, dlr, bt and lb, one at a time, assess models performance 
#and remove the factors again as appropriate. Once factors have been chosen the model is tuned.

#The assessment of the models will be based on the RMSE on the basis that it can be applied and
#to both linear and non-linear regression models allowing for direct comparison of model performance and that it
#can be applied to continuous data

#Housekeeping - removed some values/lists/data frames etc no longer required
rm(cor_mat)
rm(cov_mat)
rm(froude_data)
rm(html)
rm(split_doc)
rm(tab2)
rm(tabs)
rm(data_file)
rm(doc_text)


#Model Creation and testing

#(To summarise what is to be attempted after exploring the data, we will attempt to model the Residual Resistance 
#based on values of Froude Number and Prismatic Coefficient, both of which showed a possible relationship with the
#resistance values. The relationship between the Froude number and Residual Resistance is non linear so we will 
#attempt to build a model around both linear and non-linear models and then assess which performs best.)


#Before we begin building the models we must first create a training and test set.
#Since the dataset is small we will use an 80/20 train/test split and use k-fold cross validation on the training 
#set to improve accuracy before assessing the model on the test set. 10 fold cross validation with 20% of the 
#training should be sufficient to strike a balance between having a sample size that is not too closely related to
#the full training data set, overtraining and processing time.

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = import_data$resid_reswei, times = 1, p = 0.2, list = FALSE)
res_train <- import_data[-test_index,]
res_test <- import_data[test_index,]

#We will use the following model types, grouped as linear and non-linear, to first get an overview of accuracy as 
#these represent a reasonable overview of the different models available to us, before moving forward with the more
#sucessful of these models and tuning to improve the result.

#Linear regression models
lr_model_ens <- c("lm", "glm", "svmLinear")

#Non-linear regression models
nlr_model_ens <- c("svmPoly", "knn", "RRF")

#Define 10-fold cross validation used in training
control <- trainControl(method = "cv", number = 10, p = 0.8)

#Note: model name has been added to guage progress, defined control added and as referred to before, log values for 
#fn and resid_reswei are used. Seed is set before training to ensure models use the same cross validation samples.
set.seed(1, sample.kind="Rounding")
lr_fits <- lapply(lr_model_ens, function(model){ 
  print(model)
  train(log(resid_reswei) ~ log(fn), method = model, data = res_train, trControl = control)
})

#Establish RMSE for each method. It should be noted the RMSE generated by the fit is that for log residual 
#resistance and needs converting back to resistance.
n_models <- seq(1:3)
lr_rmses <- sapply(n_models, function(m_number){
  lr_fits[[m_number]][["results"]][["RMSE"]]
})

lr_rmses <- as.data.frame(lr_rmses) %>%
  mutate(model_name = lr_model_ens)
colnames(lr_rmses) <- c("RMSE", "model_name")
lr_rmses <- lr_rmses %>%
  mutate(adjusted_RMSE = exp(RMSE))
print(lr_rmses)

#Above is repeated, adding the Prismatic Coefficient.
set.seed(1, sample.kind="Rounding")
lr_fits_cp <- lapply(lr_model_ens, function(model){ 
  print(model)
  train(log(resid_reswei) ~ log(fn) + cp, method = model, data = res_train, trControl = control)
})

lr_rmses_cp <- sapply(n_models, function(m_number){
  lr_fits_cp[[m_number]][["results"]][["RMSE"]]
})

lr_rmses_cp <- as.data.frame(lr_rmses_cp) %>%
  mutate(model_name = lr_model_ens)
colnames(lr_rmses_cp) <- c("RMSE", "model_name")
lr_rmses_cp <- lr_rmses_cp %>%
  mutate(adjusted_RMSE = exp(RMSE))
print(lr_rmses_cp)

#Looking at both sets of RMSE we see there is no great difference made by adding the prismatic coefficient to the 
#model, with one of the three models tested producing a worse RMSE. This may reflect the observations made in exploring the data, where the relationship was not necessarily 
#clear.
print(lr_rmses)
print(lr_rmses_cp)

#Moving on to the non-linear models
set.seed(1, sample.kind="Rounding")
nlr_fits <- lapply(nlr_model_ens, function(model){ 
  print(model)
  train(resid_reswei ~ fn, method = model, data = res_train, trControl = control)
})

#Establish RMSE for each method, use of train in caret automatically chooses most favourable tuning parameters, and
#in the process, produces a number of different evaluation metrics. As a result of this we select the minimum value
#for RMSE on the basis that this result is achievable by tuning the model.
n_models <- seq(1:3)
nlr_rmses <- sapply(n_models, function(m_number){
  min(nlr_fits[[m_number]][["results"]][["RMSE"]])
})

nlr_rmses <- as.data.frame(nlr_rmses) %>%
  mutate(model_name = nlr_model_ens)
colnames(nlr_rmses) <- c("RMSE", "model_name")
print(nlr_rmses)

#Prismatic Coefficient added and RMSEs recorded
set.seed(1, sample.kind="Rounding")
nlr_fits_cp <- lapply(nlr_model_ens, function(model){ 
  print(model)
  train(resid_reswei ~ fn  + cp, method = model, data = res_train, trControl = control)
})

n_models <- seq(1:3)
nlr_rmses_cp <- sapply(n_models, function(m_number){
  min(nlr_fits_cp[[m_number]][["results"]][["RMSE"]])
})

nlr_rmses_cp <- as.data.frame(nlr_rmses_cp) %>%
  mutate(model_name = nlr_model_ens)
colnames(nlr_rmses_cp) <- c("RMSE", "model_name")
print(nlr_rmses_cp)

#Comparing the RMSE values from the models with and without prismatic coefficient
print(nlr_rmses)
print(nlr_rmses_cp)

#In both the linear and non-linear regression models, addition of the prismatic coefficient had either a small rise
#or a small reduction in the RMSE for the residual resistance, however it is clear a particular non-linear 
#approach gives better results.
print(rbind((lr_rmses_cp %>% select(-RMSE) %>% rename(RMSE = adjusted_RMSE)), nlr_rmses_cp))

#With this in mind it could be useful to spend a short amount of time investigating similar model styles to see if
#further imporovements can be made before we attempt to add any of the remaining factors.

#look at models similar to RRF and repeat above process, added ranger, Rborist and rf
rf_model_ens <- c("ranger", "Rborist", "rf", "RRF")

set.seed(1, sample.kind="Rounding")
rf_fits_cp <- lapply(rf_model_ens, function(model){ 
  print(model)
  train(resid_reswei ~ fn + cp, method = model, data = res_train, trControl = control)
})

n_rf_models <- seq(1:4)
rf_rmses_cp <- sapply(n_rf_models, function(m_number){
  min(rf_fits_cp[[m_number]][["results"]][["RMSE"]])
})

rf_rmses_cp <- as.data.frame(rf_rmses_cp) %>%
  mutate(model_name = rf_model_ens)
colnames(rf_rmses_cp) <- c("RMSE", "model_name")
print(rf_rmses_cp)

#There are two models performing noticably better than the others, ranger and RRF. We will now look at the fits and
#based on the information on current tuning at RMSE minimum, attempt to tune these models further before moving on
#to looking at the effect of adding further factors.

#ranger
rf_fits_cp[[1]][["results"]]
rf_fits_cp[[1]][["bestTune"]]
#The above describes the combination of parameters used in modelling along with that which was most sucessful

#For mtry the default is the number of predictor variables/3. In our dataset this number will not change, even if 
#we add all the variables to the model but to be thorough we will check mtry values 1-2 for best performance.

#split rule set as extratrees is more successful than variance so we will keep this as is.

#Since min.node.size has a default value of 5 for regression, and we are performing a regression this parameter will
#be left as 5

ranger_grid <- expand.grid(mtry = seq(1:2), splitrule = "extratrees", min.node.size = 5)
set.seed(1, sample.kind="Rounding")
ranger_fn_cp_tune <- train(resid_reswei ~ fn + cp, method ="ranger", data = res_train, trControl = control, tuneGrid = ranger_grid)
ranger_fn_cp_tune[["results"]]

#The original tuning produces the lowest RMSE at this stage

#RRF
rf_fits_cp[[4]][["results"]]
rf_fits_cp[[4]][["bestTune"]]
#Since the mtry value has already been tested on the ranger model and the results for a value of 1 were 
#significantly worse than that of 2, this parameter will be left as it is.

#The coefReg values and coefImp values in the best tune of 1 and 0.5 can only take values from zero to one so we
#look at improving precision by examining values around the best tune figures.
rrf_grid <- expand.grid(mtry = 2, coefReg = seq(0.75, 1, 0.05), coefImp = seq(0.4, 0.5, 0.01))
set.seed(1, sample.kind="Rounding")
rrf_fn_cp_tune <- train(resid_reswei ~ fn + cp, method ="RRF", data = res_train, trControl = control, tuneGrid = rrf_grid)
rrf_fn_cp_tune[["bestTune"]]

#Optimal values tuning parameters for RRF model are mtry = 2, coefReg = 0.85, coefImp = 0.46

#After tuning, ranger model returns lowest RMSE but we will move forward with both to see if this advantage changes
#with the addition of other factors, indicating a different approach may be stronger. We will not move forward with
#the tuned values at this stage as the addition of other factors will likely change things, however the RMSE values 
#determined are recorded.
rrf_fn_cp_tuned_RMSE <- min(rrf_fn_cp_tune[["results"]][["RMSE"]])
ranger_fn_cp_tuned_RMSE <- min(ranger_fn_cp_tune[["results"]][["RMSE"]])

#Addition of extra factors
#At this stage we will now begin adding the factors for which there was no visual proof of a relationship to the 
#residual resistance but where a mathematical relationship looked likely. Since we do not know the precise nature 
#of any relationship, present or otherwise, we need to consider this experimental and should not assume 
#improvements in RMSE will follow.
rf_model_ens_exp <- c("ranger", "RRF")

#Longitudinal position of the center of buoyancy, lcb.
set.seed(1, sample.kind="Rounding")
rf_fits_lcb <- lapply(rf_model_ens_exp, function(model){ 
  print(model)
  train(resid_reswei ~ fn + cp + lcb, method = model, data = res_train, trControl = control)
})

n_rf_exp_models <- seq(1:2)
rf_rmses_lcb <- sapply(n_rf_exp_models, function(m_number){
  min(rf_fits_lcb[[m_number]][["results"]][["RMSE"]])
})

rf_rmses_lcb <- as.data.frame(rf_rmses_lcb) %>%
  mutate(model_name = rf_model_ens_exp)
colnames(rf_rmses_lcb) <- c("RMSE", "model_name")
print(rf_rmses_lcb)

#RMSE has improved for both models, so this factor will be included in the model

#Length-displacement ratio, dlr
set.seed(1, sample.kind="Rounding")
rf_fits_dlr <- lapply(rf_model_ens_exp, function(model){ 
  print(model)
  train(resid_reswei ~ fn + cp + dlr, method = model, data = res_train, trControl = control)
})

n_rf_exp_models <- seq(1:2)
rf_rmses_dlr <- sapply(n_rf_exp_models, function(m_number){
  min(rf_fits_dlr[[m_number]][["results"]][["RMSE"]])
})

rf_rmses_dlr <- as.data.frame(rf_rmses_dlr) %>%
  mutate(model_name = rf_model_ens_exp)
colnames(rf_rmses_dlr) <- c("RMSE", "model_name")
print(rf_rmses_dlr)

#The RMSEs reported now are higher than before, so we will not include this factor in the model

#Beam-draft ratio, bt
set.seed(1, sample.kind="Rounding")
rf_fits_bt <- lapply(rf_model_ens_exp, function(model){ 
  print(model)
  train(resid_reswei ~ fn + cp + bt, method = model, data = res_train, trControl = control)
})

n_rf_exp_models <- seq(1:2)
rf_rmses_bt <- sapply(n_rf_exp_models, function(m_number){
  min(rf_fits_bt[[m_number]][["results"]][["RMSE"]])
})

rf_rmses_bt <- as.data.frame(rf_rmses_bt) %>%
  mutate(model_name = rf_model_ens_exp)
colnames(rf_rmses_bt) <- c("RMSE", "model_name")
print(rf_rmses_bt)

#Reported RMSEs are marginally higher this time than before, so we will not include this factor either

#Length-beam ratio, lb
set.seed(1, sample.kind="Rounding")
rf_fits_lb <- lapply(rf_model_ens_exp, function(model){ 
  print(model)
  train(resid_reswei ~ fn + cp + lb, method = model, data = res_train, trControl = control)
})

n_rf_exp_models <- seq(1:2)
rf_rmses_lb <- sapply(n_rf_exp_models, function(m_number){
  min(rf_fits_lb[[m_number]][["results"]][["RMSE"]])
})

rf_rmses_lb <- as.data.frame(rf_rmses_lb) %>%
  mutate(model_name = rf_model_ens_exp)
colnames(rf_rmses_lb) <- c("RMSE", "model_name")
print(rf_rmses_lb)
#Once again, RMSE has risen so this factor will be excluded.

#Add RMSEs to table for easy comparison
rf_rmses_lcb <- rf_rmses_lcb %>% mutate(factor = "fn, cp & lcb")
rf_rmses_dlr <- rf_rmses_dlr %>% mutate(factor = "fn, cp & dlr")
rf_rmses_bt <- rf_rmses_bt %>% mutate(factor = "fn, cp & bt")
rf_rmses_lb <- rf_rmses_lb %>% mutate(factor = "fn, cp & lb")
exp_factor_RMSEs <- rbind(c(ranger_fn_cp_tuned_RMSE,"ranger","fn & cp"), c(rrf_fn_cp_tuned_RMSE,"RRF","fn & cp"), rf_rmses_lcb, rf_rmses_dlr, rf_rmses_bt, rf_rmses_lb)

#With all the above factors with the exception of the longitudinal position of the center of buoyancy producing
#negative effects on prediction accuracy we will tune the model containing only the factors fn, cp, and lcb.

#ranger
rf_fits_lcb[[1]][["results"]]
rf_fits_lcb[[1]][["bestTune"]]
#As was the case before, accuracy improved with increase in mtry value, splitrule was more accurate set as 
#extratrees and min.node.size is still 5 indicating regression so this tune could not be improved upon.

#RRF
rf_fits_lcb[[2]][["results"]]
rf_fits_lcb[[2]][["bestTune"]]

#The coefReg values and coefImp values in the best tune of 1 can only take values from zero to one so we
#look at improving precision by examining values around the best tune figures.
rrf_grid_exp <- expand.grid(mtry = seq(1:3), coefReg = seq(0.9, 1, 0.025), coefImp = seq(0.9, 1, 0.025))
set.seed(1, sample.kind="Rounding")
rrf_lcb_tune_fit <- train(resid_reswei ~ fn + cp + lcb, method ="RRF", data = res_train, trControl = control, tuneGrid = rrf_grid_exp)
rrf_lcb_tune_fit[["bestTune"]]
min(rrf_lcb_tune_fit[["results"]][["RMSE"]])
#Optimal values tuning parameters for RRF model are mtry = 3, coefReg = 0.9, coefImp = 0.95, however after tuning
#the model is less accurate than that produced using ranger.
rrf_fn_cp_lcb_tuned_RMSE <- min(rrf_lcb_tune_fit[["results"]][["RMSE"]])
ranger_fn_cp_lcb_tuned_RMSE <- min(rf_fits_lcb[[1]][["results"]][["RMSE"]])
rf_fn_cp_lcb_RMSEs <- data.table(method = c("RRF", "ranger"), RMSE = c(rrf_fn_cp_lcb_tuned_RMSE, ranger_fn_cp_lcb_tuned_RMSE))

#Ranger model is now produced and performance assessed on the test set.
set.seed(1, sample.kind="Rounding")
ranger_final_fit <- train(resid_reswei ~ fn + cp + lcb, method ="ranger", data = res_train, trControl = control)

#Predictions made on test set and performance measured against actual values
res_test_pred <- predict(ranger_final_fit, res_test)
fin_model_RMSE <- RMSE(res_test_pred, res_test$resid_reswei)

#Final RMSE calculated as 0.9936
#To reach an informed conclusion we should get some idea of the distribution of the differences between the 
#predicted residual resistance and the actual residual resistance to identify any strengths or weaknesses
#within the model. To that end we will plot the two aganist the three factors used to make the predictions.

res_test_conc <- res_test %>%
  select(fn, cp, lcb, resid_reswei) %>%
  mutate(pred_resid_reswei = res_test_pred)

set.seed(1, sample.kind="Rounding")
res_test_conc %>%
  ggplot() +
  geom_jitter(aes(fn, resid_reswei, colour = "Actual Residual Resistance")) +
  geom_jitter(aes(fn, pred_resid_reswei, colour = "Predicted Residual Resistance"))

set.seed(1, sample.kind="Rounding")
res_test_conc %>%
  ggplot() +
  geom_jitter(aes(cp, resid_reswei, colour = "Actual Residual Resistance")) +
  geom_jitter(aes(cp, pred_resid_reswei, colour = "Predicted Residual Resistance"))

set.seed(1, sample.kind="Rounding")
res_test_conc %>%
  ggplot() +
  geom_jitter(aes(lcb, resid_reswei, colour = "Actual Residual Resistance")) +
  geom_jitter(aes(lcb, pred_resid_reswei, colour = "Predicted Residual Resistance"))

#We can see the differences between predicted and actual values more clearly if we plot the differences against 
#each factor
res_test_conc_dif <- res_test_conc %>%
  mutate(res_dif = resid_reswei - pred_resid_reswei)

res_test_conc_dif %>%
  ggplot(aes(fn, res_dif)) +
  geom_point()

res_test_conc_dif %>%
  ggplot(aes(cp, res_dif)) +
  geom_point()

res_test_conc_dif %>%
  ggplot(aes(lcb, res_dif)) +
  geom_point()

#Looking at the plots it seems the model loses accuracy as Froude Number increases, this seems logical as the 
#magnitude of the residual resistance increases greatly with an increase in Froude Number. The other factors, 
#prismatic coefficient, cp, and Longitudinal position of thse center of buoyancy, lcb, do not show any obvious signs
#that they have a large impact on the model accuracy with an increase or decrease in their values. Again, this is a 
#pattern shown earlier when exploring the data.

#data is saved so it can be loaded by the rmd file for compilation
save.image(file = "hydrodyn.RData")
  
