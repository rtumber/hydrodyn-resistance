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
  ggplot() +
  geom_point(aes(fn, mean_res), col = "blue") +
  geom_point(aes(fn, sd_res), col ="red")

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
#Following this, we will add the remaining factors lcb, dlr, bt and lb, one at a time, then tuning the model or 
#removing the factors again as appropriate.

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
rm(att_1_6)
rm(att_7)
rm(data_file)
rm(doc_text)


