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

