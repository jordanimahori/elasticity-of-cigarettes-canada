#Code is a very cleaned version; does not include numerous cases of trial and error
#to tune parameters, fit models, and so on. This is the version that works with just one run of the code.

#Set working directory
setwd("C:/Users/Lucas/Google Drive/Fourth Year/Second Semester/ECO446/Coding")
library(ggplot2)
library(e1071)
library(klaR)
library(caTools)
library(ElemStatLearn)
library(SuperLearner) 
library(dplyr) 
library(ipred)
library(caret) 
library(biglasso)
library(tidyverse)
library(mlr)

#Import smuggling data
smug <- read.csv("Smuggling Estimation Data.csv", na.strings=c("","NA"))

#Square of year
smug <- smug[smug$year>=1989,]
province <- smug[,1]
year_ref <- smug[,2]
smoker_pct_sqrd <- (smug$smoker_pct)^2
smug <- cbind(smug, smoker_pct_sqrd)
smug <- cbind(year_ref, smug)

#Generate dummy variables
smug <- createDummyFeatures(smug, cols = "province")

#Drop SK
smug <- cbind(province, smug)
smug <- subset(smug, select = -province.SK)

#Version of smug with observations with missing values removed
smug_r <- smug[!is.na(smug$cartons_pc), ]

#Version of smug_r with observations with missing values removed for smoker_pct
smug_r <- smug_r[smug_r$year_ref>=1989, ]
smug_r <- smug_r[!is.na(smug_r$crim_code_per_officer),]

#Version of smug with observations with missing values removed from smuggling
smug_class <- smug_r[!is.na(smug_r$Smuggling), ]

#Data we want predictions for for smuggling
smug_pred <- smug_r[is.na(smug_r$Smuggling), ]

#Dividing data into training and test sets
set.seed(302)
smug_class <- smug_class[sample(nrow(smug_class)),] #shuffle dataset 
smp_siz = floor(0.75*nrow(smug_class)) 
training_ind <- sample(seq_len(nrow(smug_class)),size = smp_siz)
training_set =smug_class[training_ind, ]
test_set <- smug_class[-training_ind, ] 

#Remove and store province col and year
train_prov <- training_set[,1]
training_set <- training_set[,-1]
test_prov <- test_set[,1]
test_set <- test_set[,-1]

train_year_ref <- training_set[,1]
training_set <- training_set[,-1]
test_year_ref <- test_set[,1]
test_set <- test_set[,-1]

#Remove and store Smuggling and binarize
train_smug <- as.numeric(training_set[,2])-1
training_set <- training_set[,-2]
test_smug <- as.numeric(test_set[,2])-1
test_set <- test_set[,-2]


###Ensemble
set.seed(700)
model <- SuperLearner(train_smug,
                      training_set,
                      family=binomial(),
                      cvControl = list(V=5),
                      SL.library=list(
                        "SL.randomForest",
                        "SL.biglasso"
                        ))
model

#Recode predictions with ensemble
predictions <- predict.SuperLearner(model, newdata=test_set)
conv_predictions <-  ifelse(predictions$pred>=0.38,1,0)

#Confusion matrix
cm <- confusionMatrix(table(conv_predictions, test_smug))
print(cm)

#AUC with test data
roc_obj <- roc(test_smug, as.numeric(predictions$pred))
auc(roc_obj)


################

#Predictions on missing values
#Remove and store smuggling col
smug_prov <- smug_pred[,1]
smug_pred <- smug_pred[,-1]
smug_year <- smug_pred[,1]
smug_pred <- smug_pred[,-1]
smug_col <- smug_pred[,2]
smug_pred <- smug_pred[,-2]

predictions <- predict.SuperLearner(model, newdata=smug_pred)
conv_predictions <-  ifelse(predictions$pred>=0.38,1,0)

#Comparision 
cols <- list(smug_prov, smug_year, conv_predictions, predictions$pred)
ensemble_compare <- as.data.frame(cols)
colnames(ensemble_compare) <- c('Province', 'Year','Prediction', 'Probability')
View(ensemble_compare)

