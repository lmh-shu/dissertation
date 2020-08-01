
library(caret)
library(caTools)
library(dplyr)
library(kernlab)
library(fastAdaboost)
library(skimr)
library(RANN)  # required for knnInpute
library(randomForest)
library(earth)
library(e1071)

source("R/data_analysis.R")
rm(chains, latestURNs, manual_input, ofsted, qualifiedleavers, rebrokerages, schools, schools_size,
   swc_acads, swc_acads_w_trust, teachers, turn_wast_schools, turnover)


# Prepare data -----------------------------------------------
reduced_dataset <- turn_wast_teachers %>%
  filter(CensusYear == 2017) %>%
  dplyr::select(# Teacher's characteristics
         QTStatus, 
         TOTFTE,
         ContractAgreementType,
         GrossPay,
         Leadership,
         # School's characteristics
         School_Type, 
         SchoolPhase_Grouped,
         School_FTE_Teachers, 
         OfstedOverallScore, 
         YearInTrust,
         Wastage) %>%
  mutate(Wastage = as.factor(Wastage))

# YearInTrust = 0 if non-academy 
reduced_dataset <- reduced_dataset %>%
  mutate(YearInTrust = case_when(is.na(YearInTrust) ~ 0, TRUE ~ YearInTrust))


# Data splitting -----------------------------------------------

# The function createDataPartition can be used to create balanced splits of the data. 
# If the y argument to this function is a factor, the random sampling occurs within each 
# class and should preserve the overall class distribution of the data

# The list = FALSE avoids returning the data as a list. 
# This function also has an argument, times, that can create multiple splits at once; 
# the data indices are returned in a list of integer vectors.

set.seed(3456)
trainIndex <- createDataPartition(reduced_dataset$Wastage, p = .8, 
                                  list = FALSE, 
                                  times = 1)

trainData <- reduced_dataset[ trainIndex,]
testData  <- reduced_dataset[-trainIndex,]

# Store X and Y for later use.
x = trainData[, 1:10]
y = trainData$Wastage


# Descriptive stats -------------------------------------------------------

skimmed <- skim_to_wide(trainData)
skimmed[, c(1:5, 9:11, 13, 15:16)]

# Pre-process data --------------------------------------

# Create the knn imputation model on the training data
preProcess_missingdata_model <- preProcess(trainData, method='knnImpute')
preProcess_missingdata_model

# Created from 367593 samples and 11 variables
# 
# Pre-processing:
#   - centered (7)
# - ignored (4)
# - 5 nearest neighbor imputation (7)
# - scaled (7)

#### MISSING DATA: Use the imputation model to predict the values of missing data points
trainData2 <- predict(preProcess_missingdata_model, newdata = trainData)
anyNA(trainData2)

#### DUMMY VARIABLES: One-Hot Encoding
# Creating dummy variables is converting a categorical variable to as many binary variables as here are categories.
dummies_model <- dummyVars(Wastage ~ ., data=trainData2)

# Create the dummy variables using predict. 
# The Y variable (Wastage) will not be present in trainData3.
trainData3 <- predict(dummies_model, newdata = trainData2)

# # Convert to dataframe
trainData3 <- data.frame(trainData3)

# # See the structure of the new dataset
str(trainData3)

#### TRANSFORM DATA: convert all the numeric variables to range between 0 and 1

preProcess_range_model <- preProcess(trainData3, method='range')
trainData4 <- predict(preProcess_range_model, newdata = trainData3)

# Append the Y variable
trainData4$Wastage <- y
apply(trainData4[, 1:24], 2, FUN=function(x){c('min'=min(x), 'max'=max(x))})


# Visualize importance of predictors --------------------------------------

featurePlot(x = trainData[, 1:24], 
            y = trainData$Wastage, 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))


featurePlot(x = trainData[, 1:24], 
            y = as.factor(trainData$Wastage), 
            plot = "density",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))



# Select the important features using recursive feature elimination (rfe) ----------------------------------------

memory.limit(size=2500)

set.seed(100)
options(warn=-1)

subsets <- c(5,8,10)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile <- rfe(x=trainData[, 1:18], y=trainData$Wastage,
                 sizes = subsets,
                 rfeControl = ctrl)

lmProfile


# Training and tuning the model -------------------------------------------

# See available algorithms in caret
modelnames <- paste(names(getModelInfo()), collapse=',  ')
modelnames
modelLookup('earth')

# Set the seed for reproducibility
set.seed(100)

# Train the model using randomForest and predict on the training data itself.
model_mars <- train(Wastage ~ ., data = trainData4, method='earth')
fitted <- predict(model_mars)
model_mars$coefnames
plot(model_mars, main="Model RMSE with MARS")
fitted_dt <- data.frame(fitted)

# Variable importance -----------------------------------------------------

varimp_mars <- varImp(model_mars)
plot(varimp_mars, main="Variable Importance with MARS")

# Prepare the test dataset and predict ------------------------------------

# Store X and Y for later use.
xx = testData[, 1:10]
yy = testData$Wastage

# Missing Value imputation –> One-Hot Encoding –> Range Normalization
# preProcess_missingdata_model –> dummies_model –> preProcess_range_model

# Step 1: Impute missing values 
# this takes a long time
testData2 <- predict(preProcess_missingdata_model, testData)  

# Step 2: Create one-hot encodings (dummy variables)
testData3 <- predict(dummies_model, testData2)
testData3 <- data.frame(testData3)

# Step 3: Transform the features to range between 0 and 1
testData4 <- predict(preProcess_range_model, testData3)

# Append the Y variable
testData4$Wastage <- yy
apply(testData4[, 1:24], 2, FUN=function(x){c('min'=min(x), 'max'=max(x))})

# View
head(testData4[, 1:25])

# Predict on testData -----------------------------------------------------

# Predict on testData
predicted <- predict(model_mars, testData4)
predicted <- data.frame(predicted)
head(predicted)

# Confusion Matrix --------------------------------------------------------

# Compute the confusion matrix
confusionMatrix(reference = testData4$Wastage, data = predicted, mode='everything', positive='1')

# testData4 %>% summarise(wastage = sum(as.double(Wastage))/n())
# 106459/98404
# levels(testData4$Wastage)
# levels(as.factor(predicted)) 
# testData4 %>% group_by(Wastage) %>% summarise(wastage = n())
# 
# (8055/90349)*100

model_mars$coefnames

model_mars$method

model_mars$results

model_mars$bestTune

model_mars$call

model_mars$metric

model_mars$finalModel

model_mars$trainingData

model_mars$resample

model_mars$resampledCM

model_mars$perfNames

model_mars$maximize

model_mars$yLimits

model_mars$levels

model_mars$terms

model_mars$xlevels

model_mars$fitControl

# Evaluate performance of multiple machine learning algorithms ---------------------------------------------------------------------

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

## Training Adaboost
set.seed(100)
# Train the model using adaboost
model_adaboost = train(as.factor(Wastage) ~ ., data=training_unproc[1:1000,], method='adaboost', tuneLength=2, trControl = fitControl)
model_adaboost

## Training Random Forest
set.seed(100)

# Train the model using rf
model_rf = train(Wastage ~ ., data=training, method='rf', tuneLength=5, trControl = fitControl)
model_rf

## Training xgBoost Dart
set.seed(100)

# Train the model using MARS
model_xgbDART = train(Wastage ~ ., data=training, method='xgbDART', tuneLength=5, trControl = fitControl, verbose=F)
model_xgbDART

## Support Vector Machines with Radial Basis Function Kernel
set.seed(100)
# Train the model 
model_svmRadial = train(Wastage ~ ., data=training, method='svmRadial', tuneLength=15, trControl = fitControl)
model_svmRadial


# Run resamples() to compare the models -----------------------------------
# Compare model performances using resample()
models_compare <- resamples(list(ADABOOST=model_adaboost, RF=model_rf, XGBDART=model_xgbDART, MARS=model_mars3, SVM=model_svmRadial))

# Summary of the models performances
summary(models_compare)

# Draw box plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare, scales=scales)







# # Scatterplots
# theme1 <- trellis.par.get()
# theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
# theme1$plot.symbol$pch = 16
# theme1$plot.line$col = rgb(1, 0, 0, .7)
# theme1$plot.line$lwd <- 2
# trellis.par.set(theme1)
# regVar <- c("TOTFTE", "GrossPay", "School_FTE_Teachers", "OfstedOverallScore")

# featurePlot(x = reduced_dataset[, regVar], 
#             y = reduced_dataset$Wastage, 
#             plot = "scatter", 
#             layout = c(3, 1))

# Pre-processing
# 
# 
# set.seed(825)
# logiboostFit1 <- train( Wastage ~ ., data = training, 
#                  method = "vglmAdjCat", # logistic regression boosted
#                  trControl = fitControl)


