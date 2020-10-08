---
title: "Prediction Assignment Writeup"
author: "Riya Sutaria"
date: "5 October 2020"
output: html_document
---
## 1. Loading the required libraries.  
```{r, one}
#Loading the required libraries.
library(caret)
library(rpart)
```
## 2. Downloading the dataset and loading it into the directiories.   
```{r, two}
if(!file.exists("trainData.csv")){
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile = "trainData.csv", method = "curl")
}
if(!file.exists("testData.csv")){
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile = "testData.csv", method = "curl")
}

trueTrainData <- read.csv("trainData.csv")
trueTestData <- read.csv("testData.csv")

```
## 3. Crearing data partation for cross validation.  
```{r, three}
#To cross validate the data let's create train and test data sets from the original train data set to build a model and then use it on the original test data.
#Here i'm using 75% data to train and 25% to validate.
inTrain <- createDataPartition(trueTrainData$classe, p = 0.75, list = FALSE)
trainData <- trueTrainData[inTrain, ]
testData <- trueTrainData[-inTrain, ]
```
## 4. Preprocessing the Data.  
```{r, four}
#Preprocessing the training data first. To preprocess the data we'll do the below:
#Ignoring the problematic predictors by eliminating the variables with zero or near to zero variance :
nsv <- nearZeroVar(trainData)
trainData <- trainData[,-nsv]
testData <- testData[,-nsv]

#Removing the variables that are 95% NA.
notNA <- sapply(trainData, function(x) mean(is.na(x))) < 0.95
trainData <- trainData[, notNA == TRUE]
testData <- testData[ , notNA == TRUE]

#First 5 variables are identification variables so we can remove it.
trainData <- trainData[, -(1:5)]
testData <- testData[, -(1:5)]
```
## 5. Fitting different models.  
### 5.1. Fitting the model with trees method.  
```{r, five}
set.seed(1212)
rpartModel <- rpart(classe ~ ., data = trainData, method="class")
#Predicting test model with trees.
preTestData <- predict(rpartModel, newdata = testData, type = "class")
confusionMatrix(preTestData, testData$classe)
```
The Accuracy obtained with trees is 76.94%.  
### 5.2. Fitting the model with bagging method.  
```{r, six}
set.seed(1212)
treeBagModel <- train(classe ~ ., data = trainData, method = "treebag")
#Predicting test model with bagging.
preTestData <- predict(treeBagModel, newdata = testData)
confusionMatrix(preTestData, testData$classe)
```
The Accuracy obtained with bagging is 99.57%.  
### 5.3. Fitting the model with random forest method.  
```{r, seven}
set.seed(1212)
controlRf <- trainControl(method = "cv", number = 5)
randomForestModel <- train(classe ~ ., data = trainData, method = "rf", trControl = controlRf, verbose = FALSE)
#Predicting test model with random forests.
preTestData <- predict(randomForestModel, newdata = testData)
confusionMatrix(preTestData, testData$classe)
```
The Accuracy obtained with random forest is 99.65%.  
### 5.4. Fitting the model with boosting method.  
```{r, eight}
set.seed(1212)
controlGbm <- trainControl(method = "repeatedcv", number = 5, repeats = 2)
gbmModel <- train(classe ~ ., data = trainData, method = "gbm", trControl = controlGbm, verbose = FALSE)
#Predictong test data with boosting.
preTestData <- predict(gbmModel, newdata = testData)
confusionMatrix(preTestData, testData$classe)
```
The Accuracy obtained generalised boosting method is 98.80%.   
## Summary  
To summarize, the predictive accuracy of the models evaluated are as follows:  
Decision Tree Model : 76.94%  
Bagging Model : 99.57%  
Random Forest Model : 99.65%  
Boosting Model : 98.80%

Choosing the model with best accuracy(i.e. Random Forest Model):  
Applying Random Forest Model to the 20 original test data points given.
```{r, nine}
result <- predict(randomForestModel, trueTestData)
result
```

