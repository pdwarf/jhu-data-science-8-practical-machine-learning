#Q1
library(ElemStatLearn)
library(caret)

data(vowel.train)
data(vowel.test)

vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)

set.seed(33833)

modelForest <- train(y ~., method = "rf", data = vowel.train)
modelBoosted <- train(y ~., method = "gbm", data = vowel.train)

predForest <- predict(modelForest, vowel.test)
predBoosted <- predict(modelBoosted, vowel.test)

predRightForest <- predForest==vowel.test$y
predRightBoosted <- predBoosted==vowel.test$y

comparisonDF <- as.data.frame(cbind(as.integer(predForest),as.integer(predBoosted),as.integer(vowel.test$y))) #question: when the two predictions agree, how often are they right?
agreedDF <- subset(comparisonDF, V1 == V2)
agreedAndRightDF <- subset(agreedDF, V1 == V3)

agreementAccuracy <- nrow(agreedAndRightDF) / nrow(agreedDF)

forestTestAccuracy <- as.integer(summary(predRightForest)[3])/length(predRightForest)
boostedTestAccuracy <- as.integer(summary(predRightBoosted)[3])/length(predRightBoosted)

forestTestAccuracy
boostedTestAccuracy
agreementAccuracy

#Q2
library(caret)
library(gbm)
library(AppliedPredictiveModeling)

set.seed(3433)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)

modRF <- train(diagnosis ~., method = "rf", data = training)
modGBM <- train(diagnosis ~., method = "gbm", data = training)
modLDA <- train(diagnosis ~., method = "lda", data = training)

predRF <- predict(modRF, testing)
predGBM <- predict(modGBM, testing)
predLDA <- predict(modLDA, testing)

predDF <- data.frame(predRF, predGBM, predLDA, diagnosis = testing$diagnosis)

modStacked <- train(diagnosis ~ ., method = "rf", data = predDF)
predStacked <- predict(modStacked, predDF)

data.frame(RF_Accuracy = confusionMatrix(predRF, testing$diagnosis)$overall[1],
           GBM_Accuracy = confusionMatrix(predGBM, testing$diagnosis)$overall[1],
           LDA_Accuracy = confusionMatrix(predLDA, testing$diagnosis)$overall[1],
           Stacked_Accuracy = confusionMatrix(predStacked, testing$diagnosis)$overall[1]
)


#Q3
set.seed(3523)

library(AppliedPredictiveModeling)
library(elasticnet)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)

modelFit <- train(CompressiveStrength ~ ., method = "lasso", data = training)

object <- predict(modelFit$finalModel, type = 'coefficients')

object$coefficients #Cement is set to zero last

plot.enet(modelFit$finalModel, xvar = "penalty", use.color = TRUE) #Cement is set to zero last



#Q4
library(lubridate) # For year() function below
library(forecast)

dat = read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv")

ts1 = ts(dat$visitsTumblr)

ts1train <- window(ts1, start = 1, end = 365)
ts1test <- window(ts1, start = 366, end = 600)

modelFit <- bats(ts1train)
fcast <- forecast(modelFit, h = 235, frequency = 1)

plot(fcast)
lines(ts1test, col="red")

table(ts1test <= fcast$lower[,2] | ts1test >= fcast$upper[,2])

table(ts1test <= fcast$lower[,2] | ts1test >= fcast$upper[,2])[1] / length(ts1test)



#Q5
set.seed(3523)

library(AppliedPredictiveModeling)
library(e1071)

set.seed(325)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]
testing = concrete[-inTrain,]

modelFit <- svm(CompressiveStrength ~ ., data = training)

predSVM <- predict(modelFit, testing)

RMSE(predSVM, testing$CompressiveStrength)