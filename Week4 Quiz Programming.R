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