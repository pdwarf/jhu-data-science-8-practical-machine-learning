#Q1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rattle)

training <- subset(segmentationOriginal, Case == "Train")

set.seed(125)

modelFit <- train(Class ~ ., method = "rpart", data = training)

fancyRpartPlot(modelFit$finalModel)

#newCases <- PS, WS, PS, not possible


#Q3
library(pgmm)
library(caret)
data(olive)
olive = olive[,-1]

modelFit <- train(Area ~ ., method = "rpart", data = olive)

newdata = as.data.frame(t(colMeans(olive)))

predict(modelFit, newdata)
#Strange, b/c area should be qualitative, not numeric

#Q4
library(ElemStatLearn)
library(caret)

data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
modelFit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, method = "glm", family = "binomial", data = trainSA)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(trainSA$chd, predict(modelFit, trainSA)) #[1] 0.2727273
missClass(testSA$chd, predict(modelFit, testSA)) #[1] 0.3116883

#Q5

library(ElemStatLearn)
#library(caret)
library(randomForest)

data(vowel.train)
data(vowel.test)

vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)

vowel.complete <- rbind(vowel.train, vowel.test)

set.seed(33833)

modelFit <- randomForest(y ~ ., data = vowel.train)
varImp(modelFit)
order(varImp(modelFit), decreasing = T)

#updates in the packages seem to have changed the values; 
#my results are most similar to the option in the quiz 
#"The order of the variables is: 
#x.2, x.1, x.5, x.6, x.8, x.4, x.9, x.3, x.7,x.10"