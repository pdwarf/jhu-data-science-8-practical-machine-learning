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