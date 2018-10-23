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