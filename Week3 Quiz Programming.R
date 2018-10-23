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