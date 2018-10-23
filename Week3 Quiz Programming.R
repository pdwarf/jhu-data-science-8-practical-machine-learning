#Q1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rattle)

#inTrain <- createDataPartition(y = segmentationOriginal$Class, p = 0.7, list = F)
#training <- segmentationOriginal[Case = "Test",]
#testing <- segmentationOriginal[-inTrain,]

training <- subset(segmentationOriginal, Case == "Train")

set.seed(125)

modelFit <- train(Class ~ ., method = "rpart", data = training)

fancyRpartPlot(modelFit$finalModel)

#newCases <- PS, WS, PS, not possible