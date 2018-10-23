
#Q2
mixtures$FlyAshFactor <- cut2(mixtures$FlyAsh, g=3)
qplot(1:length(mixtures$CompressiveStrength), mixtures$CompressiveStrength, colour=mixtures$FlyAshFactor)

mixtures$CementFactor <- cut2(mixtures$Cement, g=3)
qplot(1:length(mixtures$CompressiveStrength), mixtures$CompressiveStrength, colour=mixtures$CementFactor)
#the cement explains the non-randomness in the index of comp strength

#Q3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
# see answer here: https://www.rapidtables.com/math/algebra/logarithm/Logarithm_of_0.html

#Q4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors[,57:68])
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

preObj <- preProcess(training, method = "pca", thresh = 0.8)
preObj

#Q5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors[,57:68])
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

#Model with keeping the respective variables as they are
model1 <- train(diagnosis ~., method="glm", data=training)
confusionMatrix(testing$diagnosis, predict(model1, testing))
#result: 64.63% accuracy


#...with PCA retaining 80% of variance
preObj <- preProcess(training[,-1], method = "pca", thresh = 0.8) #heads up: leave out the outcome variable "diagnosis" of the training data set by dropping the first column --> not ultimately necessary as the PCA preprocessing otherwise automatically ignores the variable, but it's the cleaner way!
trainPCA <- predict(preObj, training)
model2 <- train(diagnosis ~.,method="glm", data=trainPCA)
testPCA <- predict(preObj, testing)
confusionMatrix(testing$diagnosis, predict(model2, testPCA))
#result: 71.95% accuracy