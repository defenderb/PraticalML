# q1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)
# q2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
plot(training$CompressiveStrength, col = cut2(training$FlyAsh, g=10),pch=19)
featurePlot(x= training[, -9],
            y= training$CompressiveStrength,
            plot="pairs")
# q3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
hist(training$Superplasticizer,50)
hist(log(training$Superplasticizer),50)
# q4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
colnames <- names(training)
colnames <- grep("^IL",colnames, value=TRUE)
training2 <- training[,c("diagnosis",colnames)]
testing2 <- testing[,c("diagnosis", colnames)]
preProc <- preProcess(training2[,-1],method = "pca", pcaComp = 7 )
trainPC <- predict(preProc, training2[,-1])
modelFit <- train(training2$diagnosis~ ., method="glm", data= trainPC)
testinfPC <- predict(preProc, testing2[,-1])
confusionMatrix(testing2$diagnosis, predict(modelFit, testinfPC))
preProcess(training2[,-1],method = "pca", thresh = 0.9)
# q5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
colnames <- names(training)
colnames <- grep("^IL",colnames, value=TRUE)
training2 <- training[,c("diagnosis",colnames)]
testing2 <- testing[,c("diagnosis", colnames)]
preProcess(training2[,-1],method = "pca", thresh = 0.8)
modelFitNonPCA <- train(diagnosis~ ., method="glm", data= training2)
confusionMatrix(testing2$diagnosis, predict(modelFitNonPCA, testing2[,-1]))
preProc <- preProcess(training2[,-1],method = "pca", pcaComp = 7 )
trainPC <- predict(preProc, training2[,-1])
modelFit <- train(training2$diagnosis~ ., method="glm", data= trainPC)
testinfPC <- predict(preProc, testing2[,-1])
confusionMatrix(testing2$diagnosis, predict(modelFit, testinfPC))