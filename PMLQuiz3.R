# q1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
#inTrain <- createDataPartition(y = segmentationOriginal$Case, p = 3/4, list = FALSE)
training <- segmentationOriginal[segmentationOriginal$Case == 'Train',]
testing <- segmentationOriginal[segmentationOriginal$Case == 'Test',]
set.seed(125)
modelFit <- train(Class ~., method="rpart", data = training)
print(modelFit$finalModel)
library(rattle)
fancyRpartPlot(modelFit$finalModel)
# q3
library(pgmm)
data(olive)
olive = olive[,-1]
modelFit <- train(Area ~., method="rpart", data = olive)
print(modelFit$finalModel)
newdata = as.data.frame(t(colMeans(olive)))
predict(modelFit, newdata)
# q4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)
modelFit <- train(chd~age+alcohol+obesity+tobacco+typea+ldl, method="glm", 
                  family="binomial", data = trainSA)
temp <- predict(modelFit, testSA)
missClass <- function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(testSA$chd, temp)
missClass(trainSA$chd, predict(modelFit, trainSA))
# q5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
head(vowel.train)
head(vowel.test)
vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)
set.seed(33833)
modelFit <- train(y~., method="rf", data = vowel.train, prox=TRUE)
modelFit3 <- train(y~., method="rf", data = vowel.train)
varImp(modelFit3)
