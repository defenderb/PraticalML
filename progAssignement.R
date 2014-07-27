traindata <- read.csv("./data/pml-training.csv", na.strings=c(NA,"","NA"))[,-1]
newdata <- read.csv("./data/pml-testing.csv",na.strings=c(NA,"","NA"))[,-1]
#modelFit <- train(classe~., method ="rpart",data = train)
# usefulCols <- colSums(!is.na(traindata))>3/4*nrow(traindata)
# traindata <- traindata[, usefulCols]

usefulCols <- (colSums(!(traindata == '')) > 0.9*nrow(traindata)) &
  (colSums(!is.na(traindata))>3/4*nrow(traindata))
traindata <- traindata[, usefulCols]
newdata <- newdata[,usefulCols]
traindata <-traindata[7:ncol(traindata)]
newdata <-newdata[7:ncol(newdata)]
#traindata <- traindata[complete.cases(traindata),]
#newdata <- newdata[complete.cases(newdata),]

exCol <- ncol(traindata)
library(caret)
inTrain <- createDataPartition(traindata$classe, p=0.5, list = FALSE)
train <- traindata[inTrain,]
test <- traindata[-inTrain,]
preObj <- preProcess(train[,-exCol], method="knnImpute")
preObj <- preProcess(train[,-exCol], method="pca", pcaComp = 20)
trainPC <- predict(preObj, train[,-exCol])  
testPC  <- predict(preObj, test[,-exCol])
newPC  <- predict(preObj, newdata[,-exCol])

system.time(model2 <- train(train$classe ~ ., data = trainPC, method = "rf", prox = TRUE, 
      trControl = trainControl(method = "cv", number = 4, allowParallel = TRUE)))
system.time(modelT <- train(classe ~ ., data = train, method = "rf", prox=TRUE,
                trControl = trainControl(method = "cv", number = 3, allowParallel = TRUE)))
modelT <- train(classe ~., data = train[complete.cases(train),], 
                method = "glm", family="binomial" )
modelT <- train(classe ~., data = train[complete.cases(train),], method="svmRadial")
modelT <- train(train$classe[1:300] ~., data = trainPC[1:300,], method="svmLinear")
model1 <- train(train$classe ~., data =trainPC, method = "rpart")
confusionMatrix(train$classe, predict(modelT, train))
confusionMatrix(test$classe, predict(modelT, test))

confusionMatrix(train$classe, predict(model2, trainPC))
confusionMatrix(test$classe, predict(model2, testPC))
modelT <- train(classe ~., data = (train[complete.cases(train),])[1:300,], method = "rf", prox = TRUE)
              trControl =trainControl(method = "none"))
modelq <- train(y~., data =x, method = "rf", prox=TRUE)
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
prediction  <- predict(modelT, newdata)
pml_write_files(prediction)
