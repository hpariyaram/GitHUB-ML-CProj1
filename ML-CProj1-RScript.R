library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(e1071)
set.seed(1234)
setwd("C:/Bahul/Coursera/JHU-DataScience/ML/ML-CProj1")
TrainingSet <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!", ""))
TestingSet <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!", ""))
TrainingSet<-TrainingSet[,colSums(is.na(TrainingSet)) == 0]
TestingSet <-TestingSet[,colSums(is.na(TestingSet)) == 0]
TrainingSet   <-TrainingSet[,-c(1:7)]
TestingSet <-TestingSet[,-c(1:7)]
dim(TrainingSet)
dim(TestingSet)
SubSamples <- createDataPartition(y=TrainingSet$classe, p=0.75, list=FALSE)
SubTraining <- TrainingSet[SubSamples, ] 
SubTesting <- TrainingSet[-SubSamples, ]

plot(SubTraining$classe, col="green", main="Classe variable Plot", xlab="Classe Values", ylab="Frequency")

Model1 <- rpart(classe ~ ., data=SubTraining, method="class")
Prediction1 <- predict(Model1, SubTesting, type = "class")

rpart.plot(Model1, main="Decision Tree", extra=102, under=TRUE, faclen=0)

confusionMatrix(Prediction1, SubTesting$classe)

Model2 <- randomForest(classe ~. , data=SubTraining, method="class")
Prediction2 <- predict(Model2, SubTesting, type = "class")

confusionMatrix(Prediction2, SubTesting$classe)

PredictFinal <- predict(Model2, TestingSet, type="class")
PredictFinal

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(PredictFinal)



