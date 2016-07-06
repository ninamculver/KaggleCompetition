####Importing the Data###
library(jsonlite)

train = fromJSON('~/Fall 2015/Data Mining I/Final Project/train.json')
test = fromJSON('~/Fall 2015/Data Mining I/Final Project/test.json')

str(train)


### Making the Data Useable###
library(dplyr)
library(tm)
install.packages("SnowballC")

ingredients = Corpus(VectorSource(train$ingredients))
ingredients = tm_map(ingredients, stemDocument)
ingredients

ingredientsDTM = DocumentTermMatrix(ingredients)
ingredientsDTM

sparse = removeSparseTerms(ingredientsDTM, 0.99)
sparse

ingredientsDTM = as.data.frame(as.matrix(sparse))
ingredientsDTM$cuisine = as.factor(train$cuisine)

trainStuff=sample(nrow(ingredientsDTM),nrow(ingredientsDTM)*.7)
training = ingredientsDTM[trainStuff,]
testing = ingredientsDTM[-trainStuff,]

###Decision Tree###

library(rpart)
modelTree = rpart(cuisine ~ ., data = training, method = "class")

library(rpart.plot)
library(RColorBrewer)
library(rattle)
fancyRpartPlot(modelTree)

confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}

predTree = predict(modelTree,newdata=testing,type="class")
confmatrix(testing$cuisine, predTree) #.43 accuracy

###k-Nearest Neighbors###
library(class)
library(kknn)
#predKNN = knn(train = training, test = testing, cl = training$cuisine, k = 3)
#confmatrix(testing$cuisine, predKNN) #Doesn't work because there are words in it...

fitKknn = train.kknn(cuisine ~ ., data= training, kmax = 15, 
                       kernel =c("rectangular","triangular", "epanechnikov",  "biweight",
                                 "triweight","cos","inv", "gaussian" ,"optimal"), 
                       distance = 2)

predKknn=(kknn(cuisine~.,train=training,test=testing,
             k=15,kernel="triangular",distance=2))$fitted.values #try optimal and 11

confmatrix(testing$cuisine,predKknn) #.64 accuracy

predKknn2=(kknn(cuisine~.,train=training,test=testing,
               k=11,kernel="optimal",distance=2))$fitted.values 

confmatrix(testing$cuisine,predKknn2) #.62 accuracy

###Naive Bayes###
library(e1071)
modelBayes = naiveBayes(cuisine~., data = training)
predBayes = predict(modelBayes, newdata = testing)
confmatrix(testing$cuisine, predBayes) #.25 accuracy
#try tree augmented naive bayes#

###Neural Networks###
library(nnet)
modelNnet=nnet(cuisine~.,data=training,size=20,linout=TRUE) 
 #I don't know how to set size and linout.
predNnet = predict(modelNnet, newdata = testing, type="class")
confmatrix(testing$cuisine, predNnet) #.01 accuracy

###Support Vector Machine###
modelSVM = svm(cuisine~.,data= training,kernel="linear")
predSVML=predict(modelSVM,newdata=testing)
confmatrix(predSVML,testing$cuisine) #0.705 accuracy


modelSVMP = svm(cuisine~.,data= training,kernel="polynomial")
predSVMP=predict(modelSVMP,newdata=testing)
confmatrix(predSVMP,testing$cuisine) #0.622 accuracy

modelSVMR = svm(cuisine~.,data= training,kernel="radial")
predSVMR=predict(modelSVMR,newdata=testing) 
confmatrix(predSVMR,testing$cuisine) #0.734 accuracy

modelSVMS = svm(cuisine~.,data= training,kernel="sigmoid")
predSVMS=predict(modelSVMS,newdata=testing)
confmatrix(predSVMS,testing$cuisine) #0.693

###Random Forests###
install.packages("randomForest")
library(randomForest)
modelForest = randomForest(cuisine~.,data=training)

#This won't work D=
predForest=predict(modelForest,newdata=training)
confmatrix(testing$cuisine,predForest)

###Bagging###
install.packages("adabag")
library(adabag)
modelBag=bagging(cuisine~.,data=training) #this won't work either
predBag=predict(modelBag,newdata=testing,type="class")
predBag

###Boosting###
modelBoost=boosting(cuisine~.,data=training)
 predBoost=predict(modelBoost,newdata=testing,type="class")
predBoost #accuracy =  0.4075253
