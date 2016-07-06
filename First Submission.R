ingredients = Corpus(VectorSource(train$ingredients))
ingredients = tm_map(ingredients, stemDocument)
ingredients

ingredientsDTM = DocumentTermMatrix(ingredients)
ingredientsDTM

sparse = removeSparseTerms(ingredientsDTM, 0.99)
sparse

ingredientsDTM = as.data.frame(as.matrix(sparse))
ingredientsDTM$cuisine = as.factor(train$cuisine)

ingredientsTest = Corpus(VectorSource(test$ingredients))
ingredientsTest = tm_map(ingredientsTest, stemDocument)
ingredientsTest

ingredientsTestDTM = DocumentTermMatrix(ingredientsTest)
ingredientsTestDTM

sparseTest = removeSparseTerms(ingredientsTestDTM, 0.99)
sparseTest

ingredientsTestDTM = as.data.frame(as.matrix(sparseTest))
trainModel = svm(cuisine~.,data= ingredientsDTM,kernel="radial")

##Messing around with things to see if I can get golden and activ gone.##

submissiontrain = ingredientsDTM[,-94]
submissiontest = ingredientsTestDTM[,-1]
submissionModel = svm(cuisine~.,data= submissiontrain,kernel="radial")

submissionPred=predict(submissionModel,newdata=submissiontest)

mysolution = data.frame(ID = test$id, Cuisine = submissionPred)


