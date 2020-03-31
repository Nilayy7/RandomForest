#Loading The Packages
install.packages('caret',dependencies=TRUE)
install.packages('randomForest')
library(caret)
library(randomForest)
library(MASS)
hist(Fraud_check$Taxable.Income,main = "Sales of Company",col = 'blue')

## if Taxable Income is less than or equal to 30000 then Risky else Good
rg = ifelse(Fraud_check$Taxable.Income <= 30000,"Risky","Good")

df = data.frame(Fraud_check,rg)

Fraudcheck =df[,c(1:7)]
table(Fraudcheck$rg) #We get 476 Good Customers and 124 Risky Customers

#Partioning Data into Traininga and Test
fraud_train <- Fraudcheck[1:420,]
fraud_test <- Fraudcheck[421:600,]

#Creation Of Random Forest
model <- randomForest(rg~.,data = fraud_train)
plot(model) #To Calculate error

#Prediction and Confusuion Matrix on training data
pred <- predict(model,data=fraud_train)
confusionMatrix(pred,fraud_train$rg)

# Prediction with test data 
pred1 <- predict(model, fraud_test)
confusionMatrix(pred1, fraud_test$rg) # 100 % accuracy on test data 

#Creating another random forest
model1 <- randomForest(rg~., data=fraud_train, ntree = 600, mtry = 2, importance = TRUE,
                    proximity = TRUE)
model1
plot(model1)
#Prediction and Confusuion Matrix on training data
pred2 <- predict(model1,data=fraud_train)
confusionMatrix(pred2,fraud_train$rg) #Around 99% of C.I.

# Prediction with test data 
pred3 <- predict(model1, fraud_test)
confusionMatrix(pred3, fraud_test$rg) #100% accuracy here as well,Around 97% of C.I.

#Number of nodes of trees

hist(treesize(model1))
#This histogram tell us majority of trees have nodes more than 250

#Variable Importance
varImp(model1)
varImpPlot(model1,n.var = 5,main = "Top 5 Variables")

#Quantitative Values
importance(model1)
importance(model)

#Partial Dependence Plot
partialPlot(model1  ,fraud_train,Taxable.Income,"Good")
partialPlot(model1  ,fraud_train,Taxable.Income,"Risky")

partialPlot(model1  ,fraud_test,Taxable.Income,"Good")
partialPlot(model1  ,fraud_test,Taxable.Income,"Risky")

# Multi Dimension scaling plot of proximity Matrix
MDSplot(model1, Fraudcheck$rg)

#Creating a third random forest
model2 <- randomForest(rg~., data=fraud_train, ntree = 300, mtry = 2, importance = TRUE,
                       proximity = TRUE)
model2
plot(model2)

#Prediction and Confusuion Matrix on training data
pred4 <- predict(model2,data=fraud_train)
confusionMatrix(pred4,fraud_train$rg) #Around 99% of C.I.

# Prediction with test data 
pred5 <- predict(model2, fraud_test)
confusionMatrix(pred5, fraud_test$rg)

#Number of nodes of trees

hist(treesize(model2))
#This histogram tell us majority of trees have nodes around 140

#Variable Importance
varImp(model2)
varImpPlot(model2,n.var = 5,main = "Top 5 Variables")

#Quantitative Values
importance(model2)
importance(model)

#Partial Dependence Plot
partialPlot(model2  ,fraud_train,Taxable.Income,"Good")
partialPlot(model2  ,fraud_train,Taxable.Income,"Risky")

partialPlot(model2  ,fraud_test,Taxable.Income,"Good")
partialPlot(model2  ,fraud_test,Taxable.Income,"Risky")

# Multi Dimension scaling plot of proximity Matrix
MDSplot(model2, Fraudcheck$rg)
