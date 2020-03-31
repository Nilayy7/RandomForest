install.packages("randomForest")
install.packages("MASS")
install.packages("caret")

library(MASS)
library(randomForest)
library(caret)

View(Company_Data)
hist(Company_Data$Sales, col = c("blue","red", "green","violet","yellow"),main = "Sales of Company Data")
table(Company_Data$US)

hsales = ifelse(Company_Data$Sales < 9,"No","Yes") #If sales less than 8 then No else Yes
Companydata =data.frame(Company_Data[2:11],hsales)
View(Companydata)
table(Companydata$hsales)

#Splitting Data into train and test samples
comp_train <- Companydata[1:280,]
comp_test <- Companydata[281:400,]

#Creating Random Forests with no. of trees = 500
rf <-randomForest(hsales~.,data = comp_train)
print(rf)

#Predicting with train data
pred <- predict(rf,comp_train)
head(pred)

#Creating a Confusion Matrix on train data
confusionMatrix(pred,comp_train$hsales)
#after applying Confusion Matrix on training data we get 100% accuracy

#Predicting with Test data
pred1 <- predict(rf,comp_test)
head(pred1)
#Creating a Confusion Matrix on train data
confusionMatrix(pred1,comp_test$hsales)
#after applying Confusion Matrix on test data we get 80% accuracy

#To Find Error Rate in RandomForest
plot(rf)

#Creating a random forest with no. of trees = 400
rf1 <- randomForest(hsales~.,data = comp_train,ntree = 400,mtry = 3, importance = TRUE,
                    proximity = TRUE)
rf1
#Find the error rate
plot(rf1)

#Predicting & Confusion Matrix With Train Data
pred2 <- predict(rf1,comp_train)
confusionMatrix(pred2,comp_train$hsales)

#Predicting & Confusion Matrix with Test Data
pred3 <-predict(rf1,comp_test)
confusionMatrix(pred3,comp_test$hsales)

# Variable Importance 
varImpPlot(rf1,sort = TRUE,main = "Top 5 Variables",n.var = 5)

#All the Quantitative Values
importance(rf1)

varUsed(rf1) #Predictor variables used

#Partial Dependence Plot
partialPlot(rf1,comp_train,US,"Yes")
partialPlot(rf1,comp_test,US,"Yes")

partialPlot(rf1,comp_train,Age,"Yes")
partialPlot(rf1,comp_test,Age,"Yes")

partialPlot(rf1,comp_train,Price,"Yes")
partialPlot(rf1,comp_test,Price,"Yes")

# Multi Dimension scaling plot of proximity Matrix
MDSplot(rf1, Companydata$hsales)

#Plotting No. of nodes 
hist(treesize(rf),main = "Number of Nodes",col = 'yellow')
hist(treesize(rf1),main = "Number of Nodes",col = 'red')

#This histogram tell us majority of trees have nodes more than 60
