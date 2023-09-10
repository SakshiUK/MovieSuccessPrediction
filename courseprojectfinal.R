#Load the required libraries:
library(rpart)
library(rpart.plot)
library(performanceEstimation)
library(ROSE)
library(smotefamily)
library(caret)
library(e1071)
library(randomForest)

#Load the dataset:
f <- read.csv("movie_success_rate.csv")

#clearing the data
colSums(is.na(f))
f1<-na.omit(f)
colSums(is.na(f1))

#Remove the first 7 columns which are of no use to us for the classification process
f1<-f1[,-1]
f1<-f1[,-1]
f1<-f1[,-1]
f1<-f1[,-1]
f1<-f1[,-1]
f1<-f1[,-1]
f1<-f1[,-1]

#Categorize data or number into 'yes' and 'no'
f1$Success=as.factor((f1$Success))
f1$Action=as.factor((f1$Action))
f1$Adventure=as.factor((f1$Adventure))
f1$Aniimation=as.factor((f1$Aniimation))
f1$Biography=as.factor((f1$Biography))
f1$Comedy=as.factor((f1$Comedy))
f1$Crime=as.factor((f1$Crime))
f1$Drama=as.factor((f1$Drama))
f1$Family=as.factor((f1$Family))
f1$Fantasy=as.factor((f1$Fantasy))
f1$History=as.factor((f1$History))
f1$Music=as.factor((f1$Music))
f1$Musical=as.factor((f1$Musical))
f1$Mystery=as.factor((f1$Mystery))
f1$Romance=as.factor((f1$Romance))
f1$Sci.Fi=as.factor((f1$Sci.Fi))
f1$Sport=as.factor((f1$Sport))
f1$Thriller=as.factor((f1$Thriller))
f1$War=as.factor((f1$War))
f1$Western=as.factor((f1$Western))
f1$Horror=as.factor((f1$Horror))

#Split the dataset into training and testing sets:
set.seed(123)
train_index <- sample(1:nrow(f1),round(nrow(f1)*0.7),replace = FALSE)
train_data <- f1[train_index, ]
test_data <- f1[-train_index, ]
cat("THE OLD DATA VALUES ARE :\n")
c<-nrow(train_data[train_data$Success==0,])
d<-nrow(train_data[train_data$Success==1,])
print(c)
print(d)
barplot(table(train_data$Success))

#Oversampling for the dataset is imbalanced
over=ovun.sample(Success~.,data=train_data,method = "over")$data
barplot(table(over$Success))
cat("THE NEW DATA VALUES ARE :\n")
r<-nrow(over[over$Success==0,])
f2<-nrow(over[over$Success==1,])
print(r)
print(f2)

#Cross-Validation using k-folds where k=10 for avoiding overfitting of the models
set.seed(123)
ctrl <- trainControl(method = "cv", number = 10)

#Decision tree model.
dt_model<-train(Success~.,data=over,method="rpart",trControl = ctrl,tuneLength=30)
#Plot the Decision Tree
rpart.plot(dt_model$finalModel)
dt_pred<-predict(dt_model,test_data)
confusion_matrix1<-confusionMatrix(dt_pred,test_data$Success)
print("Decision Tree Model Predictions:")
print(confusion_matrix1)

#SVM model
svm_model<-train(Success~.,data=over,method="svmRadial",trControl=ctrl)
svm_pred<-predict(svm_model,test_data)
confusion_matrix2<-confusionMatrix(svm_pred,test_data$Success)
print("Suport Vector Model Predictions:")
print(confusion_matrix2)

#random forest model
rf_model<-train(Success~.,data=over,method="rf",trControl=ctrl,ntree = 100)
rf_pred<-predict(rf_model,test_data)
confusion_matrix3<-confusionMatrix(rf_pred,test_data$Success)
print("Random Forest Model Predictions:")
print(confusion_matrix3)


#logistic regression model
glm_model<-train(Success~.,data=over, method="glm",family="binomial",trControl=ctrl)
glm_pred<-predict(glm_model,test_data)
confusion_matrix4<-confusionMatrix(glm_pred,test_data$Success)
print("Logistic Regression Model")
print(confusion_matrix4)

#KNN model
knn_model<-train(Success~.,data=over,method="knn",trControl=ctrl)
knn_pred<-predict(knn_model,test_data)
confusion_matrix5<-confusionMatrix(knn_pred,test_data$Success)
print("K-Nearest Neighbors: ")
print(confusion_matrix5)
