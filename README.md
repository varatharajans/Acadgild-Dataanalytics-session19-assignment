# Acadgild-Dataanalytics-session19-assignment
DATA ANALYTICS WITH R, EXCEL AND TABLEAU SESSION 19 ASSIGNMENT 
The assignment is performed in 3 methods and are give below

Note- Feature selection, model evaluation and assessment and SVM classification method


1. Use the below given data set 
DataSet 
2. Perform the below given activities: 
a. Create classification model using different classifiers 
b. Verify model goodness of fit 
c. Apply all the model validation techniques.

The Weight Lifting Exercises (WLE) dataset is used to investigate how well an activity is being performed. Six participants were performing one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl in five different fashions:
•	Class A - exactly according to the specification,
•	Class B - throwing the elbows to the front,
•	Class C - lifting the dumbbell only halfway,
•	Class D - lowering the dumbbell only halfway,
•	Class E - throwing the hips to the front.
Class A corresponds to the specified execution of the exercise, while the other 4 classes correspond to common mistakes
 The outcome of the given classification problem is given by the variable classe in the last column which is a factor variable with 5 levels “A”,“B”,“C”,“D”, and “E”. Each class is sufficiently represented in the training dataset. The outcome variable classe, however, is not included in the test dataset where it is replaced by a variable problem_id for identification purposes of the 20 test cases for the submission of the prediction results. Both datasets are consistent in their variable names (except for the last column with the outcome classe in the training dataset and problem_id in test dataset) and contain a considerable number of missing values marked as NA.
For feature extraction we only use the variables which are related to the raw measurements from the sensors located on the belt, forearm, arm, and dumbbell for the physical movement during the exercise. 
A quick verification shows that the reduced training (trainPredSet) and test (testPredSet) datasets are consistent in their predictor variable names and have no missing values (NAs).
In order to evaluate our prediction algorithm cross-validation is used. The training set is split into a cross-validation training set cvTrain (80%) and test set cvTest (20%). So we can train our model on the cvTrain dataset and test the accuracy of our prediction on the cvTest dataset in order to evaluate the influence of different training methods, predictor selections and predictor preprocessing methods. A high number of training examples (80%) is chosen to optimize for the training of the model.
When using the described prediction model to predict the 20 different test cases from the original test dataset testing we obtain 20 predictions as output .




setwd("C:/Users/Seshan/Desktop/sv R related/acadgild/assignments/session19 Assignment")
WLE<- read.csv("WLE.csv",header=T, na.strings=c("","NA"))
data<-WLE
View(data)
summary(data)
summary(data$classe)
names(data)
library(devtools)
install_github('adam-m-mcelhinney/helpRFunctions')
library(helpRFunctions)
training<-data[1:4000,]
testing<-data[4001:4024,]
dim(training)
summary(training)
str(training)
predictorIdx <- c(grep("^accel", names(training)), grep("^gyros", names(training)), 
                  grep("^magnet", names(training)), grep("^roll", names(training)), grep("^pitch",  names(training)), grep("^yaw", names(training)), grep("^total", names(training)))

trainPredSet <- training[, c(predictorIdx, 157)]

testPredSet <- testing[, c(predictorIdx, 157)]
length(predictorIdx)
sum(names(testing)[predictorIdx] != names(training)[predictorIdx])
#sum(is.na(trainPredSet)) color = trainPredSet$classe)
nearZeroVar(trainPredSet[, -7], saveMetric = TRUE)
qplot(x = trainPredSet[, "accel_belt_x"], y = trainPredSet[, "accel_arm_x"],color = trainPredSet$classe)

set.seed(125)
inTrain <- createDataPartition(y = trainPredSet$classe, p = 0.8, list = FALSE)
cvTrain <- trainPredSet[inTrain, ]
cvTest <- trainPredSet[-inTrain, ]
fitCtrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
set.seed(125)
modFit <- train(classe ~ ., data = cvTrain, method = "qda", preProcess = c("center","scale"), trControl = fitCtrl) 
                                                                       
print(modFit)
ptrain <- predict(modFit, newdata = cvTrain)
equalPredTrain <- (ptrain == cvTrain$classe)
print(sum(equalPredTrain)/length(equalPredTrain))
confusionMatrix(data = ptrain, reference = cvTrain$classe)
ptest <- predict(modFit, newdata = cvTest)
equalPredTest <- (ptest == cvTest$classe)
print(sum(equalPredTest)/length(equalPredTest))
testPrediction <- predict(modFit, newdata = testing)
print(rbind(testing[1:20, 157], as.character(testPrediction)))


                               model_evaluation_and_assessment_excercise.R
                                            Seshan
Sat Aug 11 14:18:47 2018
setwd("C:/Users/Seshan/Desktop/sv R related/acadgild/assignments/session19 Assignment")
WLE<- read.csv("WLE.csv",header=T, na.strings=c("","NA"))
data<-WLE
View(data)
training<-data[1:4010,]
testing<-data[4011:4024,]
names(training)
library(ResourceSelection)
## ResourceSelection 0.3-2   2017-02-28
hoslem.test(training$classe, fitted(fit))
##  Hosmer and Lemeshow goodness of fit (GOF) test
## data:  training$classe, fitted(fit)
## X-squared = 4010, df = 8, p-value < 2.2e-16
#plot the fitted model
plot(fit$fitted.values)
pred <- predict(fit,newdata = testing,type = 'response')
ifelse(type == : prediction from a rank-deficient fit may be misleading
library(caret)
#with default prob cut 0.50
testing$pred_classe <- ifelse(pred<0.7,'yes','no')
table(testing$pred_classe,testing$classe)
round(table(training$classe)/nrow(training),2)*100
round(table(testing$classe)/nrow(testing),2)*100
#create confusion matrix 
confusionMatrix(testing$classe,testing$classe)
# define training control
#train_control<- trainControl(method="cv", number=10)

# train the model 
#model<- train(classe~.,data=training, trControl=train_control, method="glm")
 append predictions
pred<- cbind(testing,predictions)
# summarize results
confusion Matrix<- confusion Matrix(pred$predictions,pred$pred_classe)
Confusion Matrix and Statistics


                                                   WLE_SVM_classification.R
                                                            Seshan
Thu Aug 09 19:42:34 2018
setwd("C:/Users/Seshan/Desktop/sv R related/acadgild/assignments/session17")
library(readr)
WLE<- read.csv("WLE.csv",header=T, na.strings=c("","NA"))
View(WLE)
dim(WLE)
## [1] 4024  158
library(kernlab)
WLE_train<-WLE[1:3950,]
WLE_test<-WLE[3951:4024,]
names(WLE)
WLE_classifier<-ksvm(classe~.,data=WLE_train, kernel="vanilladot")
##  Setting default kernel parameters
WLE_classifier
## Support Vector Machine object of class "ksvm" 
## SV type: C-svc  (classification) 
## Linear (vanilla) kernel function. 
WLE_prediction<-predict(WLE_classifier,WLE_test)
head(WLE_prediction)
table(WLE_prediction,WLE_test$classe)
##               
## WLE_prediction  A  B  C  D  E
##              A  0  0 74  0  0
##              B  0  0  0  0  0
##              C  0  0  0  0  0
##              D  0  0  0  0  0
##              E  0  0  0  0  0
WLE_prediction
##  [1] A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A
## [36] A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A
## [71] A A A A
## Levels: A B C D E
Agreement<-WLE_prediction ==WLE_test$classe
prop.table(table(Agreement))
## Agreement
## FALSE 
##     1
set.seed(12345)
WLE_classifier_rbf<-ksvm(classe~.,data=WLE_train, kernel ="rbfdot")
## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.
WLE_prediction_rbf<-predict(WLE_classifier_rbf,WLE_test)
Agreement_rbf<-WLE_prediction_rbf==WLE_test$classe
table(Agreement_rbf)
## Agreement_rbf
## FALSE 
##    74
prop.table(table(Agreement_rbf))
## Agreement_rbf
## FALSE 
##     1





