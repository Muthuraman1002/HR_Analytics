#Install required packages
install.packages("caret")
install.packages("kernlab")
install.packages("dplyr")
install.packages("readr")
install.packages("ggplot2")
install.packages("gridExtra")

library(kernlab)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(readr)
library(caret)

#Import the data
mnist_data <- read.csv("mnist_train.csv", header = FALSE)
names(mnist_data)[1] <- "Digit"

#Import test data set
mnist_data_test <- read.csv("mnist_test.csv", header = FALSE)
names(mnist_data_test)[1] <- "Digit"

#Business Understanding
#A classic problem in the field of pattern recognition is that of handwritten digit recognition.
#The goal is to develop a model that can correctly identify the digit (between 0-9) written in an image. 

#Data Understanding
#Pixel vlaues for the prdeicted digits(0-9) is given in the training data set which contains 10000 entires.
#Columns - 785
#Rows - 60000
ncol(mnist_data)
nrow(mnist_data)
View(mnist_data)


#Structutre of Data set
str(mnist_data)

#Exploring the data set
summary(mnist_data)

summary(as.factor(mnist_data$Digit))


#Data Preparation

#Check unique
nrow(unique(mnist_data))


View(mnist_data)
mnist_data$Digit <- factor(mnist_data$Digit)


#Sampling the data to build the model in order to increase the speed of SVM. 

set.seed(5)

#Create an empty data frame
mnist_sample <- data.frame(Date=as.Date(character()),
                           File=character(), 
                           User=character(), 
                           stringsAsFactors=FALSE)

#include 10% of data from each digit for sampling
for(i in c(0:9))
{
  mnist_data_i <-  subset(mnist_data, Digit == i)
  mnist_train_i <- mnist_data_i[sample(1:nrow(mnist_data_i), 0.08*nrow(mnist_data_i)),]
  mnist_sample <- rbind(mnist_sample,mnist_train_i)
}  

summary(mnist_sample)

nrow(mnist_sample)
#2996 Rows

ncol(mnist_sample)
#785 columns



#Scaling the test and train data

mnist_sample[,-1] <- mnist_sample[,-1]/255
mnist_data_test[,-1] <- mnist_data_test[,-1]/255


#Build a model with the mnist_sample data

#Constructing Model

#Using Linear Kernel
Model_linear <- ksvm(Digit~ ., data = mnist_sample, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, mnist_data_test)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,mnist_data_test$Digit)
# Accuracy    : 0.909 Kappa : 0.8988

#Using Poly Kernel
Model_poly <- ksvm(Digit~ ., data = mnist_sample, scale = FALSE, kernel = "polydot")
Eval_poly<- predict(Model_poly, mnist_data_test)

#confusion matrix - Poly Kernel
confusionMatrix(Eval_poly,mnist_data_test$Digit)
# Accuracy    : 0.909 Kappa : 0.8988

#Using RBF Kernel
Model_RBF <- ksvm(Digit~ ., data = mnist_sample, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, mnist_data_test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,mnist_data_test$Digit)
#Accuracy     : 0.9439  Kappa : 0.9376  

#Accuracy has increased from 90% to 94% in RBF Kernel function


#Registering clusters for Parallel operations to reduce the execution time
library(mlbench)
library(parallel)
library(doParallel)

cluster <- makeCluster(detectCores() - 2) # convention to leave 1 core for OS
registerDoParallel(cluster)



############   Hyperparameter tuning and Cross Validation #####################

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 2 implies Number of folds in CV.


trainControl <- trainControl(method="cv", number=3, allowParallel = TRUE)


# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
set.seed(7)
grid <- expand.grid(.sigma=c(0.005, 0.01, 0.015, 0.02), .C=c(1,2,3,4,5))


fit.svm <- train(Digit~., data=mnist_sample, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl, preProc = c("center", "scale"))

#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were sigma = 0.005 and C = 3.

print(fit.svm)

plot(fit.svm)

#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were sigma = 0.005 and C = 3.
#Accuracy - 0.8525510 (i.e) 85.25%   Kappa  : 0.8361111 


######################################################################
# Checking overfitting - Non-Linear - SVM
######################################################################

# Validating the model results on test data
evaluate_non_linear<- predict(fit.svm, mnist_data_test)
confusionMatrix(evaluate_non_linear, mnist_data_test$Digit)

#Accuracy : 87.39%  Kappa : 0.8598


