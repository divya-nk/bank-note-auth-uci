rm(list = ls())
library(boot)
# Author: Muthudivya NK
# Date: April 21st, 2017
# Purpose: Determine the authenticity of banknotes

set.seed(54)
#### Reading data into R ####

mydata <- read.csv("C:/Users/Muthudivya NK/OneDrive - Clemson University/3. Data Science/UCI_Project/BankNotes.csv", header = TRUE, sep = ",")
dim(mydata)
names(mydata)
attach(mydata)
summary(mydata)

#### Performing EDA #####

par(mfrow=c(2,2)) # to split the plot space
hist(Curtosis)
hist(Entropy)
hist(Skewness)
hist(Variance)

c <- cor(mydata, method = "pearson") # Finding Pearson's Coeff Correlation among the variables
View(c)


#### Best Subset Selection ####

require(leaps)
require(MASS)
bestsub = regsubsets(Class ~   Skewness + Variance + Curtosis + Entropy, data = mydata, nvmax = 10)
summary(bestsub)

#### Splitting data into training and test data ####

N <- nrow(mydata) # counting the total number of rows
which.train <- sample(N, N/5) # sampling the no of rows in 80:20 ratio
mydata.train <- mydata[-which.train,] 
mydata.test <- mydata[which.train,]

#### Training the model - Logistic Regression with 4 Predictors ####

train.m <- glm(Class ~  Variance + Skewness + Curtosis + Entropy, data = mydata.train)
summary(train.m)

train.probs <-predict(train.m, (mydata.train[,1:4]), type = "response")
train.class <- rep(0, 1098)
train.class[train.probs>0.5] <- 1

error.train <- mean(train.class != mydata.train$Class) # gives the error rate on training data

#### Validation Set Approach ####

newdf <- mydata.test[,1:4] # new dataframe (test data) without output column
pred.probs <- predict(train.m, newdf, type = "response") # fitting the trained model to testing data, and calculating the predicted probability of each observation

pred.class <- rep(0, 274) # creating a list of 274 rows with value 0
pred.class[pred.probs > 0.5] <- 1 # Changing observations with prob values > 0.5 to 1

#write.table(pred.class1, "C:/Users/Muthudivya NK/OneDrive - Clemson University/Documents/3. Data Science/UCI_Project/Predclass.csv", sep = ",")
#write.table(mydata.test, "C:/Users/Muthudivya NK/OneDrive - Clemson University/Documents/3. Data Science/UCI_Project/mydatatest.csv", sep = ",")

#### Miscalculation Rate ####

error.rate <- mean(pred.class != mydata.test$Class)

#### Confusion matrix and Accuracy Calculation ####

confusion.matrix <- table(mydata.test$Class, pred.class) 
print(addmargins(confusion.matrix))
print(c("accuracy", (confusion.matrix[1,1] + confusion.matrix[2,2]) / 274 )) # Calculating Accuracy
