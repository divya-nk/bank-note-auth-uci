#Author: Muthudivya NK
#Date: April 15, 2017
#Purpose: Logistic Regression with one predictor

x <- read.table("C:/Users/Muthudivya NK/Documents/3. Data Science/UCI_Project/Training Data.csv", header = TRUE, sep = ",")
View(x)
attach(x)
hist(X1)
hist(X2)
hist(X3)
hist(X4)
hist(X5)
hist(X6)
hist(X7)
hist(X8)

x1a <- lm(Y1 ~ X1)
?lm()
summary(x1a)
predict(x1a, data.frame(lstat=c(5,10,15)), interval =  "confidence")
?predict()
