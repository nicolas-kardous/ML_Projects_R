#install.packages(c("caTools", "ROCR"))

library(dplyr)
library(ggplot2)
library(GGally)
library(caTools)
library(ROCR)

# Reading and splitting data
loans <- read.csv("loans.csv")

# Set a seed so we all get the same split
set.seed(144)


# sample.split splits the dataset smartly for binary outcomes:
# it keeps the same ratio defaulated/not defaulted in the train and test sets (first argument)
# SplitRatio = 0.7 means that we will put 70% of the data in the training, 30% in the testing
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)

# what is a split?
loans.train <- filter(loans, split == TRUE) # is split a variable in loans?
loans.test <- filter(loans, split == FALSE)


# How many loans have defaulted?
table(loans.train$not.fully.paid)

?ggscatmat
ggscatmat(loans.train, alpha = 0.8)


# Baseline model: predict that no one defaults
# Accuracy of baseline on training:
table(loans.train$not.fully.paid)
5596/(5596 + 1065)

# Accuracy of baseline on testing:
table(loans.test$not.fully.paid)
# (exercise) 

# Excercise
# What is the TPR and FPR of the baseline?



# Fit the logistic regression model
# Notice glm instead of lm
# https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/

mod <- glm(not.fully.paid ~ installment + log.annual.inc + fico + revol.bal + 
             inq.last.6mths + pub.rec, data=loans.train, family="binomial")
?glm
# Alternative:
# mod <- glm(not.fully.paid~., data=train, family="binomial")
# ~. dot says to use all other variables in the dataset as independent variables
# Be careful with this! Sometimes indicies, etc. can be present
# In general, consider this quote:
# "Programs are meant to be read by humans, and only incidentally for computers to execute." - Donald Knuth

summary(mod)
ggscatmat(loans.train, columns = 2:5, alpha = 0.8) # any correlations?


# Prediction for the example new observation
play.obs <- data.frame(installment=366, log.annual.inc=4.51, fico=682, revol.bal=7.53, inq.last.6mths=1, pub.rec=0)
predict(mod, newdata=play.obs, type="response")


# Predictions on the test set 
predTest = predict(mod, newdata=loans.test, type="response")
# predTest is the vector of probabilities as given by your model
# on the test set. Values between 0 and 1.
# Remember, P(Yi = 1) = 1/(1 + e^(-(b0 + b1*x1 + b2*x2 +...)) )

# If you don't include "type="response"", then predTest will
# return -(b0 + b1*x1 + b2*x2 +...).
# Values could range from -Inf to +Inf.

# Summary of model probabilities on the test set.
summary(predTest)


# Now, create the confusion matrix with threshold probability = 0.5.
table(loans.test$not.fully.paid, predTest > 0.5)
# What is the accuracy?
(2387+12)/nrow(loans.test)

# What is the True Positive Rate ?


# What is the False Positive rate ?




# Be careful about the definitions of FPR, TPR, recall, precision, sensitivity, Specificity etc.
# https://en.wikipedia.org/wiki/Sensitivity_and_specificity

# Now, try threshold probability = 0.2.

# What is the accuracy?
# What is the True Positive Rate ?
# What is the False Positive rate ?

