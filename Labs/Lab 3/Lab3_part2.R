# IEOR 242 Discussion 3 part 2
# This Lab is similar to Homework 2.

# Session -> Setting Working Directory -> To Source File Location
# Make sure you have all the packages installed
# install.packages(c("ROCR", "caTools", "MASS"))
setwd("/Users/lucas.spangher/Documents/Berkeley Files/Class/Fall 2019/IEOR242/Lab/Lab3")

library(dplyr)
library(ggplot2)
library(GGally)
library(caTools)
library(ROCR)
library(MASS)

churn <- read.csv("customerchurn.csv")

set.seed(144)

split <- sample.split(churn$Churn, SplitRatio = 0.7)

train <- filter(churn, split == TRUE)
test <- filter(churn, split == FALSE)

str(train) # any issues with categorical x variables?

LogModel <- glm(Churn ~ MonthlyCharges + SeniorCitizen + PaymentMethod + InternetService + tenure + 
                  Contract, data=train, family="binomial")
summary(LogModel)

## now take the estimate for SeniorCitizen and transform it into an intuition of how much p will increase 



# Let's remove MonthlyCharges
LogModel2 <- glm(Churn ~ SeniorCitizen + PaymentMethod + InternetService + tenure + 
                  Contract, data=train, family="binomial")
summary(LogModel2)

# Let's remove PaymentMethod Credit Card and PaymentMethodMailed check. How do we do this? Create a new feature. 

train_2 <- train

train$ElectronicCheck <- as.numeric(train$PaymentMethod == "Electronic check")
test$ElectronicCheck <- as.numeric(test$PaymentMethod == "Electronic check")

# Do it in other way:
train_2$ElectricCheck <- 0
train_2[train_2$PaymentMethod == "Electronic check",]$ElectricCheck <- 1

#
LogModel3 <- glm(Churn ~ SeniorCitizen + ElectronicCheck + InternetService + tenure + 
                   Contract, data=train, family="binomial")
summary(LogModel3)

predTestLog <- predict(LogModel, newdata=test, type="response") # type on default gives log-odds, "response" gives predicted probabilities


table(test$Churn, predTestLog > 1/3)


# Using LDA:

LdaModel <- lda(Churn ~ MonthlyCharges + SeniorCitizen + PaymentMethod + InternetService + tenure + 
                  Contract, data=train)

predTestLDA <- predict(LdaModel, newdata=test) #reference https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/predict.lda.html
predTestLDA_probs <- predTestLDA$posterior[,2]

table(test$Churn, predTestLDA_probs > 1/3)

# ROC curves
rocr.log.pred <- prediction(predTestLog, test$Churn)
logPerformance <- performance(rocr.log.pred, "tpr", "fpr")
plot(logPerformance, colorize = TRUE)
abline(0, 1)
as.numeric(performance(rocr.log.pred, "auc")@y.values)
# reference :https://rocr.bioinf.mpi-sb.mpg.de/ROCR.pdf

rocr.lda.pred <- prediction(predTestLDA_probs, test$Churn)
ldaPerformance <- performance(rocr.lda.pred, "tpr", "fpr")
plot(ldaPerformance, colorize = TRUE)
abline(0, 1)
as.numeric(performance(rocr.lda.pred, "auc")@y.values)

plot(logPerformance, col="blue")
plot(ldaPerformance, col="red", add=TRUE)
abline(0,1)

# For fun, let's use LDA for a multiclass problem
# Predict payment type based on monthy charges, senior citizen, Internet service, tenure, contract

lda_fun_mod <- lda(PaymentMethod ~ MonthlyCharges + SeniorCitizen + InternetService + 
                     tenure + Contract, data = train)

pred_test_lda_fun <- predict(lda_fun_mod, newdata = test)
pred_test_lda_fun_class <- pred_test_lda_fun$class
pred_test_lda_fun_probs <- pred_test_lda_fun$posterior

tab <- table(test$PaymentMethod, pred_test_lda_fun_class)
accuracy <- sum(diag(tab))/ sum(tab)
