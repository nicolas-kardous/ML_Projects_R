library(rpart)
#install.packages()
library(rpart.plot)
library(caret)
library(randomForest)
library(gbm)
library(caTools)
library(dplyr)
library(ggplot2)

## warmup programming exercise: 

## 1. Use the rnorm() function to generate 10 samples from a N(0,1) distribution and plot it using 
## histogram in ggplot() (look up how to make a histogram)

a = rnorm(n=100, mean=0,sd = 1)

a1 = c(2,3,4,4,4,4,4,4,4,4,4,4)

ggplot()+
  geom_histogram(mapping = aes(x=a1),  fill='red', alpha=0.5)
  ggtitle("My cool histogram")+
  geom_point(mapping = aes(x=a,y=a))

## 2. Use the dgamma() function to compute the probability density with a vector from 1 to 10 
## (use the c() function) with shape .5 and scale .5. Plot it using 
# ggplot() + geom_line(aes(x= (vector of 1 to 10) , y = (output of dgamma)))

# We do bagging for the CART model, so we add diversity in our model, this protects our data from outliers, 
  # Boosting: Repeat the same base model method but with different weights:
  #  1: From model i predict data,  without replacement
  # 2: Weights wp, predict unvertainty
  # 3: Train model on Dwpm1 (Data weighted on model 1)
  
# Bboosting is different to bagging bbecause it samples  without  replacement  and adds different weights

# convenience function, you're welcome :) 
OSR2 <- function(predictions, test, train) {
  SSE <- sum((test - predictions)^2)
  SST <- sum((test - mean(train))^2)
  r2 <- 1 - SSE/SST
  return(r2)
}

# You can write your function in another r script file, and use "source()" to use that function. 


# begin analysis

ctr = read.csv("CTR.csv")

set.seed(144) # Make sample look different, 
train.ids = sample(nrow(ctr), 0.7*nrow(ctr))
train.ctr = ctr[train.ids,]
test.ctr = ctr[-train.ids,]

# Let's try basic CART first
set.seed(33)
#?train()
##To change the candidate values of the tuning parameter,
##either of the tuneLength or tuneGrid arguments can be used. 
##The tuneGrid argument is used when specific values are desired. 
##A data frame is used where each row is a tuning parameter setting and each column is a tuning parameter.
##trControl	:A list of values that define how this function acts. 
##See http://topepo.github.io/caret/using-your-own-model-in-train.html. 
train.cart = train(CTR ~ .,
                   data = train.ctr,
                   method = "rpart", # recursive partitioning
                   tuneGrid = data.frame(cp=seq(0, 0.1, 0.005)),
                   trControl = trainControl(method="cv", number=5),
                   metric = "RMSE")
train.cart$results ## results : A data frame the training error rate and values of the tuning parameters.
train.cart
best.cart = train.cart$finalModel ##finalModel: A fit object using the best parameters

test.ctr.mm = as.data.frame(model.matrix(CTR ~ . + 0, data=test.ctr)) 
## model.matrix() creates a design (or model) matrix, 
##e.g., by expanding factors to a set of dummy variables (depending on the contrasts) and expanding interactions similarly.
## http://genomicsclass.github.io/book/pages/expressing_design_formula.html

pred.best.cart = predict(best.cart, newdata = test.ctr.mm)


# Now let's try random forests
# First, basic training of a RF, can take a minute
set.seed(144)
mod.rf <- randomForest(CTR ~ ., data = train.ctr, mtry = 5, nodesize = 5, ntree = 500)

pred.rf <- predict(mod.rf, newdata = test.ctr) # just to illustrate

importance(mod.rf) # How predictive the variable is
#IncNodePurity :Mean decrease in node impurity. (At each split, you can calculate how much this split reduces node impurity. Then summed over all splits for that variable, over all trees.

# bagging can be done just by setting mtry = p = 17
# need to use model matrix because of how factor x variables are handled
# "randomForest" doesn't break categorical variables for us, it treat them as one numeric column.
train.ctr.mm = as.data.frame(model.matrix(CTR ~ . + 0, data = train.ctr)) 
set.seed(3432)
mod.bag <- randomForest(x = train.ctr.mm, y = train.ctr$CTR, mtry = 17, nodesize = 5, ntree = 500)
pred.bag <- predict(mod.bag, newdata = test.ctr.mm)

# syntax is very similar to CART training
# Warning: this took about 20 mins on my computer


# https://topepo.github.io/caret/train-models-by-tag.html

set.seed(99)
train.rf <- train(CTR ~ .,
                  data = train.ctr,
                  method = "rf", # random forest
                  tuneGrid = data.frame(mtry=1:16),
                  trControl = trainControl(method="cv", number=5, verboseIter = TRUE),
                  metric = "RMSE")
# RMSE or Rsquared doesn't matter actually -- both will be generated for regression problems
train.rf$results
train.rf
best.rf <- train.rf$finalModel
pred.best.rf <- predict(best.rf, newdata = test.ctr.mm) # can use same model matrix

ggplot(train.rf$results, aes(x = mtry, y = Rsquared)) + geom_point(size = 3) + 
  ylab("CV Rsquared") + theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18))

