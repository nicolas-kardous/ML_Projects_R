# install.packages('caret')
# install.packages('e1071')

library(dplyr)
library(ggplot2)
library(caTools) # splits
library(rpart) # CART
library(rpart.plot) # CART plotting
library(caret) # cross validation

# cross validation in caret
# https://en.wikipedia.org/wiki/Cross-validation_(statistics)
# First standard CV with respect to Accuracy, then the loss function
# Syntax below: 
# method = specify classification method, "rpart" for CART
# tuneGrid = gives the sequence of parameters to try, 
#             in this case, we try cp = 0 through cp=0.1 in increments of .002
# trControl = here using 10-fold cross validation
# metric = "Accuracy" for classification accuracy, "RMSE" or "Rsquared" or for regression

cpVals = data.frame(cp = seq(0, .04, by=.002))
# https://topepo.github.io/caret/model-training-and-tuning.html


parole.train= read.csv("Downloads/NYCparole.csv")

set.seed(123)
train.cart <- train(Violator ~ Male + Age + TimeServed + Class + Multiple + InCity,
                    data = parole.train,
                    method = "rpart",
                    tuneGrid = cpVals,
                    trControl = trainControl(method = "cv", number=10),
                    metric = "Accuracy")

# look at the cross validation results, stored as a data-frame
# https://machinelearningmastery.com/machine-learning-evaluation-metrics-in-r/
train.cart$results # please ignore kappa
train.cart

# plot the results
ggplot(train.cart$results, aes(x=cp, y=Accuracy)) + geom_point()
# We can increase the size of the points:
ggplot(train.cart$results, aes(x=cp, y=Accuracy)) + geom_point(size=3)
# We can change the default axis labels
ggplot(train.cart$results, aes(x=cp, y=Accuracy)) + geom_point(size=3) +
  xlab("Complexity Parameter (cp)") + geom_line()


# Extract the best model and make predictions
train.cart$bestTune
mod123 = train.cart$finalModel
prp(mod123, digits=3)

# We need to extract the "model matrix" for parole.test before we can make predictions
# This is because caret does not work with factors, instead it creates dummy variables 
parole.test.mm = as.data.frame(model.matrix(Violator~.+0, data=parole.test))
pred = predict(mod123, newdata=parole.test.mm, type="class")
table(parole.test$Violator, pred)



######## Cross-validation on CART with Loss Function ######

cpVals = data.frame(cp = seq(0, .04, by=.002))
set.seed(123)

# we will first define a special loss function in CV
# we would like to have the small Average Loss
Loss = function(data, lev = NULL, model = NULL, ...) {
  c(AvgLoss = mean(data$weights * (data$obs != data$pred)),
    Accuracy = mean(data$obs == data$pred))
}
# We using 10-fold cross-validation, with our defined 
# "Loss" function when choosing best model in cv
trControl = trainControl(method = "cv",
                         number = 10,
                         summaryFunction = Loss)

# This specifies the weights i.e. the loss function
# Read this as: if observation is a violator and we make a mistake, assign weight 20
#               if observation is NOT a violator and we make a mistake, assign weight 1
weights = ifelse(parole.train$Violator == 1, 20, 1)

# now train with cross validation!
loss.cv <- train(Violator ~ Male + Age + TimeServed + Class + Multiple + InCity,
                 data=parole.train,
                 method="rpart",
                 weights = weights,
                 trControl=trControl,
                 tuneGrid=cpVals, 
                 metric="AvgLoss", 
                 maximize=FALSE)

# plot the Average Loss as a function of cp 
cp.plot <- ggplot(loss.cv$results, aes(x=cp, y=AvgLoss)) + geom_line(lwd=2) +
  ylab("Average Loss of Predictions")
cp.plot

# add some error bars
cp.plot2 <- cp.plot + geom_errorbar(aes(ymin = AvgLoss - AvgLossSD, ymax = AvgLoss + AvgLossSD))
cp.plot2

loss.cv$bestTune
mod3432 = loss.cv$finalModel
prp(mod3432, digits=3)

# make predictions
parole.test.mm = as.data.frame(model.matrix(Violator~.+0, data=parole.test))
pred = predict(mod3432, newdata=parole.test.mm, type="class")
table(parole.test$Violator, pred)

