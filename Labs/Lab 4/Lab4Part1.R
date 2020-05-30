#IEOR 242 Discussion 3 Part 1

# install.packages(c('rpart','rpart.plot'))
# install.packages(c('ggplot2','caTools'))

library(dplyr)
library(ggplot2)
library(caTools) # splits
library(rpart) # CART
library(rpart.plot) # CART plotting

### Let's practice finding functions for things we don't already know how to do. 

# CART for classification with Parole data
# https://datascience.stackexchange.com/questions/6048/decision-tree-or-logistic-regression
# https://cran.r-project.org/web/packages/rpart/vignettes/longintro.pdf

parole <- read.csv("NYCparole.csv")

# Very important below!!! 
# transform Violate from 0-1 variable to a factor variable, otherwise CART may try it as regression
parole$Violator <- as.factor(parole$Violator)

# why is this an important function ---^? 

set.seed(144)
split <- sample.split(parole$Violator, 0.7) # 70/30 split

parole.train <- filter(parole, split == TRUE)
parole.test <- filter(parole, split == FALSE)

# Baseline:
table(parole.train$Violator)
table(parole.test$Violator)

# Now let's build a classification tree 
# rpart takes the following syntax:
# Model equation (similar to regression)
# data (similar to regression)
# method:     "class" says we are using CART for classification
# minbucket:  minimum number of observations per bucket
# cp:         we can preset the cp before pruning if we know that we want that desired level of cp
mod <- rpart(Violator ~ Male + Age + TimeServed + Class + Multiple + InCity,
            data = parole.train, method="class", 
            minbucket=5, cp = 0.001)
mod
prp(mod) # plots the tree. Might be slow/crash -- big tree! 

# Try pruning to get a smaller tree
mod.small <- prune(mod, 0.02)
# for our purposes, let's just access cp through the original rpart function
# in other words, know that the prune function exists but it's not worth it to use it for
# cross validation, etc.

# Make predictions 
pred <- predict(mod, newdata = parole.test, type = "class")
table(parole.test$Violator, pred)


# Let's incoproate a loss matrix
loss.mat <- cbind(c(0, 20), c(1, 0)) # cbind is column bind, rbind is row bind

# adding loss function to a list of "parms"
mod2 = rpart(Violator ~ Male + Age + TimeServed + Class + Multiple + InCity,
             data = parole.train, method="class", 
             parms=list(loss = loss.mat),
             minbucket = 5, cp = 0.02)
prp(mod2, digits=3)

pred2 <- predict(mod2, newdata = parole.test, type = "class")
table(parole.test$Violator, pred2)


# Regression example - wine
wine <- read.csv("../Lab1/wine_agg.csv")

wine.train <- filter(wine, Year <= 1985)
wine.test <- filter(wine, Year > 1985)

cart.mod <- rpart(LogAuctionIndex ~ WinterRain + HarvestRain + GrowTemp + HarvestTemp + Age + FrancePop + USAlcConsump, 
                  data = wine.train, method = ??? , cp = 0.02, minsplit = 10)

## fill in the method yourselves. Might require some online searches!! 


prp(cart.mod)

wineCartPredictions <- predict(cart.mod, newdata=wine.test)
SSE = sum((wine.test$LogAuction - wineCartPredictions)^2)
SST = sum((wine.test$LogAuction - mean(wine.train$LogAuction))^2)
OSR2_cart = 1 - SSE/SST # is this model useful at all? 
OSR2_cart
