install.packages("softImpute")
install.packages("ranger")

library(softImpute)
library(randomForest)
library(ranger)
library(dplyr)
library(tidyverse)
library(reshape2)

OSR2 <- function(predictions, train, test) {
  SSE <- sum((test - predictions)^2)
  SST <- sum((test - mean(train))^2)
  r2 <- 1 - SSE/SST
  return(r2)
}

movie <- read.csv("MovieLensFeatures.csv")

set.seed(122)
train.ids <- sample(nrow(movie), 0.95*nrow(movie))
train <- movie[train.ids,]
test <- movie[-train.ids,]

# split training into real training and validation set
val.ids <- sample(nrow(train), (5/95)*nrow(train))
val <- train[val.ids,]
train <- train[-val.ids,]


# Biscale

mat.train <- Incomplete(train$userID, train$movieID, train$rating)
summary(train)


# Instruction of biscale()
set.seed(100)
mat.train.centered <- biScale(mat.train, maxit = 10000, row.scale = FALSE, col.scale = FALSE)
# mat.train.centered is X_ij - alpha_i - beta_j
alpha <- attr(mat.train.centered, "biScale:row")$center
beta <- attr(mat.train.centered, "biScale:column")$center
# take a look at mutate(), arrange(), desc(), inner_join(), functions that may be useful for HW5

