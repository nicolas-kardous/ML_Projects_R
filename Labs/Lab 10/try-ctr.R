library(tidyverse)
library(randomForest)

OSR2 <- function(predictions, test, train) {
  SSE <- sum((test - predictions)^2)
  SST <- sum((test - mean(train))^2)
  r2 <- 1 - SSE/SST
  return(r2)
}

ctr <- read_csv("CTR.csv")

set.seed(144)
train.ids = sample(nrow(ctr), 0.7*nrow(ctr))
train.ctr = ctr[train.ids,]
test.ctr = ctr[-train.ids,]



# Load keras
# install if you have never used keras
library(keras)
use_session_with_seed(349)

# Prep for Keras
trainX <- model.matrix(CTR ~ . , data = train.ctr)
trainX <- trainX[,2:17]
trainY <- train.ctr$CTR

testX <- model.matrix(CTR ~ . , data = test.ctr)
testX <- testX[,2:17]
testY <- test.ctr$CTR

# Single layer model
nn_mod_1 <- keras_model_sequential() 
nn_mod_1 %>%
  layer_dense(units = 15, activation = "sigmoid", input_shape = c(16)) %>%
  layer_dense(units = 1)
summary(nn_mod_1)

nn_mod_1 %>% compile(
  optimizer = "rmsprop",
  loss = "mse"
)

nn_mod_1 %>% fit(trainX, trainY,
               epochs = 50, validation_split = 0.2)

# evaluate
nn_pred <- predict(nn_mod_1, testX)
OSR2(nn_pred, testY, trainY)
# 0.4679358


# Three hidden layer model
nn_mod_2 <- keras_model_sequential() 
nn_mod_2 %>%
  layer_dense(units = 15, activation = "sigmoid", input_shape = c(16)) %>%
  layer_dense(units = 15, activation = "sigmoid") %>%
  layer_dense(units = 15, activation = "sigmoid") %>%
  layer_dense(units = 1)
summary(nn_mod_2)

nn_mod_2 %>% compile(
  optimizer = "rmsprop",
  loss = "mse"
)

nn_mod_2 %>% fit(trainX, trainY,
                 epochs = 50, validation_split = 0.2)

# evaluate
nn_pred_2 <- predict(nn_mod_2, testX)
OSR2(nn_pred_2, testY, trainY)
# 0.4633785

# 10 hidden layer model
nn_mod_3 <- keras_model_sequential() 
nn_mod_3 %>%
  layer_dense(units = 15, activation = "sigmoid", input_shape = c(16)) %>%
  layer_dense(units = 15, activation = "sigmoid") %>%
  layer_dense(units = 15, activation = "sigmoid") %>%
  layer_dense(units = 15, activation = "sigmoid") %>%
  layer_dense(units = 15, activation = "sigmoid") %>%
  layer_dense(units = 15, activation = "sigmoid") %>%
  layer_dense(units = 15, activation = "sigmoid") %>%
  layer_dense(units = 15, activation = "sigmoid") %>%
  layer_dense(units = 15, activation = "sigmoid") %>%
  layer_dense(units = 15, activation = "sigmoid") %>%
  layer_dense(units = 1)
summary(nn_mod_3)

nn_mod_3 %>% compile(
  optimizer = "rmsprop",
  loss = "mse"
)

nn_mod_3 %>% fit(trainX, trainY,
                 epochs = 50, validation_split = 0.2)

# evaluate
nn_pred_3 <- predict(nn_mod_3, testX)
OSR2(nn_pred_3, testY, trainY)





