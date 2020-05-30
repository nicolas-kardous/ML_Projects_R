library(tidyverse)
library(yardstick) # for quantifying how well model fits to a data set
library(randomForest)
library(MASS)
library(keras)
library(tictoc) #for timing R scripts
# use_condaenv("airbnb")

#setwd("/Users/lucas.spangher/Documents/Berkeley Files/Class/Fall 2019/IEOR242/Lab/Lab11")

use_session_with_seed(12)

poker <- read.csv("train.csv")
col_names <- names(poker)
poker[,col_names] <- lapply(poker[,col_names] , factor)

set.seed(1341)
train.ids = sample(nrow(poker), 0.7*nrow(poker))
train.poker = poker[train.ids,]
test.poker = poker[-train.ids,]

# Baseline
# predict 0
table(train.poker$hand)
table(test.poker$hand)
3717/nrow(test.poker)


# Try LDA
mod_lda <- lda(hand ~ ., data = train.poker)
pred_lda <- predict(mod_lda, newdata = test.poker)
metric_lda <- data.frame(truth = test.poker$hand, lda_pred = pred_lda$class)
metrics(metric_lda, truth, lda_pred)

# Try Random Forests
tic("Random Forests time:")
set.seed(133)
mod_rf <- randomForest(hand ~ ., data = train.poker, do.trace = TRUE)
toc() #  744.777
pred_rf <- predict(mod_rf, newdata = test.poker)
metric_rf <- data.frame(truth = test.poker$hand, rf_pred = pred_rf)
metrics(metric_rf, truth, rf_pred)



# Load keras
# If you need to install keras need one of backend engines: TensorFlow, Theano, or CNTK
# To install keras, run these code:
# install.packages("keras")
# library(keras)
# install_keras()

# Useful reference: https://keras.rstudio.com

library(keras)
install_tensorflow()
library(tensorflow)
use_session_with_seed(564) # Use A Session With A Random Seed

# Prep for Keras
trainX <- model.matrix(hand ~ . , data = train.poker)
trainX = trainX[,2:76]
trainY <- model.matrix(~ hand -1, data = train.poker)

testX <- model.matrix(hand ~ . , data = test.poker)
testX = testX[,2:76]
testY <- model.matrix(~ hand -1, data = test.poker)


# Single hidden layer model sigmoid
nn_mod_1 <- keras_model_sequential() 
# Sequential models are created using the keras_model_sequential() function and are composed of a set of linear layers
# Add A Densely-Connected NN Layer To An Output using layer_dense()
# It has arguments object: model, units: number of units, input_shape: Dimensionality of the input, activation: choose activation function
nn_mod_1 %>%
  layer_dense(units = 100, activation = "sigmoid", input_shape = c(75)) %>% # Adding the hidden layer
  layer_dense(units = 10, activation = "softmax") # adding the output layer
summary(nn_mod_1)


#Before training a model, we need to configure the learning process, which is done via the compile() function.
#It receives three arguments:
#1. An optimizer. 
#This could be the string identifier of an existing optimizer (e.g. as “rmsprop” or “adagrad”) or a call to an optimizer function (e.g. optimizer_sgd()).
#2. A loss function. 
#This is the objective that the model will try to minimize. It can be the string identifier of an existing loss function (e.g. “categorical_crossentropy” or “mse”) or a call to a loss function (e.g. loss_mean_squared_error()).
#3. A list of metrics. 
#For any classification problem you will want to set this to metrics = c('accuracy'). A metric could be the string identifier of an existing metric or a call to metric function (e.g. metric_binary_crossentropy()).

# rmsprop is basiclly : Divide the gradient by a running average of its recent magnitude. You can google it to get more information.
# Consider it as a sophisticated way to get gradient in gradient descent.

nn_mod_1 %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

tic("Neural Net 1:") # Neural Net 1:: 92.882 sec elapsed
training_history <- nn_mod_1 %>% 
  fit(trainX, trainY, 
      epochs = 100, validation_split = 0.2)
toc()

# evaluate
nn_mod_1 %>% evaluate(testX, testY)


# Single hidden layer model ReLU
# Switching sigmoid to ReLU max(0,a)

nn_mod_2 <- keras_model_sequential() 
nn_mod_2 %>%
  layer_dense(units = 100, activation = "relu", input_shape = c(75)) %>%
  layer_dense(units = 10, activation = "softmax")
summary(nn_mod_2)

nn_mod_2 %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

tic("Neural Net 2:")
training_history <- nn_mod_2 %>% 
  fit(trainX, trainY, 
      epochs = 100, validation_split = 0.2)
toc()

# evaluate
nn_mod_2 %>% evaluate(testX, testY)

# Is it better than sigmoid?


# Three hidden layer model ReLU
nn_mod_3 <- keras_model_sequential() 
nn_mod_3 %>%
  layer_dense(units = 75, activation = "relu", input_shape = c(75)) %>%
  layer_dense(units = 50, activation = "relu") %>%
  layer_dense(units = 25, activation = "relu") %>%
  layer_dense(units = 10, activation = "softmax")
summary(nn_mod_3)

nn_mod_3 %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

tic("Neural Net 3:")
training_history <- nn_mod_3 %>% 
  fit(trainX, trainY, 
      epochs = 100, validation_split = 0.2)
toc()

# evaluate
nn_mod_3 %>% evaluate(testX, testY)


