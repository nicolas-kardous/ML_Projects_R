library(tictoc)
library(keras)
use_session_with_seed(64251)


##### Preparing the data ##### 

# Load MNIST data directly from keras
mnist <- dataset_mnist()
train_images <- mnist$train$x
train_labels <- mnist$train$y
test_images <- mnist$test$x
test_labels <- mnist$test$y

# Need to add 1 channel
train_images <- array_reshape(train_images, c(60000, 28, 28, 1))
train_images <- train_images / 255
test_images <- array_reshape(test_images, c(10000, 28, 28, 1))
test_images <- test_images / 255
train_labels <- to_categorical(train_labels)
test_labels <- to_categorical(test_labels)


##### Building and Training the Convnet ##### 
# https://medium.com/apache-mxnet/multi-channel-convolutions-explained-with-ms-excel-9bbf8eb77108
# First add the convolutional and max pooling layers
# Here we use 32, 64, 64 filters aka channels at the 3 layer conv layers
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                input_shape = c(28, 28, 1)) %>% # 26*26*32
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% # 25*25*32
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>% #23*23*64
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% # 22*22*64
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") #20*20*64
summary(model)


# Then add the dense layers
model <- model %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 10, activation = "softmax")
summary(model)

model %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

# train
tic("Training Time:")
model %>% fit(train_images, train_labels, epochs = 5, batch_size = 64, validation_split = 0.2)
toc()

# evaluate
model %>% evaluate(test_images, test_labels)

