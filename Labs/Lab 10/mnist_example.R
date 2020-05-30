library(tictoc)
library(keras)
use_condaenv("IEOR242")
use_session_with_seed(12)
# tf works, the information it spits outt just means it isnt as fast as it could be.



##### Preparing the data ##### 

# Load MNIST data directly from keras
mnist <- dataset_mnist()
train_images <- mnist$train$x
train_labels <- mnist$train$y
test_images <- mnist$test$x
test_labels <- mnist$test$y

str(train_images) # 60,000 28 x 28
str(train_labels) 
table(train_labels) #60,000 labels between 0-9, baseline model is to predict class 1

table(test_labels)
baseline_acc = 1135/nrow(test_labels)

# Reshape the image data into length 28*28 = 784 feature vectors, then normalize
train_images <- array_reshape(train_images, c(60000, 28 * 28))
train_images <- train_images / 255

test_images <- array_reshape(test_images, c(10000, 28 * 28))
test_images <- test_images / 255

str(train_images)

# Convert labels to dummy variable categories (one hot encoding)
train_labels <- to_categorical(train_labels)
test_labels <- to_categorical(test_labels)


##### Building and Training the Network ##### 

network <- keras_model_sequential() %>%
  layer_dense(units = 512, activation = "relu", input_shape = c(28 * 28)) %>%
  layer_dense(units = 10, activation = "softmax")
summary(network)

network %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

# train
tic("Training Time:")
network %>% fit(train_images, train_labels, epochs = 5, batch_size = 128, validation_split = 0.2)
toc()

# evaluate
network %>% evaluate(test_images, test_labels)

