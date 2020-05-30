# IND242HW4
# Nicolas Kardous

# First, install the required packages
install.packages("tm")
install.packages("SnowballC")

library(tm)
library(SnowballC)
library(wordcloud)
library(MASS)
library(caTools)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(tm.plugin.webmining)

# Problem 1

data = read.csv("ggplot2questions2016_17.csv", stringsAsFactors=FALSE)

# Add a column for Useful posts

data$Useful = as.factor(as.numeric(data$Score >= 1))

# Step 1: Extract the html strips from the data

for (i in seq(1,length(data$Body),1)) {
  data$Body[i]=extractHTMLStrip(data$Body[i])
}

# Step 2: Get rid of all the numbers in the title and body

data$Title = gsub("[0-9]*", "", data$Title)
data$Body = gsub("[0-9]*", "", data$Body)

# Step 3: Convert title and body to a "corpus"

# A vector source interprets each element of the vector as a document.
# Corpus creates a collection of documents 
corpusTitle = Corpus(VectorSource(data$Title))
corpusBody = Corpus(VectorSource(data$Body))
# The titles and body are now "documents"

# Step 4: Change all the text to lower case.
# tm_map applies an operation to every document in our corpus
# Here, that operation is 'tolower', i.e., 'to lowercase'
corpusTitle = tm_map(corpusTitle, tolower)
corpusBody = tm_map(corpusBody, tolower)
# tolower is a function

# Step 5: Remove all punctuation
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusBody = tm_map(corpusBody, removePunctuation)

# Step 6: Remove stop words
# Remove stopword,these are words common in the title and body
corpusTitle = tm_map(corpusTitle, removeWords, c("apple", "ggplot","ggplot2",stopwords("english")))
corpusBody = tm_map(corpusBody, removeWords, c("apple", "ggplot","ggplot2",stopwords("english")))

# Step 7: Stem our document
# We chop off the ends of words that aren't maybe
# as necessary as the rest, like 'ing' and 'ed'
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusBody = tm_map(corpusBody, stemDocument)

# Step 8: Create a word count matrix (rows are each post, columns are words)
# We've finished our basic cleaning, so now we want to calculate frequencies
# of words across each post
frequenciesTitle = DocumentTermMatrix(corpusTitle)
frequenciesBody = DocumentTermMatrix(corpusBody)

# Step 9: Account for sparsity
# We currently have way too many words, which will make it hard to train
# our models and may even lead to overfitting.
# Use findFreqTerms to get a feeling for which words appear the most

# Words that appear at least 50 times:
findFreqTerms(frequenciesTitle, lowfreq=50)
findFreqTerms(frequenciesBody, lowfreq=50)
# Words that appear at least 20 times:
findFreqTerms(frequenciesTitle, lowfreq=20)
findFreqTerms(frequenciesBody, lowfreq=20)

# Our solution to the possibility of overfitting is to only keep terms
# that appear in x% or more of the posts. For example:
# 2% of the posts or more for the title, and 9% or more for the body
# Here we will get 45 variables for the title, and 88 variables for the body
sparseTitle = removeSparseTerms(frequenciesTitle, 0.98)
sparseBody = removeSparseTerms(frequenciesBody, 0.91)

# Step 10: Create data frame from the document-term matrix, we have 45 variables for the title, 
# and 88 variables for the body
dataTitle = as.data.frame(as.matrix(sparseTitle))
dataBody = as.data.frame(as.matrix(sparseBody))

# We have some variable names that start with a number, 
# which can cause R some problems. Let's fix this before going
# any further
colnames(dataTitle) = make.names(colnames(dataTitle))
colnames(dataBody) = make.names(colnames(dataBody))

# Step 11: Join the independent variables together

duplicatedBody = dataBody[, names(dataBody) %in% names(dataTitle)]
duplicatedTitle = dataTitle[, names(dataTitle) %in% names(dataBody)]
duplicatedTotal = duplicatedBody + duplicatedTitle
Bodynew = select(dataBody, -c(names(duplicatedTitle)))
Titlenew = select(dataTitle, -c(names(duplicatedTitle)))
datajoined = as.data.frame(cbind(duplicatedTotal,Bodynew,Titlenew))

# Step 12: Create new column to say a score is positive if it is greater than or equal to 1
datajoined$Useful = data$Useful

# Part b)

spl = sample.split(datajoined$Useful, SplitRatio = 0.7)

Train <- datajoined %>% filter(spl == TRUE)
Test <- datajoined %>% filter(spl == FALSE)

table(Train$Useful)
table(Test$Useful)

# Function to compute accuracy of a classification model
tableAccuracy <- function(test, pred) {
  t = table(test, pred)
  a = sum(diag(t))/sum(t)
  return(a)
}

# Basic Random Forests:
DataRF = randomForest(Useful ~ ., data=Train)
PredictRF = predict(DataRF, newdata = Test)
table(Test$Useful, PredictRF)
tableAccuracy(Test$Useful, PredictRF)

# Random Forest with changing mtry:

DataRF2 <- train(Useful ~.,
                 data = Train,
                 method = "rf", # random forest
                 tuneGrid = data.frame(mtry=1:10),
                 trControl = trainControl(method="cv", number=5, verboseIter = TRUE),
                 metric = "Accuracy")

DataRF2.best <- DataRF2$finalModel
PredictRF2 = predict(DataRF2.best, newdata = Test)
table(Test$Useful, PredictRF2)
tableAccuracy(Test$Useful, PredictRF2)

# Cross-validated CART model
train.cart = train(Useful ~ .,
                   data = Train,
                   method = "rpart",
                   tuneGrid = data.frame(cp=seq(0, 0.4, 0.002)),
                   trControl = trainControl(method="cv", number=10))
train.cart
train.cart$results
train.cart$bestTune

ggplot(train.cart$results, aes(x = cp, y = Accuracy)) + 
  geom_point(size = 2) + 
  geom_line() + 
  ylab("CV Accuracy") + 
  theme_bw() + 
  theme(axis.title=element_text(size=18), axis.text=element_text(size=18))

mod.cart = train.cart$finalModel
prp(mod.cart)

predict.cart = predict(mod.cart, newdata = Test, type = "class")
table(Test$Useful, predict.cart)
tableAccuracy(Test$Useful, predict.cart)

# What about CART on training set?
PredictCARTTrain = predict(mod.cart, type = "class")
table(Train$Useful, PredictCARTTrain)
tableAccuracy(Train$Useful, PredictCARTTrain)

# Logistic Regression

DataLog = glm(Useful ~ ., data = Train, family = "binomial")
summary(DataLog)

# Predictions on test set
PredictLog = predict(DataLog, newdata = Test, type = "response")
table(Test$Useful, PredictLog > 0.5)
tableAccuracy(Test$Useful, PredictLog > 0.5)

# But what about training set?
PredictLogTrain = predict(DataLog, type = "response")
table(Train$Useful, PredictLogTrain > 0.5)
tableAccuracy(Train$Useful, PredictLogTrain > 0.5)

# Linear Discriminant Analysis
library(MASS)
lda.mod = lda(Useful ~ ., data = Train)

predict.lda = predict(lda.mod, newdata = Test)$class
table(Test$Useful, predict.lda)
tableAccuracy(Test$Useful, predict.lda)

### Bootstrap ###

library(boot)

TPR <- function(data, index) {
  responses <- data$response[index]
  predictions <- data$prediction[index]
  contingency <- table(responses, predictions)
  TPR <- contingency[2, 2]/(contingency[2,2]+contingency[2,1])
  return(TPR)
}

FPR <- function(data, index) {
  responses <- data$response[index]
  predictions <- data$prediction[index]
  contingency <- table(responses, predictions)
  FPR <- contingency[1, 2]/(contingency[1, 2] + contingency[1, 1])
  return(FPR)
}

Accuracy <- function(data, index) {
  responses <- data$response[index]
  predictions <- data$prediction[index]
  t = table(responses, predictions)
  a = sum(diag(t))/length(responses)
  return(a)
}

all_metrics <- function(data, index) {
  accuracy <- Accuracy(data, index)
  tpr <- TPR(data, index)
  fpr <- FPR(data, index)
  return(c(accuracy, tpr, fpr))
}

###### Basic Random Forest ###### 
RF_test_set = data.frame(response = Test$Useful, prediction = PredictRF)

# do bootstrap
RF_boot <- boot(RF_test_set, all_metrics, R = 10000)
RF_boot

# get confidence intervals (not manually)
boot.ci(RF_boot, index = 1, type = "basic")
boot.ci(RF_boot, index = 2, type = "basic")
boot.ci(RF_boot, index = 3, type = "basic")

###### Random Forest with changing mtry ######

RF_test_set2 = data.frame(response = Test$Useful, prediction = PredictRF2)

# do bootstrap
RF_boot2 <- boot(RF_test_set, all_metrics, R = 10000)
RF_boot2

# get confidence intervals (not manually)
boot.ci(RF_boot2, index = 1, type = "basic")
boot.ci(RF_boot2, index = 2, type = "basic")
boot.ci(RF_boot2, index = 3, type = "basic")

###### CART ###### 
CART_test_set = data.frame(response = Test$Useful, prediction = predict.cart)

# do bootstrap
CART_boot <- boot(CART_test_set, all_metrics, R = 10000)
CART_boot

# get confidence intervals
boot.ci(CART_boot, index = 1, type = "basic")
boot.ci(CART_boot, index = 2, type = "basic")
boot.ci(CART_boot, index = 3, type = "basic")

###### Logistic Regression ###### 
Log_test_set = data.frame(response = Test$Useful, prediction = PredictLog)

# do bootstrap
Log_boot <- boot(Log_test_set, all_metrics, R = 10000)
Log_boot

# get confidence intervals
boot.ci(Log_boot, index = 1, type = "basic")
boot.ci(Log_boot, index = 2, type = "basic")
boot.ci(Log_boot, index = 3, type = "basic")

###### Linear Discriminant Analysis ###### 
LDA_test_set = data.frame(response = Test$Useful, prediction = predict.lda)

# do bootstrap
LDA_boot <- boot(LDA_test_set, all_metrics, R = 10000)
LDA_boot

# get confidence intervals
boot.ci(LDA_boot, index = 1, type = "basic")
boot.ci(LDA_boot, index = 2, type = "basic")
boot.ci(LDA_boot, index = 3, type = "basic")
