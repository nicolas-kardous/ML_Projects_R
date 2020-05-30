#install.packages("softImpute")
#install.packages("ranger")

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

# movie <- MovieLensFeatures

# clean up
movie$AgeRange <- as.factor(movie$AgeRange)
movie$wday <- as.factor(movie$wday)
movie$mon <- as.factor(movie$mon)
movie$year <- as.factor(movie$year)
movie$hour <- as.factor(movie$hour)

set.seed(122)
train.ids <- sample(nrow(movie), 0.95*nrow(movie))
train <- movie[train.ids,]
test <- movie[-train.ids,]

# split training into real training and validation set
val1.ids <- sample(nrow(train), (5/95)*nrow(train))
val1 <- train[val1.ids,]
train <- train[-val1.ids,]

val2.ids <- sample(nrow(train), (5/90)*nrow(train))
val2 <- train[val2.ids,]
train <- train[-val2.ids,]

# First try CF
mat.train <- Incomplete(train$userID, train$movieID, train$rating)

# compute validation set MAE for rank = 1,2,...,20
mae.vals = rep(NA, 20)
for (rnk in seq_len(20)) {
  print(str_c("Trying rank.max = ", rnk))
  mod <- softImpute(mat.train, rank.max = rnk, lambda = 0, maxit = 1000)
  preds <- impute(mod, val1$userID, val1$movieID) %>% pmin(5) %>% pmax(1)
  mae.vals[rnk] <- mean(abs(preds - val1$rating))
}

mae.val.df <- data.frame(rnk = seq_len(20), mae = mae.vals)
ggplot(mae.val.df, aes(x = rnk, y = mae)) + geom_point(size = 3) + 
  ylab("Validation MAE") + xlab("Number of Archetypal Users") + 
  theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18))


# choose k = 10
set.seed(100)
mod.final <- softImpute(mat.train, rank.max = 10, lambda = 0, maxit = 1000)
preds <- impute(mod.final, test$userID, test$movieID) %>% pmin(5) %>% pmax(1)

mean(abs(preds - test$rating))/4
sqrt(mean((preds - test$rating)^2))/4
OSR2(preds, train$rating, test$rating)


# Now try a linear regression without CF as a varible
lin.mod <- lm(rating ~ . -movieID -userID, data = train)
summary(lin.mod)

preds.lm <- predict(lin.mod, newdata = test) %>% pmin(5) %>% pmax(1)
mean(abs(preds.lm - test$rating))/4
sqrt(mean((preds.lm - test$rating)^2))/4
OSR2(preds.lm, train$rating, test$rating)


# Now try random forests (Warning: this took 2 hours to run)
set.seed(3592)
rf.mod <- ranger(rating ~ . -movieID -userID, 
                 data = train, 
                 mtry = floor((ncol(train) - 3)/3), 
                 num.trees = 500,
                 verbose = TRUE)

preds.rf <- predict(rf.mod, data = test)
preds.rf <- preds.rf$predictions
mean(abs(preds.rf - test$rating))/4
sqrt(mean((preds.rf - test$rating)^2))/4
OSR2(preds.rf, train$rating, test$rating)



# Blending

val.preds.cf <- impute(mod.final, val2$userID, val2$movieID)
val.preds.lm <- predict(lin.mod, newdata = val2)
val.preds.rf <- predict(rf.mod, data = val2)$predictions

# Build validation set data frame
val.blending_df = data.frame(rating = val2$rating, cf_preds = val.preds.cf, 
                             lm_preds = val.preds.lm, rf_preds = val.preds.rf)

# Train blended model
blend.mod = lm(rating ~ . -1, data = val.blending_df)
summary(blend.mod)

# Get predictions on test set
test.preds.cf <- impute(mod.final, test$userID, test$movieID)
test.preds.lm <- predict(lin.mod, newdata = test)
test.preds.rf <- predict(rf.mod, data = test)$predictions

test.blending_df = data.frame(rating = test$rating, cf_preds = test.preds.cf, 
                              lm_preds = test.preds.lm, rf_preds = test.preds.rf)

test.preds.blend <- predict(blend.mod, newdata = test.blending_df)

mean(abs(test.preds.blend - test$rating))/4
sqrt(mean((test.preds.blend - test$rating)^2))/4
OSR2(test.preds.blend, train$rating, test$rating)

