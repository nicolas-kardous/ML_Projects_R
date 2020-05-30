library(boot)

load("after_boost.RData")

tableAccuracy <- function(label, pred) {
  t = table(label, pred)
  a = sum(diag(t))/length(label)
  return(a)
}

tableTPR <- function(label, pred) {
  t = table(label, pred)
  return(t[2,2]/(t[2,1] + t[2,2]))
}

tableFPR <- function(label, pred) {
  t = table(label, pred)
  return(t[1,2]/(t[1,1] + t[1,2]))
}

boot_accuracy <- function(data, index) {
  labels <- data$label[index]
  predictions <- data$prediction[index]
  return(tableAccuracy(labels, predictions))
}

boot_tpr <- function(data, index) {
  labels <- data$label[index]
  predictions <- data$prediction[index]
  return(tableTPR(labels, predictions))
}

boot_fpr <- function(data, index) {
  labels <- data$label[index]
  predictions <- data$prediction[index]
  return(tableFPR(labels, predictions))
}

boot_all_metrics <- function(data, index) {
  acc = boot_accuracy(data, index)
  tpr = boot_tpr(data, index)
  fpr = boot_fpr(data, index)
  return(c(acc, tpr, fpr))
}

# sanity test
table(TweetsTest$Negative, predict.lda)
lda_df = data.frame(labels = TweetsTest$Negative, predictions = predict.lda)
boot_accuracy(lda_df, 1:355)
boot_tpr(lda_df, 1:355)
boot_fpr(lda_df, 1:355)
boot_all_metrics(lda_df, 1:355)



#### do bootstrap

big_B = 100000

# Logistic
log_df = data.frame(labels = TweetsTest$Negative, predictions =  PredictStepLog > 0.5)
set.seed(342)
Log_boot = boot(log_df, boot_all_metrics, R = big_B)
Log_boot
boot.ci(Log_boot, index = 1, type = "basic")
boot.ci(Log_boot, index = 2, type = "basic")
boot.ci(Log_boot, index = 3, type = "basic")

# LDA
lda_df = data.frame(labels = TweetsTest$Negative, predictions = predict.lda)
set.seed(5810)
Lda_boot = boot(lda_df, boot_all_metrics, R = big_B)
Lda_boot
boot.ci(Lda_boot, index = 1, type = "basic")
boot.ci(Lda_boot, index = 2, type = "basic")
boot.ci(Lda_boot, index = 3, type = "basic")

# CART
cart_df = data.frame(labels = TweetsTest$Negative, predictions = predict.cart)
set.seed(3526)
CART_boot = boot(cart_df, boot_all_metrics, R = big_B)
CART_boot
boot.ci(CART_boot, index = 1, type = "basic")
boot.ci(CART_boot, index = 2, type = "basic")
boot.ci(CART_boot, index = 3, type = "basic")

# CART
rf_df = data.frame(labels = TweetsTest$Negative, predictions = predict.rf)
set.seed(6722)
RF_boot = boot(rf_df, boot_all_metrics, R = big_B)
RF_boot
boot.ci(RF_boot, index = 1, type = "basic")
boot.ci(RF_boot, index = 2, type = "basic")
boot.ci(RF_boot, index = 3, type = "basic")

# Boosting
boost_df = data.frame(labels = TweetsTest$Negative, predictions = predict.boost < 0.5)
set.seed(9391)
Boost_boot = boot(boost_df, boot_all_metrics, R = big_B)
Boost_boot
boot.ci(Boost_boot, index = 1, type = "basic")
boot.ci(Boost_boot, index = 2, type = "basic")
boot.ci(Boost_boot, index = 3, type = "basic")

# Baseline
predict.baseline = factor(seq(0,0,length.out = 355), levels = c(0,1))
baseline_df = data.frame(labels = TweetsTest$Negative, predictions = predict.baseline)
set.seed(6829)
Baseline_boot = boot(baseline_df, boot_all_metrics, R = big_B)
Baseline_boot
boot.ci(Baseline_boot, index = 1, type = "basic")
boot.ci(Baseline_boot, index = 2, type = "basic")
boot.ci(Baseline_boot, index = 3, type = "basic")

