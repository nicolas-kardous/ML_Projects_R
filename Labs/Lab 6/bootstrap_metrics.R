library(boot)

mean_squared_error <- function(data, index) {
  responses <- data$response[index]
  predictions <- data$prediction[index]
  MSE <- mean((responses - predictions)^2)
  return(MSE)
}

mean_absolute_error <- function(data, index) {
  responses <- data$response[index]
  predictions <- data$prediction[index]
  MAE <- mean(abs(responses - predictions))
  return(MAE)
}

OS_R_squared <- function(data, index) {
  responses <- data$response[index]
  predictions <- data$prediction[index]
  baseline <- data$baseline[index]
  SSE <- sum((responses - predictions)^2)
  SST <- sum((responses - baseline)^2)
  r2 <- 1 - SSE/SST
  return(r2)
}

all_metrics <- function(data, index) {
  mse <- mean_squared_error(data, index)
  mae <- mean_absolute_error(data, index)
  OSR2 <- OS_R_squared(data, index)
  return(c(mse, mae, OSR2))
}


###### Random Forests + Plots ###### 
RF_test_set = data.frame(response = test.ctr$CTR, prediction = pred.best.rf, baseline = mean(train.ctr$CTR))

# sanity check
all_metrics(RF_test_set, 1:1818)
mean((pred.best.rf - test.ctr$CTR)^2)
mean(abs(pred.best.rf - test.ctr$CTR))
OSR2(pred.best.rf, test.ctr$CTR, train.ctr$CTR)


# do bootstrap
set.seed(892)
RF_boot <- boot(RF_test_set, all_metrics, R = 10000)
RF_boot


# make plots
rf_boot_plot_results = data.frame(osr2estimates = RF_boot$t[,3], delta = RF_boot$t[,3] - RF_boot$t0[3])

ggplot(rf_boot_plot_results) + geom_histogram(aes(x = osr2estimates), binwidth = 0.005, color = "blue") + 
  ylab("Count") + xlab("Bootstrap OSR2 Estimate") + theme_bw() + 
  theme(axis.title=element_text(size=18), axis.text=element_text(size=18))

# manual CI + plot

quantile(rf_boot_plot_results$delta, c(0.025, 0.975))

ggplot(rf_boot_plot_results) + geom_histogram(aes(x = delta), binwidth = 0.005, color = "green") + 
  ylab("Count") + xlab("Boot OSR2 - Test Set OSR2") + theme_bw() + 
  theme(axis.title=element_text(size=18), axis.text=element_text(size=18)) +
  geom_vline(xintercept = -0.05964194) +
  geom_vline(xintercept = 0.05359415)

test_set_OSR2 = OSR2(pred.best.rf, test.ctr$CTR, train.ctr$CTR)
lower = test_set_OSR2 - 0.05359415
upper = test_set_OSR2 - -0.05964194


# get confidence intervals (not manually)
boot.ci(RF_boot, index = 1, type = "basic")
boot.ci(RF_boot, index = 2, type = "basic")
boot.ci(RF_boot, index = 3, type = "basic")



###### CART ###### 
CART_test_set = data.frame(response = test.ctr$CTR, prediction = pred.best.cart, baseline = mean(train.ctr$CTR))

# sanity check
all_metrics(CART_test_set, 1:1818)
mean((pred.best.cart - test.ctr$CTR)^2)
mean(abs(pred.best.cart - test.ctr$CTR))
OSR2(pred.best.cart, test.ctr$CTR, train.ctr$CTR)


# do bootstrap
set.seed(7191)
CART_boot <- boot(CART_test_set, all_metrics, R = 10000)
CART_boot

# get confidence intervals
boot.ci(CART_boot, index = 1, type = "basic")
boot.ci(CART_boot, index = 2, type = "basic")
boot.ci(CART_boot, index = 3, type = "basic")



###### Boosting ###### 
# code to re-train the final model without CV
mod.boost <- gbm(CTR ~ .,
                 data = train.ctr,
                 distribution = "gaussian",
                 n.trees = 11500,
                 shrinkage = 0.001,
                 interaction.depth = 10)

pred.best.boost <- predict(mod.boost, newdata = test.ctr, n.trees = 11500)


Boost_test_set = data.frame(response = test.ctr$CTR, prediction = pred.best.boost, baseline = mean(train.ctr$CTR))

# sanity check
all_metrics(Boost_test_set, 1:1818)
mean((pred.best.boost - test.ctr$CTR)^2)
mean(abs(pred.best.boost - test.ctr$CTR))
OSR2(pred.best.boost, test.ctr$CTR, train.ctr$CTR)


# do bootstrap
set.seed(3134)
Boost_boot <- boot(Boost_test_set, all_metrics, R = 10000)
Boost_boot

# get confidence intervals
boot.ci(Boost_boot, index = 1, type = "basic")
boot.ci(Boost_boot, index = 2, type = "basic")
boot.ci(Boost_boot, index = 3, type = "basic")



###### Linear Reg ###### 
LR_test_set = data.frame(response = test.ctr$CTR, prediction = pred.mod.lm, baseline = mean(train.ctr$CTR))

# sanity check
all_metrics(LR_test_set, 1:1818)
mean((pred.mod.lm - test.ctr$CTR)^2)
mean(abs(pred.mod.lm - test.ctr$CTR))
OSR2(pred.mod.lm, test.ctr$CTR, train.ctr$CTR)


# do bootstrap
set.seed(562)
LR_boot <- boot(LR_test_set, all_metrics, R = 10000)
LR_boot

# get confidence intervals
boot.ci(LR_boot, index = 1, type = "basic")
boot.ci(LR_boot, index = 2, type = "basic")
boot.ci(LR_boot, index = 3, type = "basic")




###### Advanced:  Boosting minus RF ###### 
OS_R_squared_diff <- function(data, index) {
  responses <- data$response[index]
  predictions_boost <- data$prediction_boost[index]
  predictions_rf <- data$prediction_rf[index]
  baseline <- data$baseline[index]
  
  SSE_boost <- sum((responses - predictions_boost)^2)
  SST_boost <- sum((responses - baseline)^2)
  r2_boost <- 1 - SSE_boost/SST_boost
  
  SSE_rf <- sum((responses - predictions_rf)^2)
  SST_rf <- sum((responses - baseline)^2)
  r2_rf <- 1 - SSE_rf/SST_rf
  
  return(r2_boost - r2_rf)
}

RF_Boost_test_set = data.frame(response = test.ctr$CTR, prediction_boost = pred.best.boost, 
                               prediction_rf = pred.best.rf, baseline = mean(train.ctr$CTR))

# sanity check
OS_R_squared_diff(RF_Boost_test_set, 1:1818)
OSR2(pred.best.boost, test.ctr$CTR, train.ctr$CTR) - OSR2(pred.best.rf, test.ctr$CTR, train.ctr$CTR)


# do bootstrap
set.seed(567)
Diff_boot <- boot(RF_Boost_test_set, OS_R_squared_diff, R = 10000)
Diff_boot

boot.ci(Diff_boot, type = "basic")


