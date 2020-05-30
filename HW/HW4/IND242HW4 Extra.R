
# Boosting
tGrid = expand.grid(n.trees = (1:100)*50, interaction.depth = c(1,2,4,6,8,10,12,14,16),
                    shrinkage = 0.01, n.minobsinnode = 10)

train.boost <- train(Useful ~ .,
                     data = Train,
                     method = "gbm",
                     tuneGrid = tGrid,
                     trControl = trainControl(method="cv", number=5, verboseIter = TRUE),
                     metric = "Accuracy",
                     distribution = "bernoulli")
train.boost
train.boost$results

ggplot(train.boost$results, aes(x = n.trees, y = Accuracy, colour = as.factor(interaction.depth))) + geom_line() + 
  ylab("CV Accuracy") + theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18)) + 
  scale_color_discrete(name = "interaction.depth")

mod.boost = train.boost$finalModel

Test.boost = as.data.frame(model.matrix(Useful ~ . +0, data = Test))
predict.boost = predict(mod.boost, newdata = Test.boost, n.trees = 4100, type = "response")
table(Test$Useful, predict.boost < 0.5) # for some reason the probabilities are flipped in gbm
tableAccuracy(Test$Useful, predict.boost < 0.5)


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