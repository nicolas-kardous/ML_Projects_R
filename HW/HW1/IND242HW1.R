# IND242HW1 Problem 3
# Nicolas Kardous

# Part a)

mydata = read.csv("Wrangler242-Fall2019.csv")
mydata.train <- filter(mydata, Year <= 2015)
mydata.test <- filter(mydata, Year > 2015)

LR1a <- lm(WranglerSales ~ Unemployment + WranglerQueries + CPI.All + CPI.Energy, data=mydata.test)
cor(mydata.train$WranglerQueries,mydata.train$Unemployment)
cor(mydata.train$WranglerQueries,mydata.train$CPI.All)
cor(mydata.train$WranglerQueries,mydata.train$CPI.Energy)
summary(LR1a)
LR1b <- lm(WranglerSales ~ Unemployment + CPI.All + CPI.Energy, data=mydata.train)
summary(LR1b)
LR1c <- lm(WranglerSales ~ Unemployment + CPI.All,  data=mydata.train)
summary(LR1c)

# compute OSR^2

Predictions <- predict(LR1c, newdata=mydata.test)
# this builds a vector of predicted values on the test set
SSE = sum((mydata.test$WranglerSales - Predictions)^2)
SST = sum((mydata.test$WranglerSales - mean(mydata.train$WranglerSales))^2)
OSR2 = 1 - SSE/SST

# Part b)

LR2a <- lm(WranglerSales ~ Unemployment + WranglerQueries + CPI.All + CPI.Energy + MonthFactor, data=mydata.train)
summary(LR2a)
cor(mydata.train$WranglerQueries,mydata.train$MonthFactor)
cor(mydata.train$WranglerQueries,mydata.train$CPI.All)
cor(mydata.train$WranglerQueries,mydata.train$CPI.Energy)
LR2b <- lm(WranglerSales ~ Unemployment + CPI.All + CPI.Energy + MonthFactor, data=mydata.train)
summary(LR2b)
LR2c <- lm(WranglerSales ~ Unemployment + CPI.All + MonthFactor, data=mydata.train)
summary(LR2c)

# Part c)

LR3a <- lm(WranglerSales ~ Unemployment + WranglerQueries + CPI.All + CPI.Energy + MonthFactor, data=mydata.train)
summary(LR2a)
LR3b <- lm(WranglerSales ~ Unemployment + CPI.All + CPI.Energy + MonthFactor, data=mydata.train)
summary(LR3b)
LR3c <- lm(WranglerSales ~ Unemployment + CPI.Energy + MonthFactor, data=mydata.train)
summary(LR3c)

# Compute OSR^2
Predictions <- predict(LR3c, newdata=mydata.test)
# this builds a vector of predicted values on the test set
SSE = sum((mydata.test$WranglerSales - Predictions)^2)
SST = sum((mydata.test$WranglerSales - mean(mydata.train$WranglerSales))^2)
OSR2 = 1 - SSE/SST

# Part d)

LR4 <- lm(WranglerSales ~ Unemployment + USD + CPI.Energy + MonthFactor, data=mydata.train)
summary(LR4)

# Compute OSR^2
Predictions <- predict(LR4, newdata=mydata.test)
# this builds a vector of predicted values on the test set
SSE = sum((mydata.test$WranglerSales - Predictions)^2)
SST = sum((mydata.test$WranglerSales - mean(mydata.train$WranglerSales))^2)
OSR2 = 1 - SSE/SST




