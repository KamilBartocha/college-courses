library(rpart)
library(rpart.plot)
library(Cubist)
# TASK 1 
cT <- c(1,1,1,2,2,3,4,5,5,6,6,7,7,7,7)
c1<- c(1,1,1,2,2,3,4)
sd(c1)
c2 <- c(5,5,6,6,7,7,7,7)
sd(cT)
sum1 <- 7/15 * sd(c1)
sum1
sum2 <- 8/15 * sd(c2)
sum2
SDR <- sd(cT) - sum1 -sum2
SDR
# > SDR1
# [1] 1.202815

# > SDR2
# [1] 1.392751
wine <- read.csv("whitewines.csv")
str(wine)

hist(wine$quality)
summary(wine)
wine_train <- wine[1:3750, ]
wine_test  <- wine[3751:3898, ]

# m <- rpart(dv ~ iv, data = mydata)
m.rpart <- rpart(quality ~ ., data = wine_train)
m.rpart
summary(m.rpart)

rpart.plot(m.rpart)

p.rpart <- predict(m.rpart, wine_test)
summary(p.rpart)
summary(wine_test$quality)

cor(p.rpart, wine_test$quality)

MAE <- function(actual, predicted){
  mean(abs(actual - predicted))
}

MAE(p.rpart, wine_test$quality)
mean(wine_train$quality)

MAE(5.87, wine_test$quality)
m.cubist <- cubist(x = wine_train[-12], y=wine_train$quality)
m.cubist
summary(m.cubist)

p.cubist <- predict(m.cubist, wine_test)
summary(p.cubist)

cor(p.cubist, wine_test$quality)
MAE(wine_test$quality, p.cubist)
