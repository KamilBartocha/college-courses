#y = mx + b
#y = beta0 + beta1 x + epsilon


n <- 100
p <- 2
beta0 <- 1
beta1 <- 0
x <- rnorm(n, mean = 10, sd = 1)
epsilon <- rnorm(n, mean = 0, sd = 1)
y <- beta0 + beta1*x + epsilon

fit <- lm(y ~ x)
summary(fit)

library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

summary(income.data)
hist(income.data$happiness)
plot(happiness ~ income, data = income.data)
income.happiness.lm <- lm(happiness ~ income, data = income.data)
summary(income.happiness.lm)
par(mfrow=c(2,2))
plot(income.happiness.lm)
par(mfrow=c(1,1))


## Plot the data points on the graph
income.graph<-ggplot(income.data, aes(x=income, y=happiness))+
  geom_point()
income.graph

## Add the linear regression line to the plotted data
income.graph <- income.graph + geom_smooth(method="lm", col="black")
income.graph

income.graph +
  theme_bw() +
  labs(title = "Reported happiness as a function of income",
       x = "Income (x$10,000)",
       y = "Happiness score (0 to 10)")
