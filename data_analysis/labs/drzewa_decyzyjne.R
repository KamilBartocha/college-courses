library(C50)

# Entropia
curve(-x * log2(x) - (1 - x) * log2(1-x), lwd = 4,
      xlab = "x", ylab = "Entropy")
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
str(credit)

table(credit$checking_balance)
table(credit$savings_balance)

summary(credit$months_loan_duration)
summary(credit$amount)

table(credit$default)

#just for classes
RNGversion("3.5.2")
set.seed(123)
train_sample <- sample(1000, 900)
str(train_sample)

credit_train <- credit[train_sample, ]
credit_test  <- credit[-train_sample, ]

prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

# -17 bo minus cechy sprytne! 
credit_model <- C5.0(credit_train[-17], credit_train$default)
credit_model
summary(credit_model)
