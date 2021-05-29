library(olsrr)


p <- 4 # number of explanatory variables
2^p - 1

# All possible regression
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_step_all_possible(model)

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
k <- ols_step_all_possible(model)
plot(k)

# Best subset regression
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_step_best_subset(model)

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
k <- ols_step_best_subset(model)
plot(k)






# ---------------------------
# Stepwise FORWARD regression

# data: survival of patients undergoing liver operation
# bcs: blood clotting score
# pindex : prognostic index
# enzyme_test: enzyme function test score
# liver_test: liver function test score
# age: age, in years
# gender: indicator variable for gender (0 = male, 1 = female)
# alc_mod: indicator variable for history of alcohol use (0 = None, 1 = Moderate)
# alc_heavy: indicator variable for history of alcohol use (0 = None, 1 = Heavy)
# y: Survival Time

head(surgical)
dim(surgical)
p <- 8
2^p - 1

k <- ols_step_all_possible(model)
plot(k)

model <- lm(y ~ ., data = surgical)
summary(model)
ols_step_all_possible(model)

model <- lm(y ~ ., data = surgical)
ols_step_forward_p(model)

model <- lm(y ~ ., data = surgical)
k <- ols_step_forward_p(model)
plot(k)

# Clear Console
ols_step_forward_p(model, details = TRUE)


# ---------------------------
# Stepwise BACKWARD regression

model <- lm(y ~ ., data = surgical)
ols_step_backward_p(model)

model <- lm(y ~ ., data = surgical)
k <- ols_step_backward_p(model)
plot(k)

# Clear Console
ols_step_backward_p(model, details = TRUE)


# ---------------------------
# Stepwise Regression
model <- lm(y ~ ., data = surgical)
ols_step_both_p(model)

ols_step_forward_p(model)

ols_step_both_p(model, details = TRUE)

summary(model)
ols_step_both_p(model, pent = 0.05, prem = 0.1, details = FALSE)

#arguments:
# model:  Model to fit. Need to use "lm()" before you run "ols_stepwise()"
# pent: Threshold of the p-value used to enter a variable into the stepwise model. 
#       By default, 0.1
# prem: Threshold of the p-value used to exclude a variable into the stepwise model. 
#       By default, 0.3
# details: Print the details of each step




# ---------------------------
# Stepwise AIC Forward Regression

# -2logL + kp 
# L: the maximized value of the likelihood function of the model
# k: the number of parameters estimated by the model

# AIC: p = 2, BIC: p = log n

model <- lm(y ~ ., data = surgical)
ols_step_forward_aic(model)
k <- ols_step_forward_aic(model)
plot(k)

ols_step_forward_aic(model, details = TRUE)


# ---------------------------
# Stepwise AIC Backward Regression
model <- lm(y ~ ., data = surgical)
k <- ols_step_backward_aic(model)
k
plot(k)

ols_step_backward_aic(model, details = TRUE)


# ---------------------------
# Stepwise AIC Regression
model <- lm(y ~ ., data = surgical)
k <- ols_step_both_aic(model)
k
plot(k)
ols_step_both_aic(model, details = TRUE)