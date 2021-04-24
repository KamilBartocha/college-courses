# Load data
data = read.table("student-mat.csv",sep=";",header=TRUE)
data_test <- data[0:100, ]
data_train <- data[100:395, ]
reg<-lm(data_train$G3~., data_train)
summary(reg)
plot(data_train$G3, reg$res)
shapiro.test(reg$res)
# cleaning data from rows containing G3 = 0
data_train = data_train[data_train$G3 != 0,]
# ToDo fix this below
##### men = data[data$sex != "F"]
##### men = mapvalues(data, data$sex)
##### women = data[data$sex != "M"]
# print(nrow(data)) # 382 students

#summary(data)
#boxplot(data)

reg<-lm(data_train$G3~., data_train)
summary(reg)
plot(data_train$G3, reg$res)
# we can see that famrel, absences, G1, G2, paid, health are essential
# Lets check residuals
# We dont see any dependence
shapiro.test(reg$res)
data_train_tmp = data_train[data_train$G3 != 0,]
# so we accept/reject hypothesis about normality of residuals
# Lest see qq plots of residuals 
qqnorm(reg$res)

# New model
new_reg <-lm(data_train$G3~data_train$famrel+data_train$absences+data_train$G1+data_train$G2+data_train$paid+data_train$health, data_train)
summary(new_reg)

# Lets check residuals
plot(data_train$G3, new_reg$res)
# We dont see any dependence
shapiro.test(new_reg$res)
# so we accept/reject hypothesis about normality of residuals
# Lest see qq plots of residuals 
qqnorm(new_reg$res)

# Lest check dependencies residuals of variables
plot(data$G3 ,new_reg$res)
abline(h=0)
plot(data$famrel ,new_reg$res)
abline(h=0)
plot(data$health ,new_reg$res)
abline(h=0)
plot(data$absences ,new_reg$res)
abline(h=0)
plot(data$G1 ,new_reg$res)
abline(h=0)
plot(data$G2 ,new_reg$res)
abline(h=0)
# We dont see any dependencies

# Lets check a smaller model with out variables G1 and G2
new_reg2 <-lm(data$G3~data$famrel+data$absences+data$paid+data$health, data)
summary(new_reg2)
# We see that, it is a bad idea


shapiro.test(reg$res)
shapiro.test(new_reg$residuals)
plot(data$G3, new_reg$residuals)
new_results = data$G3

# Lets check the model with only G1 and G2
new_reg_abs <-lm(data$G3~data$G1+data$G2, data)
summary(new_reg_abs)
# We can see that it is the best model, we can see that G2 is more essential variables that G1

# Check residuals 
plot(data$G3, new_reg_abs$res)
shapiro.test(new_reg_abs$res)
qqnorm(new_reg_abs$res)
# looks good

# Lets check the model with only G2
new_reg_g2 <-lm(data$G3~data$G2, data)
summary(new_reg_g2)
# ToDo write the results




# ***********************
#------   ANOVA   --------
# ***********************

# We will research if the sex has influence on the G3
# prepare data
grades = data$G1
sex = data$sex
# build ANOVA
anova.tool <- aov(grades~sex,data)
print(summary(anova.tool))
# we can see that sex has/hasn't a influence on the G3

# ToDo makes some plots
