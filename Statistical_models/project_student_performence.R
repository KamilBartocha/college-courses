# Load data
data = read.table("student-mat.csv",sep=";",header=TRUE)
data = data[data$G3 != 0,]
# ToDo fix this below
men = data[data$sex != "F"]
men = mapvalues(data, data$sex)
women = data[data$sex != "M"]
# print(nrow(data)) # 382 students


#summary(data)
#boxplot(data)

# regression, whole model
reg<-lm(data$G3~., data)
summary(reg)
# we can see that famrel, absences, G1, G2, paid, health are essential
# Lets check resiuduals
plot(data$G3, reg$res)
# We dont see any dependence
shapiro.test(new_reg$res)
# so we accept/reject hipotes about normality of residuals
# Lest see qq plots of residuals 
qqnorm(new_reg$res)

# New model
new_reg <-lm(data$G3~data$famrel+data$absences+data$G1+data$G2+data$paid+data$health, data)
summary(new_reg)

# Lets check resiuduals
plot(data$G3, new_reg$res)
# We dont see any dependence
shapiro.test(new_reg$res)
# so we accept/reject hipotes about normality of residuals
# Lest see qq plots of residuals 
qqnorm(new_reg$res)

# Lest chceck dependensis residuals of variebals
plot(data$G3 ,new_reg$res)
abline(h=0)
plot(data$paid ,new_reg$res)
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

# Lets ckeck a smaller model with out variebles G1 nad G2
new_reg2 <-lm(data$G3~data$famrel+data$absences+data$paid+data$health, data)
summary(new_reg2)
# We see that, it is a bad idea


# this inside the ===== we can delate
#==============================
plot(data$G3, new_reg2$res)
shapiro.test(new_reg2$res)
qqnorm(new_reg2$res)


plot(data$G3 ,new_reg2$res)
abline(h=0)
plot(data$paid ,new_reg2$res)
abline(h=0)
plot(data$famrel ,new_reg2$res)
abline(h=0)
plot(data$health ,new_reg2$res)
abline(h=0)
plot(data$absences ,new_reg2$res)
abline(h=0)

#=================================
shapiro.test(reg$res)
shapiro.test(new_reg$residuals)
plot(data$G3, new_reg$residuals)
new_results = data$G3

# Ltes check the model with only G1 and G2
new_reg_abs <-lm(data$G3~data$G1+data$G2, data)
summary(new_reg_abs)
# We can see that it is the best model, we can se taht G2 is more esential variebals that G1

# Check residuals 
plot(data$G3, new_reg_abs$res)
shapiro.test(new_reg_abs$res)
qqnorm(new_reg_abs$res)
# looks good

# Lets chek the model wuth onyly G2
new_reg_g2 <-lm(data$G3~data$G2, data)
summary(new_reg_g2)
# ToDo   compile and write the results




# ***********************
#------   ANOVA   --------
# ***********************

# We will research if the sex has influnce on the G3
# prepear data
grades = data$G1
sex = data$sex
# bulid ANOVA
anova.tool <- aov(grades~sex,data)
print(summary(anova.tool))
# we can see that sex has/hasent a influence on the G3


# ToDo makes some plots
