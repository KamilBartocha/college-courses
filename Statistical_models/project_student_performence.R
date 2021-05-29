#some functions :)
check_anova <- function(result, variable, var_name, res_name="Final Grade"){
  anova <- aov(result~variable, data.frame(result, variable))
  if (unlist(summary(anova))["Pr(>F)1"] < 0.05) {
    print("SUMMARY OF ANOVA FOR:")
    print(var_name)
    print(summary(anova))
    try({
      plot(variable, result, main=var_name)}, silent = TRUE)
  }
}

do_anova <- function(result, variable, var_name, res_name="Final Grade"){
  print("==================================================")
  print("==================================================")
  print("==================================================")
  
  print("VARIABLE NAME:")
  print(var_name)
  
  print("SUMMARY:")
  print(summary(variable))
  
  print("BOX PLOTS:")
  plot(factor(variable), result, xlab=var_name, ylab=res_name)
  try({
    print("HISTOGRAM FOR")
    print(var_name)
    hist(variable, main=var_name)}, silent = TRUE)
  
  try({
    print("BARPLOT FOR")
    print(var_name)
    barplot(prop.table(table(variable)), main=var_name)}, silent = TRUE)
  

  print("Start ANOVA for:")
  print(res_name)
  print("explained by:")
  print(var_name)

  anova <- aov(result~variable, data.frame(result, variable))
  
  print("SUMMARY OF ANOVA")
  print(summary(anova))
  
  lr <-lm(result~variable, data.frame(result, variable))
  
  print("SUMMARY OF LINEAR MODEL")
  print(summary(lr))
  print("RESIDUAL PLOTS")
  plot(lr$residuals, main=var_name)
  print("Shapiro normality test")
  print(shapiro.test(lr$residuals))
}

do_anova_for_each_column <- function(result,
                                     data,
                                     res_name="Final Grade",
                                     no_zero_res_name="Final Grade with out 0"){
  for (name in colnames(data)){
    do_anova(result, factor(data[[name]]), name, res_name)
  }
}


do_check_anova <- function(result, data, res_name="Final Grade"){
  for (name in colnames(data)){
    check_anova(result, factor(data[[name]]), name, res_name)
  }
}

# Load data
data = read.table("student-mat.csv",sep=";",header=TRUE)
no_zero_data <- data[data$G3 != 0,]
no_zero_data <- no_zero_data[no_zero_data$G1 != 0,]
imp_data  <- no_zero_data[no_zero_data$G1<10,]
improve <- imp_data$G3 - imp_data$G1
improve <-ifelse(improve + imp_data$G1 > 9,1,0)
print(improve)

fg <- no_zero_data$G3
firstg <- no_zero_data$G1
plot(no_zero_data$absences , fg, name="absence")

no_res = subset(no_zero_data, select = -c(G1, G2, G3))
do_check_anova(improve, imp_data)


reg <- lm(improve~imp_data$age + imp_data$nursery, imp_data)
summary(reg)
do_anova_for_each_column(fg, no_zero_fg, no_res, no_zero_res)


do_check_anova(improve, imp_data, res_name = "Improvment")
do_anova_for_each_column(improve, imp_data, res_name = "Improvment")
summary(improve)
plot(improve, imp_data$romantic)

#====================================================
#PART 2 ALL possible regession models for G3 (finay grade)
#====================================================
library(olsrr)
model <- lm(no_zero_data$G3 ~ no_zero_data$address + no_zero_data$Medu
            + no_zero_data$Fedu + no_zero_data$Mjob + no_zero_data$Fjob
            + no_zero_data$studytime + no_zero_data$higher
            + no_zero_data$internet + no_zero_data$goout + no_zero_data$Walc
            + no_zero_data$absences,
            no_zero_data)

models <- ols_step_all_possible(model)

plot(models)

best_subset <- ols_step_best_subset(model)
print(best_subset)

plot(best_subset)

last_model <- lm(no_zero_data$G3 ~no_zero_data$Medu
                           + no_zero_data$Mjob + no_zero_data$Fjob
                          + no_zero_data$studytime
                          + no_zero_data$internet + no_zero_data$goout
                          + no_zero_data$absences,
                          no_zero_data)
summary(last_model)

#====================================================
#PART 2 ALL possible regession models for improvment (binary)
#====================================================

imp_data
model <- lm(improve ~ imp_data$failures + imp_data$romantic+
            imp_data$goout,
            data.frame(imp_data, improve))

summary(model)

models <- ols_step_all_possible(model)

plot(models)

best_subset <- ols_step_best_subset(model)

plot(best_subset)
print(best_subset)

last_model <-lm(improve ~imp_data$romantic+
                  imp_data$goout,
                data.frame(imp_data, improve))
summary(last_model)





















plotsPath = "C:\Users\baranek\Documents\R projects\COMPUTING\Math_WMS_courses\Statistical_models\allPlots.pdf"
pdf(file=plotsPath)  

for (x in seq(1,100))   
{   
  par(mfrow = c(2,1))
  p1=rnorm(x)  
  p2=rnorm(x)  
  plot(p1,p2)   
} 
dev.off() 




dataw0 = data[data$G3 != 0,]
summary(data)
attach(data)

#plot(age, G3)
#plot(factor(school), G3, xlab = "School", ylab="Final Grade")
plot(factor(sex), G3, xlab = "Sex", ylab="Final Grade")
#plot(factor(age), G3, xlab = "Age", ylab="Final Grade")
#plot(factor(address), G3, xlab = "address", ylab="Final Grade")
#plot(factor(famsize), G3, xlab = "famsize", ylab="Final Grade")
plot(factor(Pstatus), G3, xlab = "Living with Parents", ylab="Final Grade")
plot(factor(Medu), G3, xlab = "Mother's education", ylab="Final Grade")
plot(factor(dataw0$Fedu), dataw0$G3, xlab = "Father's education", ylab="Final Grade")
plot(factor(Mjob), G3, xlab = "Mother's job", ylab="Final Grade")
plot(factor(Fjob), G3, xlab = "Father's job", ylab="Final Grade")
#(factor(reason), G3, xlab = "School choose reason", ylab="Final Grade")
#plot(factor(guardian), G3, xlab = "Guardian", ylab="Final Grade")
#plot(factor(traveltime), G3, xlab = "Home to School Travel Time", ylab="Final Grade")
plot(factor(studytime), G3, xlab = "Study time", ylab="Final Grade")
###
# Rest To Be Done ^^^^^
###

data = subset(data, select=-c(school, age, address, famsize, Pstatus,
                              reason, failures, G2, guardian, traveltime,
                              nursery, romantic, age))

#=======================================================
#   ANOVA
#=======================================================
# SEX
do_anova <- function(result, variable, var_name, res_name="Final Grade"){
  print("Start ANOVA for:")
  print(res_name)
  print("explained by:")
  print(var_name)
  print("BOX PLOTS:")
  plot(factor(variable), result, xlab=var_name, ylab=res_name)
  anova <- aov(result~variable, data.frame(result, variable))
  print("SUMMARY OF ANOVA")
  print(summary(anova))
  lr <-lm(result~variable, data.frame(result, variable))
  print("SUMMARY OF LINEAR MODEL")
  print(summary(lr))
  print("RESIDUAL PLOTS")
  plot(lr$residuals)
  print("Shapiro normality test")
  print(shapiro.test(lr$residuals))
}

do_anova_for_each_column <- function(result, data, res_name="Final Grade"){
  for (name in colnames(data)){
    print(class(df[[i]]))
    do_anova(result, factor(data[[name]]), name, res_name)
  }
}








sex <- dataw0$sex
G3 <- dataw0$G3
do_anova(G3, sex, "SEX", "improve")
anova.sex <- aov(G3~sex, data.frame(sex, G3))
summary(anova.sex)
plot(anova.sex)
sex_lr <-lm(G3~sex, data.frame(sex, G3))
summary(sex_lr)
plot(sex_lr$residuals)
shapiro.test(sex_lr$residuals)
#=======================================================
#Father Job
ft <- dataw0$Fjob
result = dataw0$G3
ft[ft != "teacher"] <- 0
ft[ft == "teacher"] <- 1

anova.tech <- aov(result~ft, data.frame(ft, result))
summary(anova.tech)

sex_lr <-lm(result~ft, data.frame(result, ft))
summary(sex_lr)




plot(factor(sex), G3, xlab = "Sex", ylab="Final Grade")
plot(factor(sex), G3, xlab = "Sex", ylab="Final Grade")

plot(G3, factor(school))
plot(G3, factor(school))

hist(school)
sc(school, G3)
plot(sex, G3)
plot(age, G3)
plot(address, G3)
plot(famsize, G3)
plot(Pstatus, G3)
plot(Medu, G3)


plot(data$absences, data$G3)
summary(data$absences)
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


<<<<<<< HEAD
# this inside the ===== we can delete
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
=======
>>>>>>> a5c3e2a527e1a093e12f6fb1f57c6858b628a4c7
shapiro.test(reg$res)
shapiro.test(new_reg$residuals)
plot(data$G3, new_reg$residuals)
new_results = data$G3

# Lets check the model with only G1 and G2
new_reg_abs <-lm(data$G3~data$G1+data$G2, data)
summary(new_reg_abs)
<<<<<<< HEAD
# We can see that it is the best model, we can see that G2 is more essential variebals that G1
=======
# We can see that it is the best model, we can see that G2 is more essential variables that G1
>>>>>>> a5c3e2a527e1a093e12f6fb1f57c6858b628a4c7

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
<<<<<<< HEAD

=======
>>>>>>> a5c3e2a527e1a093e12f6fb1f57c6858b628a4c7

# ToDo makes some plots
