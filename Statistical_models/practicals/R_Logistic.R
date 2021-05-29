#A. Logistic Regression 

#- Test of Effects 
#- Form CIs for log odds
#- Form CIs for probability of an event given x 



# Recall from 29 April 2021 Lecture:

# An important concept to understand for interpreting the 
# logistic regression coefficients, is the ODDS RATIO. 

# An odds ratio measures the association between a 
# predictor variable (x) and the outcome variable (y). 






#######################################################
library(tidyverse)
library(caret)
theme_set(theme_bw())

data("PimaIndiansDiabetes2", package = "mlbench")

# The data set PimaIndiansDiabetes2 contains a corrected 
# version of the original data set. While the UCI Repository 
# of Machine Learning Databases index claims that 
# there are no missing values, closer inspection 
# of the data shows several physical impossibilities, 
# e.g., blood pressure or body mass index of 0.

# In PimaIndiansDiabetes2, all zero values of glucose, pressure, 
# triceps, insulin and mass have been set to NA, 
# see also Wahba et al (1995) and Ripley (1996).

PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)

# Inspect the data
sample_n(PimaIndiansDiabetes2, 3)

# pregnant	- Number of times pregnant
# glucose   - Plasma glucose concentration (glucose tolerance test)
# pressure	- Diastolic blood pressure (mm Hg)
# triceps   - Triceps skin fold thickness (mm)
# insulin   - 2-Hour serum insulin (mu U/ml)
# mass	    - Body mass index (weight in kg/(height in m)^2)
# pedigree	- Diabetes pedigree function
# age       - Age (years)
# diabetes	- Class variable (test for diabetes)


# Split the data into training and test set
set.seed(123)

training.samples <- PimaIndiansDiabetes2$diabetes %>% 
  createDataPartition(p = 0.8, list = FALSE)

train.data  <- PimaIndiansDiabetes2[training.samples, ]
test.data   <- PimaIndiansDiabetes2[-training.samples, ]

# Fit the model (All Covariates)
model <- glm( diabetes ~ ., data = train.data, family = binomial)

# Summarize the model
summary(model)

# Make predictions
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")

# Model accuracy
mean(predicted.classes == test.data$diabetes)
table(predicted.classes, test.data$diabetes)


# Simple Logistic Regression Model
model <- glm( diabetes ~ glucose, data = train.data, family = binomial)
summary(model)
summary(model)$coef

beta0hat <- summary(model)$coef[1,1]
beta1hat <- summary(model)$coef[2,1]

prob <- function(x, beta0hat, beta1hat) {
  diabetes.pos <- exp(beta0hat + beta1hat*x)/(1 + exp(beta0hat + beta1hat*x))
  diabetes.neg <- 1/(1 + exp(beta0hat + beta1hat*x))
  if (diabetes.neg + diabetes.pos == 1) {
    return(diabetes.pos)
  } else {
    print("Please check for errors.")
  }
}

print(prob(x = 89, beta0hat = beta0hat, beta1hat = beta1hat))

newdata <- data.frame(glucose = c(20,  180))
probabilities <- model %>% predict(newdata, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
predicted.classes

train.data %>%
  mutate(prob = ifelse(diabetes == "pos", 1, 0)) %>%
  ggplot(aes(glucose, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "Plasma Glucose Concentration",
    y = "Probability of being diabete-pos"
  )


# Multiple Logistic Regression Model
model <- glm( diabetes ~ glucose + mass + pregnant, 
              data = train.data, family = binomial)
summary(model)$coef

model <- glm( diabetes ~., data = train.data, family = binomial)
summary(model)$coef
summary(model)

model <- glm( diabetes ~ glucose + mass + pedigree, 
              data = train.data, family = binomial)
summary(model)$coef
exp(summary(model)$coef[3,1])

# Interpretation: 
# This indicate that one unit increase in the glucose 
# concentration will increase the odds of being positive 
# by 1.04 times.

# Making Predictions
probabilities <- model %>% predict(test.data, type = "response")
head(probabilities)

predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)
mean(predicted.classes == test.data$diabetes)





#######################################################
library(effects)

# Inspect the data
# neuroticism  - scale from Eysenck personality inventory
# extraversion - scale from Eysenck personality inventory
# sex          - a factor with levels: female; male
# volunteer    - volunteering, a factor with levels: no; yes

# Neuroticism or emotionality is characterized by high 
# levels of negative affect such as depression and anxiety. 


head(Cowles)
model.cowles <- glm(volunteer ~ sex + neuroticism*extraversion,
                    data = Cowles, family=binomial)
summary(model.cowles)
plot(allEffects(model.cowles))

# Females have a higher expected probability 
# of volunteering than males. 

e.out <- allEffects(model.cowles)
e.out$sex$model.matrix

mean(Cowles$neuroticism)
mean(Cowles$extraversion)
mean(Cowles$neuroticism) * mean(Cowles$extraversion) # interaction

invLogit <- function(x) {
  exp(x)/(1 + exp(x))
}
invLogit(e.out$sex$model.matrix %*% coef(model.cowles))

# Making predictions with new data
ndata <- data.frame(sex=factor(c("female","male")), 
                    neuroticism=11.47009, extraversion=12.37298)
predict(object = model.cowles, newdata = ndata, type = "response")

# Neuroticism * Extraversion effect plot 
allEffects(model.cowles)
head(e.out$`neuroticism:extraversion`$model.matrix)
prop.table(table(Cowles$sex))

plot(allEffects(model.cowles, 
                xlevels=list(extraversion=seq(10, 20, 2), neuroticism=10:20),
                given.values=c(sexmale=1)))
plot(Effect(focal.predictors = c("neuroticism","extraversion"), mod = model.cowles,
            xlevels=list(extraversion=seq(10, 20, 2), neuroticism=10:20),
            given.values=c(sexmale=1)))
plot(Effect(focal.predictors = c("neuroticism","extraversion","sex"), mod = model.cowles,
            xlevels=list(extraversion=seq(10, 20, 2), neuroticism=10:20)))
plot(Effect(focal.predictors = c("neuroticism","extraversion","sex"), mod = model.cowles,
            xlevels=list(extraversion=seq(10, 20, 2), neuroticism=10:20)),
     multiline = TRUE)



#######################################################
library(data.table)
library(plyr)
library(stringr)
train <- fread("train.csv", na.strings = c(""," ",NA,"NA"),
               strip.white = TRUE)
test <- fread("test.csv",na.strings = c(""," ",NA,"NA"), 
              strip.white = TRUE)

head(train)
head(test)
str(train)

#check missing values
colSums(is.na(train))
colSums(is.na(test))

#Quick Data Exploration
summary(train$Age)
summary(test$Age)

train[,.N/nrow(train),Pclass]
test[,.N/nrow(test),Pclass]

train [,.N/nrow(train),Sex]
test [,.N/nrow(test),Sex]

train [,.N/nrow(train),SibSp]
test [,.N/nrow(test),SibSp]

train [,.N/nrow(train),Parch]
test [,.N/nrow(test),Parch]

summary(train$Fare)
summary(test$Fare)

train [,.N/nrow(train),Cabin]
test [,.N/nrow(test),Cabin]

train [,.N/nrow(train),Embarked] 
test [,.N/nrow(test),Embarked]

alldata <- rbind(train,test,fill=TRUE)

#Extract passengers title
alldata [,title := strsplit(Name,split = "[,.]")]
alldata [,title := ldply(.data = title,.fun = function(x) x[2])]
alldata [,title := str_trim(title,side = "left")]

#combine titles
alldata [,title := replace(title, which(title %in% c("Capt","Col","Don","Jonkheer","Major","Rev","Sir")), "Mr"),by=title]
alldata [,title := replace(title, which(title %in% c("Lady","Mlle","Mme","Ms","the Countess","Dr","Dona")),"Mrs"),by=title]

#ticket binary coding
alldata [,abs_col := strsplit(x = Ticket,split = " ")]
alldata [,abs_col := ldply(.data = abs_col,.fun = function(x)length(x))]
alldata [,abs_col := ifelse(abs_col > 1,1,0)]

#Impute Age with Median
for(i in "Age")
  set(alldata,i = which(is.na(alldata[[i]])),j=i,value = median(alldata$Age,na.rm = T))

#Remove rows containing NA from Embarked
alldata <- alldata[!is.na(Embarked)]

#Impute Fare with Median
for(i in "Fare")
  set(alldata,i = which(is.na(alldata[[i]])),j=i,value = median(alldata$Fare,na.rm = T))

#Replace missing values in Cabin with "Miss"
alldata [is.na(Cabin),Cabin := "Miss"]

#Log Transform Fare
alldata$Fare <- log(alldata$Fare + 1)

#Impute Parch 9 to 0
alldata [Parch == 9L, Parch := 0]

#Collect train and test
train <- alldata[!(is.na(Survived))]
train [,Survived := as.factor(Survived)]

test <- alldata[is.na(Survived)]
test [,Survived := NULL]

#Logistic Regression
model <- glm(Survived ~ ., family = binomial(link = 'logit'), 
             data = train[,-c("PassengerId","Name","Ticket")])
summary(model)
anova(model, test = 'Chisq')
model2 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Fare + title, 
              data = train,family = binomial(link="logit"))
summary(model2)
anova(model,model2,test = "Chisq")

#partition and create training, testing data
library(caret)
split <- createDataPartition(y = train$Survived,p = 0.6,list = FALSE)

new_train <- train[split] 
new_test <- train[-split]

#model training and prediction
log_model <- glm(Survived ~ Pclass + Sex + Age + SibSp + Fare + title, 
                 data = new_train[,-c("PassengerId","Name","Ticket")],
                 family = binomial(link="logit"))
log_predict <- predict(log_model,newdata = new_test,type = "response")
log_predict <- ifelse(log_predict > 0.5,1,0)

#plot Receiver Operating Characteristic (ROC) curve
library(ROCR) 
library(Metrics)
pr <- prediction(log_predict,new_test$Survived)
perf <- performance(pr,measure = "tpr",x.measure = "fpr") 
plot(perf)
auc(new_test$Survived,log_predict)

log_predict <- predict(log_model,newdata = new_test,type = "response")
log_predict <- ifelse(log_predict > 0.6,1,0)

pr <- prediction(log_predict,new_test$Survived)
perf <- performance(pr,measure = "tpr",x.measure = "fpr")
plot(perf)
auc(new_test$Survived,log_predict) 