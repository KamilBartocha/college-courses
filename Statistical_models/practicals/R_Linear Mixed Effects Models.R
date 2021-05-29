######################################
#                                    #
#   Mixed Effects Modelling in R     #
#                                    #
######################################

# Based on work developed by Gabriela Hajduk and Liam Bailey




# ---- Inspect the data ----

# load the data and have a look at it

load("dragons.RData")
head(dragons)

hist(dragons$testScore)  
hist(dragons$testScore, breaks = 20)  
dragons$bodyLength2 <- scale(dragons$bodyLength)


# Question: Is test score affected by body length?

# ---- Fit all data in one analysis ----

library(lme4)

model1 <- lm(testScore ~ bodyLength, data = dragons)
summary(model1)

model2 <- lm(testScore ~ bodyLength2, data = dragons)
summary(model2)

y.new <- function(beta0.hat, beta1.hat, x.new) {
  y.new <- beta0.hat + beta1.hat*x.new 
  print(y.new)
}

x.new <- mean(dragons$bodyLength)
beta0.hat <- as.numeric(model1$coefficients[1])
beta1.hat <- as.numeric(model1$coefficients[2])
y.new(beta0.hat, beta1.hat, x.new)

x.new <- -1
beta0.hat <- as.numeric(model2$coefficients[1])
beta1.hat <- as.numeric(model2$coefficients[2])
y.new(beta0.hat, beta1.hat, x.new)


library(ggplot2)
ggplot(dragons, aes(x = bodyLength, y = testScore)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()


# Diagnostic Checking

# Constancy of Variance
plot(model2, which = 1)

# Normality
plot(model2, which = 2)

shapiro.test(scale(model1$residuals))
shapiro.test(scale(model2$residuals))

# Independence
boxplot(testScore ~ mountainRange, data = dragons)  

ggplot(dragons, aes(x = bodyLength, y = testScore, 
                    colour = mountainRange)) +
  geom_point(size = 2) +
  theme_classic() +
    theme(legend.position = "none")

# It looks like our mountain ranges vary both in the dragon body length
# and in their test scores. This confirms that our observations from 
# within each of the ranges aren't independent. We can't ignore that.



# ---- Run multiple analyses ----

# Data split by mountain range

ggplot(aes(bodyLength, testScore), data = dragons) + geom_point() +
    facet_wrap(~ mountainRange) +
    xlab("length") + ylab("test score") + theme_bw()

require(lattice)
xyplot(testScore ~ bodyLength | mountainRange, dragons, 
       layout = c(4, 2), type = c("g","p","r"),
       index = function(x,y) coef(lm(y ~ x))[1],
       xlab = "body length",
       ylab = "test score")


# ---- Modify the model ----

model3 <- lm(testScore ~ bodyLength2 + mountainRange, 
                  data = dragons)
summary(model3)

# We are not interested in quantifying test scores for each 
# specific mountain range: we just want to know whether body 
# length affects test scores and we want to simply control 
# for the variation coming from mountain ranges.



# ---- Mixed Effects models ----

# Random intercept with fixed mean
model4 <- lmer(testScore ~ 1 + (1|mountainRange), 
               data = dragons)
summary(model4)


aggregate(dragons$testScore, list(dragons$mountainRange), FUN=mean)
mean(dragons$testScore)

y <- aggregate(dragons$testScore, list(dragons$mountainRange), FUN=mean)[2]
z <- ranef(model4)$mountainRange
w <- y - z
cbind(z, y, w)
mean(w$x)


# Q: While we still want to know whether there is an association 
#    between dragon's body length and the test score, we want to 
#    know if that association exists after controlling for the 
#    variation in mountain ranges.

model5 <- lmer(testScore ~ bodyLength2 + (1|mountainRange), 
                   data = dragons)
summary(model5)

# The random effect part tells you how much variance you
# find among levels of the mountain ranges, plus the residual variance

# The random effect of the mountain range is meant to capture all 
# the influences of mountain ranges on dragon test scores --- 
# whether we observed those influences explicitly or not, 
# whether those influences are big or small etc. 

# It could be many, many teeny-tiny influences that, when combined, 
# affect the test scores and that's what we are hoping to control for.

plot(model5, which = 1)
qqnorm(resid(model5))
qqline(resid(model5))
shapiro.test(resid(model5))

# Incorrect Nested Model
# Treats the two random effects as if they are crossed
model6 <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + 
                 (1|site), data = dragons)  

summary(model6)

# Correct Nested Model
# Syntax stays the same, but now the nesting is taken into account
dragons <- within(dragons, sample <- factor(mountainRange:site))
model7 <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + 
                 (1|sample), data = dragons)  
summary(model7)


(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, 
                               colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +   
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(model7)), 
              aes(y = pred), size = 1) +   
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines")))

library(ggeffects)

# Extract the prediction data frame
# This gives overall predictions for the model
pred.mm <- ggpredict(model7, terms = c("bodyLength2"))  

(ggplot(pred.mm) + 
    geom_line(aes(x = x, y = predicted)) +
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +
    geom_point(data = dragons,
               aes(x = bodyLength2, y = testScore, colour = mountainRange)) + 
    labs(x = "Body Length (indexed)", y = "Test Score", 
         title = "Body length does not affect intelligence in dragons") + 
    theme_minimal()
)

# Random Slopes Model
model8 <- lmer(testScore ~ 1 + 
                 (1 + bodyLength2|mountainRange/site), 
               data = dragons) 
summary(model8)


model9 <- lmer(testScore ~ bodyLength2 + 
                 (1 + bodyLength2|mountainRange/site), 
               data = dragons) 
summary(model9)

(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +   
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(model9)), 
              aes(y = pred), size = 1) +   
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))
)

library(sjPlot)

# Visualise random effects 
ranef(model9)
(re.effects <- plot_model(model9, type = "re", show.values = TRUE))
summary(model9)

(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(model9)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)

library(stargazer)
stargazer(model1, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")


# Variable Selection
full.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), 
                  data = dragons, REML = FALSE)
reduced.lmer <- lmer(testScore ~ 1 + (1|mountainRange) + (1|sample), 
                     data = dragons, REML = FALSE)
anova(reduced.lmer, full.lmer)




##########################################################
str(sleepstudy)

# Inspect the data
# The average reaction time per day for subjects in a 
# sleep deprivation study. On day 0 the subjects had their 
# normal amount of sleep. Starting that night they were 
# restricted to 3 hours of sleep per night. The observations 
# represent the average reaction time on a series of tests 
# given each day to each subject.

# Reaction - Average reaction time (ms)
# Days - Number of days of sleep deprivation
# Subject - Subject number on which the observation was made.

require(lattice)
xyplot(Reaction ~ Days | Subject, sleepstudy, type = c("g","p","r"),
       index = function(x,y) coef(lm(y ~ x))[1],
       xlab = "Days of sleep deprivation",
       ylab = "Average reaction time (ms)", aspect = "xy")

# Model With Correlated Random Effects
model1 <- lmer(Reaction ~ 1 + Days + (1 + Days|Subject), 
               sleepstudy, REML = FALSE)
summary(model1)
head(ranef(model1)[["Subject"]])

# Interpretations:
# average initial reaction time (i.e. without sleep deprivation)
# in the population

# average increase in reaction time per day of sleep deprivation

# estimated subject-to-subject variation in the intercept 
# corresponds to a standard deviation of about 25 ms

# estimated subject-to-subject variation in the slope 
# corresponds to a standard deviation of about 6 ms/day

# little evidence of a systematic relationship 
# between these quantities; observing a subject's initial
# reaction time does not give us much information for 
# predicting whether their reaction time will be 
# strongly affected by each day of sleep deprivation or not



# Model With Uncorrelated Random Effects
model2 <- lmer(Reaction ~ 1 + Days + (1|Subject) + 
                 (0+Days|Subject), sleepstudy, REML = FALSE)
summary(model2)
head(ranef(model2)[["Subject"]])

anova(model1, model2)


# Other models:
# Random intercept with fixed mean without covariates
model3 <- lmer(Reaction ~ 1 + (1|Subject), sleepstudy, REML = FALSE)
summary(model3)

# Random intercept with fixed mean with covariates
model4 <- lmer(Reaction ~ 1 + Days + (1|Subject), sleepstudy, REML = FALSE)
summary(model4)


# If you have time to prepare for examples on logistic regression that
# would be helpful. In particular confidence intervals for

#    Pr(Y=1 for a given set of covariates)
#    log odds
