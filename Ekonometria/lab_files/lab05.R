library(leaps)
library(faraway)

data(state)
?state
statedata<-data.frame(state.x77,
                      row.names=state.abb,
                      check.names=T)
View(statedata)
summary(statedata)
plot(statedata)
cor(statedata)
g <- lm(Life.Exp~., statedata)
summary(g)
# regresja krokowa wsteczna => -1 zmienna w korku
g<-update(g, .~.-Area)
# lub g <- lm(Life.Exp~. -Area, statedate)
summary(g)
g<-update(g, .~.-Illiteracy)
summary(g)
g<-update(g, .~.-Income)
summary(g)
g<-update(g, .~.-Population)
summary(g)
# R-squared  0.736 -> 0.7127 => we should keep this
# variable in model
g<-update(g,.~.+Population)
summary(g)


# AIC elimination method
g <- lm(Life.Exp~., statedata)
?step
step(g,direction="backward")
# BIC k = log(n)
step(g,direction="backward", k = log(50))
# both direction
step(g,direction="both", k = log(50))
g0 <- lm(Life.Exp~1, statedata)
step(g0, direction="forward",
     scope = c(upper = ~Population + Income + Illiteracy + 
               Murder + HS.Grad + Frost + Area, lower = ~1))
# C_p Mallow criteria
g <- lm(Life.Exp~., statedata)
?leaps
model.frame(g)
# x matrix of predictors, y - response vector
x <- model.frame(g)[,-1]
y <- model.frame(g)[,1]

leaps(x, y, method="Cp", names=names(statedata)[-4]) -> cp
cp
# Cpplot is avaliable in faraway lib
Cpplot(cp) # => 1456 - the best model, second 456(cp lowest)


leaps(x, y, method="adjr2", names=names(statedata)[-4]) -> adjr2
adjr2
maxadjr(adjr2, 10) # => 1,4,5,6 (0.713) it the best model

sort(hatvalues(g)) # => AK is the highest
x1<-x[state.abb!="AK",]
y1<-y[state.abb!="AK"]

leaps(x1, y1, method="adjr2", names=names(statedata)[-4]) -> adjr2_2
adjr2_2
maxadjr(adjr2_2, 10) # => 1,4,5,6, 7 (0.710) it the best model

par(mfrow=c(3,3)) # parameter 3x3 plots
for(i in 1:8)
  boxplot(statedata[,i],main=names(statedata)[i])

nx <- cbind(log(x[,1]), x[,2], log(x[,3]), x[,4:6], 
            log(x[,7]))

par(mfrow=c(3,3))
?apply
apply(nx, 2, boxplot)

leaps(nx, y, method="adjr2", names=names(statedata)[-4]) -> adjr2_3
adjr2_3
maxadjr(adjr2_3, 10)
