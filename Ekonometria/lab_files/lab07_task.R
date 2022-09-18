library(faraway)
data(chmiss)
View(chmiss)
attach(chmiss)
summary(chmiss)

reg <- lm(involact ~ ., chmiss)
summary(reg)
dim(chmiss) # 20 obs deleted due to N/A values

# change N/A values to mean
avg <- apply(chmiss, 2, mean, na.rm=T)
avg
chm <- chmiss
for(i in c(1:4,6)){
  chm[is.na(chm[,i]),i] <- avg[i]
}
chm
summary(chm)
reg1 <- lm(involact ~ ., chm)
summary(reg1)
# only 3 missing values in involact var

gr <- lm(race ~ fire+theft+age+income, chmiss)
summary(gr)
predict(gr, chmiss[is.na(chmiss$race), ]) 

# -17% - need to be tranformed
gr <- lm(logit(race / 100) ~ fire+theft+age+income,
         chmiss)
summary(gr)
ilogit(predict(gr,chmiss[is.na(chmiss$race),]))*100

data(chicago)
chicago$race[is.na(chmiss$race)]
