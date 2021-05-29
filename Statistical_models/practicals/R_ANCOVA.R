library(faraway)
data(sexab)          # getting the data frame sexab
attach(sexab)        # attaching object names of sexab
names(sexab)         # names of objects in sexab
head(sexab)
dim(sexab)
table(sexab$csa)

by(sexab, sexab$csa, summary)
plot(ptsd~csa, sexab)   # box plot ptsd for each csa group
plot(cpa~csa, sexab)    # box plot cpa for each csa group

plot(ptsd~cpa, pch=as.character(csa), sexab)
cor(ptsd, cpa) 

n1 <- 45
n2 <- 31
cor(ptsd[1:n1], cpa[1:n1]) 
cor(ptsd[(n1 + 1):(n1 + n2)], cpa[(n1 + 1):(n1 + n2)])

T <- sexab$csa 
levels(T) <- c("A","N") 
cor(ptsd, cpa) 
cor(ptsd[T=="A"], cpa[T=="A"]) 
cor(ptsd[T=="N"], cpa[T=="N"]) 

rslt_t <- lm(ptsd ~ cpa) 
plot(cpa, ptsd) 
rslt_t

abline(rslt_t, col="green")

par(mfrow=c(2,1)) 
rslt_A <- lm(ptsd[T=="A"] ~ cpa[T=="A"]) 
rslt_A

plot(cpa[T=="A"], ptsd[T=="A"]) 
abline(rslt_A, col="red") 

rslt_N <- lm(ptsd[T=="N"] ~ cpa[T=="N"]) 
rslt_N

plot(cpa[T=="N"], ptsd[T=="N"]) 
abline(rslt_N, col="blue")

par(mfrow=c(1,1)) 
plot(cpa, ptsd, pch=as.character(T))
abline(rslt_A, col="red")
abline(rslt_N, col="blue") 
abline(rslt_t, col="green") 

t.test(ptsd[T=="A"], ptsd[T=="N"])
t.test(ptsd[T=="A"], ptsd[T=="N"], var.equal=TRUE)

m1 <- lm(ptsd ~ cpa + csa + cpa:csa, sexab)
summary(m1)
model.matrix(m1)

m2 <- lm(ptsd ~ cpa + csa, sexab) 
summary(m2)

plot(ptsd~cpa, pch=as.character(csa)) # scatterplot
abline(10.248, 0.551, col="red") # regr. line `Abused'
abline(10.248-6.273, 0.551, col="blue") # `NotAbused'
abline(rslt_t, col="green") # total group

plot(fitted(m2), residuals(m2), pch=as.character(csa),
     xlab="Fitted", ylab="Residuals") 
plot(m2)

