blood <- read.table("blood.txt")
attach(blood)
View(blood)
summary(blood)
plot(blood)

reg <- lm(pressure ~ age, blood)
summary(reg)
abline(reg)
plot(reg$res~age)
abline(h=0)
sort(age)
grupa <- rep(1, 54)
grupa[30 <= age&age < 40] <- 2
grupa[40 <= age&age < 50] <- 3
grupa[50 <= age] <- 4

v1 <- var(reg$res[grupa==1])
v2 <- var(reg$res[grupa==2])
v3 <- var(reg$res[grupa==3])
v4 <- var(reg$res[grupa==4])

wariancja <- c(v1, v2, v3, v4)
wariancja

# srodek każdej z grup wiekowych
srodek <- c(25, 35, 45, 55)
zal <- data.frame(srodek, wariancja)
zal <- cbind(zal, v.x = wariancja/srodek,
                  v.x2 = wariancja/srodek^2,
                  v.sqrt.x = wariancja/sqrt(srodek))
# zal

waga = rep(1:54)
waga[grupa==1] <- 1/v1
waga[grupa==2] <- 1/v2
waga[grupa==3] <- 1/v3
waga[grupa==4] <- 1/v4

reg1<-lm(pressure~age,weight=waga)
reg1
summary(reg1)
summary(reg)
plot(reg1$res ~ age)
plot(sqrt(waga) * reg$res ~ age)
