westwood <- read.table("westwood.txt")
attach(westwood)
summary(westwood)
View(westwood)
# fix(westwood)
plot(ManHours~LotSize)
boxplot(LotSize)
plot(LotSize, type="o")
?lm
reg<-lm(ManHours ~ LotSize)
summary(reg)
# dorysowanie lini reg do poprzedniego wykresu
abline(reg)
# ale czy wariancja jest stała i jest niezależność?
plot(LotSize, reg$res)
abline(h=0)
plot(reg$res ~ reg$fit)

plot(reg$res, type="o")
abline(h=0)
boxplot(reg$res)

qqnorm(reg$res)
qqline(reg$res)
plot(reg$res~reg$fit)

identify(LotSize,reg$res)

