library(faraway)
data(savings)
View(savings)
attach(savings)

g<-lm(sr~pop15+pop75+dpi+ddpi,savings)
summary(g)
# first and last of residuals
sort(g$res)[c(1,50)]
plot(g$res)
countries <- row.names(savings)
identify(1:50, g$res, countries)

lev<-hatvalues(g)
plot(lev)
# p=5 n=50 => 2p/n = 1/5
abline(h=0.2)
identify(1:50, lev, countries)
# Libya, United States, Irleand, Japan 
stud <- rstandard(g)
plot(stud)
identify(1:50, stud, countries)

jack <- rstudent(g)
plot(jack)
qt(0.95, 44)
# 1,68023
abline(h = 1.68)
abline(h = -1.68)
identify(1:50, jack, countries)
identify(1:50, jack, countries)
# Zambia, Chile, Iceland, Korea, Paraguay

# DIFFITS - Difference Fit
# abs(diffits) < 1 (small samples)
# abs(diffits) > 2 sqrt(p/n) (big samples)
dffits(g)
sort(dffits(g))
# Libya > 1
big_s <- 2*sqrt(0.1)
# big_s = 0.6324

# DIFBETAS - Difference Beta
# abs(diffits) < 1 (small samples)
# abs(diffits) > 2 sqrt(p/n) (big samples)
dfbetas(g) 
# Libya = -1.0244773078 > 1

# cooks_d < 0.1-0.2 (small samples)
# cooks_d > 0.5 (big samples)
cooks.distance(g)
sort(cooks.distance(g))
# Libya 2.68
?pf
# pf(libya(worst), parametry, st swobody)
pf(0.268, 5, 45)

inf<-influence.measures(g)
inf
summary(inf)
