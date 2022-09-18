library(faraway)
data(savings)
View(savings)
attach(savings)
countries <- row.names(savings)
g<-lm(sr~pop15+pop75+dpi+ddpi,savings)
summary(g)

gl<-lm(sr~pop15 + pop75 + dpi + ddpi,
       savings, subset = (countries!="Libya"))
summary(gl)

ginf<-lm.influence(g)
plot(ginf$coef[,2], ginf$coef[,3],
     xlab="pop15 coef", ylab="pop75 coef")
identify(1:50, ginf, countries)
# Japan
gj<-lm(sr~pop15 + pop75 + dpi + ddpi,
       savings, subset = (countries!="Japan"))
summary(gj)

halfnorm(lm.influence(g)$hat
         ,labs=countries, ylab="Leverages")

halfnorm(cooks.distance(g),labs=countries
         ,ylab="Cook Statistics")

plot(g)
