library(faraway)
data(pima)
pima
summary(pima)
View(pima)

pima$diastolic[pima$diastolic == 0] <- NA
pima$glucose[pima$glucose == 0] <- NA
pima$triceps[pima$triceps == 0] <- NA
pima$insulin[pima$insulin == 0] <- NA
pima$bmi[pima$bmi == 0] <- NA
View(pima)

pima$test <- factor(pima$test)
levels(pima$test) <- c("negative","positive")
summary(pima)

westwood<-data.frame()
fix(westwood)
View(westwood)
summary(westwood)
write.table(westwood,"westwood.txt")
?read.table
temperature <- read.table("temperature.txt", header = TRUE, sep = ";")
View(temperature)
