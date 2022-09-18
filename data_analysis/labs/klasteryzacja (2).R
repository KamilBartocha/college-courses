
# https://www.inf.ed.ac.uk/teaching/courses/nlu/assets/reading/Gurney_et_al.pdf
library(kmeans)
teens <- read.csv("snsdata.csv", stringsAsFactors = TRUE)
str(teens)
table(teens$gender, useNA = "ifany")

teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA)
summary(teens$age)

teens$female <- ifelse(teens$gender == "F" & !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")

mean(teens$age, na.rm = TRUE)
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)

ave_age <- ave(teens$age, teens$gradyear, FUN = function(x) mean(x, na.rm = TRUE))
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)

interests <- teens[5:40]

interests_z <- as.data.frame(lapply(interests, scale))
summary(interests$basketball)
summary(interests_z$basketball)

RNGversion("3.5.2")
set.seed(2345)
?kmeans
teen_clusters <- kmeans(interests_z, 5)
teen_clusters
teen_clusters$size
teen_clusters$center
