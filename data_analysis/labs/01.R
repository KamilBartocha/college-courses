library(class)
library(gmodels)
# k-nn model for wisc data
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)
# usuwamy id
wbcd <- wbcd[-1]
str(wbcd)
table(wbcd$diagnosis)
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M")
                         ,labels = c("Benign","Malignant"))
wbcd$diagnosis
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)
summary(wbcd)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# x = c(2, 3, 4)
# y = c(4, 6, 8)
#x <- normalize(x)
#y <- normalize(y)
#x
#y
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
# summary(wbcd_n$area_mean)
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

wbcd_pred <- knn(train = wbcd_train, test = wbcd_test,
                 cl = wbcd_train_labels, k = 21)
# summary(wbcd_pred)
# summary(wbcd_test_labels)
CrossTable(x = wbcd_test_labels, wbcd_pred, prop.chisq = FALSE)

## DOPRACOWANIE MODELU
wbcd_pred_d <- knn(train = wbcd_train, test = wbcd_test,
                 cl = wbcd_train_labels, k = 3)
CrossTable(x = wbcd_test_labels, wbcd_pred_d, prop.chisq = FALSE)
