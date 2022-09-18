install.packages("expsmooth")
library(expsmooth)
data(visitors)
plot(visitors)

period <- 1:length(visitors)
raw <- data.frame(cbind(visitors, period))
timetrend <- lm(visitors ~ period + I(log(period)), data = raw)

raw$timetrend <- predict(timetrend, raw)
raw$withouttimetrend <- raw$visitors - raw$timetrend
plot(raw$withouttimetrend, type = "o")

seasonsmatrix = t(matrix(data = raw$withouttimetrend, nrow = 12))
seasons = colMeans(seasonsmatrix, na.rm = T)
raw$seasons <- c(rep(seasons, 20))
raw$error <- raw$visitors - raw$timetrend - raw$seasons

plot(raw$error, type = "o")
par(mfrow = c(2,1))
plot(raw$timetrend, type = "o")
plot(raw$seasons, type = "o")
plot(raw$error, type = "o")

stdev <- sd(raw$error)
high_outliers <- which(raw$error > (mean(raw$error) + 2 * sd(raw$error)))
low_outliers <- which(raw$error < (mean(raw$error) - 2 * sd(raw$error)))

raw[high_outliers, ]

plot(raw$period, raw$visitors, type = "o")
points(raw$period[high_outliers], raw$visitors[high_outliers], pch = 19,
       col = "red")
points(raw$period[low_outliers], raw$visitors[low_outliers], pch = 19, 
       col = "blue")


#symulacja

x <- 1:round(2 * pi * 100 + 100) / 100
y <- rep(0, round(2 * pi * 100) + 100)

y[1:314] <- sin(x[1:314])
y[415:728] <- sin(x[315:628])
y[100] <- 0
plot(x, y, type = "o")

difference_y <- y[2:length(y)] - y[1:length(y) - 1]
boxplot(difference_y)
which(difference_y > 0.5)

#changes <- NULL
#ks <- NULL
#k = 11
#while(k < (length(y) - 100)
#      changes <- c(changes, max(abs(y[k] - y[k-10], abs(y[k] - y[k-10])))))

normal_results <- c(100, 95, 106, 92, 109, 190, 210, 201, 198)
new_results <- c(98, 32, 270, 140, 200)

bandwidth <- 25
our_estimate <- density(normal_results, bw = bandwidth)

plot(our_estimate)

new_density_1 = density(normal_results, bw = 25, n = 1,
                        from = new_results[1], to = new_results[1])$y
print(new_density_1)
