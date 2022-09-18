raw <- read.csv("heightsweights.csv", stringsAsFactors = FALSE)
plot(raw$height, raw$weight)

centroid <- c(mean(raw$height), mean(raw$weight))
centroid

example_distance <- raw[1, c('height', 'weight')] - centroid
