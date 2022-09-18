data(rivers)
boxplot(rivers)

log_rivers <- (log(rivers))
boxplot(log_rivers)
interquartile_range <- unname(quantile(rivers, .75) - quantile(rivers, .25))
interquartile_range_l <- unname(quantile(rivers, .75) - quantile(rivers, .25))
print(interquartile_range)
upper_limit <- unname(quantile(rivers, .75) + 1.5 * interquartile_range)
lower_limit <- unname(quantile(rivers, .25) - 1.5 * interquartile_range)

rivers[which(rivers > upper_limit | rivers < lower_limit)]

# upper_limit2 <- unname(quantile(rivers, .5) + 1.5 * interquartile_range)
# lower_limit2 <- unname(quantile(rivers, .5) - 1.5 * interquartile_range)

# rivers[which(rivers > upper_limit2 | rivers < lower_limit2)]
# upper_limit_log <- unname(quantile(log_rivers, .5) + 1.5 * interquartile_range_l)
# lower_limit_log <- unname(quantile(log_rivers, .5) - 1.5 * interquartile_range_l)

# log_rivers[which(log_rivers > upper_limit_log | log_rivers < lower_limit_log)]
standard_deviation <- sd(rivers)
z_scores <- (rivers - mean(rivers)) / standard_deviation

outliners <- rivers[which(z_scores > 2  | z_scores <2)]
outliners
