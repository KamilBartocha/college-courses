# Problem 1
library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr, warn.conflicts = FALSE)
library(xtable)

# Read in the data
grades <- c(16.25, 12.5, 15, 16.5, 11.5, 10, 16, 14, 14.75, 13, 11, 12.5, 13.25,
            15, 15, 12.75, 16, 9.5, 11.75, 14.5, 17, 18, 14.25, 17.5, 14, 15.25,
            15.25, 14.5, 16, 18, 13.5, 16.5, 12, 16, 12)

# Define factor "pen" corresponding to pen-type for each grade
pen <- factor(c(rep("ball",7*3), rep("ink", 7*2)))

# Define factor "color" corresponding to pen color for each grade
color <- factor(c(rep(c("blue","black","green"), each = 7, times = 2)))
color <- color[1:(7*5)]

# Combine color and pen to get "writing tool"
tool <- paste(color, pen)

# Put data in a long-form (melted) data frame (one observation per row)
grades.df <- data.frame(pen = pen, color = color, tool = tool, grade = grades)
head(grades.df)


ggplot(grades.df) + 
  geom_boxplot(aes(x = tool, y = grade, fill = tool)) + theme_bw()

# One-way ANOVA for pen-type
anova.tool <- aov(grade ~ tool, grades.df)
print(summary(anova.tool))
pf(1.15, 4, 30, lower.tail = FALSE)

blueball <- c(16.25, 12.5, 15, 16.5, 11.5, 10, 16)
blackball <- c(14, 14.75, 13, 11, 12.5, 13.25,15)
greenball <- c(15, 12.75, 16, 9.5, 11.75, 14.5, 17)
blueink <- c(18, 14.25, 17.5, 14, 15.25,15.25, 14.5)
blackink <- c(16, 18, 13.5, 16.5, 12, 16, 12)

blue <- c(blueball, blueink)
black <- c(blackball, blackink)
green <- greenball

anova.color <- aov(grade ~ color, grades.df)
print(summary(anova.color))
print(Fstatistic(blue, black, green))
pf(Fstatistic(blue, black, green), 2, 32, lower.tail = FALSE)



# Problem 2:
library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr, warn.conflicts = FALSE)

# Read in the data
fabric <- c(7, 7, 15, 11, 9, 12, 17, 12, 18, 18, 14, 18, 18, 19, 19,
            19, 25, 22, 19, 23, 7, 10, 11, 15, 11)

# Define factor "cotton" corresponding to percentage of cotton for each fabric
pct <- factor(c(rep("15pct",5), rep("20pct", 5), rep("25pct",5), rep("30pct", 5), rep("35pct",5)))

# Put data in a long-form (melted) data frame (one observation per row)
cotton.df <- data.frame(pct = pct, fabric = fabric)

ggplot(cotton.df) + geom_boxplot(aes(x = pct, y = fabric, fill = pct))

# Analysis of Variance

cotton.aov <- aov(fabric ~ pct, cotton.df)
print(summary(cotton.aov))


# Pairwise Comparisons

library(DescTools)

PostHocTest(cotton.aov, method = "scheffe")
PostHocTest(cotton.aov, method = "bonferroni")
PostHocTest(cotton.aov, method = "lsd")
PostHocTest(cotton.aov, method = "hsd")
PostHocTest(cotton.aov, method = "duncan")
PostHocTest(cotton.aov, method = "newmankeuls")
DunnettTest(fabric, pct, control = "15pct")

# Problem 3:
library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr, warn.conflicts = FALSE)
library(DescTools)

# Read in the data
trt1 <- c(35,37,49,46,63,39,46,56,63,65,56,65,70,63,65,70,77,81,86,70,70,77,77,81,77)
trt2 <- c(40,37,44,47,47,47,68,47,54,61,71,75,89,58,59,62,79,96,58,62,70,72,75,96,75)
trt3 <- c(46,42,65,46,58,42,48,58,50,80,63,65,70,70,72,97,46,56,70,70,72,76,90,76,92)
trt4 <- c(21,40,44,54,36,40,56,60,48,53,60,60,65,68,60,81,81,48,48,56,68,75,81,48,68)
trt5 <- c(16,19,19,32,33,33,30,42,42,33,26,30,40,54,34,34,47,47,42,47,54,54,56,60,44)

trt <- c(trt1, trt2, trt3, trt4, trt5)
desc <- rbind(summary(trt1), summary(trt2), summary(trt3),summary(trt4),summary(trt5))
desc2 <- cbind(desc, c(sd(trt1), sd(trt2), sd(trt3), sd(trt4), sd(trt5)))
rownames(desc2) <- c("None", "1 pregnant", "1 virgin", "8 pregnant", "8 virgin")
colnames(desc2) <- c("Min", "Q1","Median", "Mean", "Q3", "Max", "Std Dev")
print(desc2)


# Define factor 
group <- factor(c(rep("None",25), rep("1 pregnant", 25), 
                  rep("1 virgin",25), rep("8 pregnant", 25), 
                  rep("8 virgin",25)))

# Put data in a long-form (melted) data frame (one observation per row)
fly.df <- data.frame(trt = trt, group = group)
fly.aov <- aov(trt ~ group, fly.df)
print(summary(fly.aov))

PostHocTest(fly.aov, method = "scheffe")
PostHocTest(fly.aov, method = "bonferroni")
PostHocTest(fly.aov, method = "lsd")
PostHocTest(fly.aov, method = "hsd")
DunnettTest(trt, group, control = "None")

plot(fly.aov)

bartlett.test(trt ~ group, data = fly.df)

# Original Levene's Test
LeveneTest(trt, group, center = mean)

# Brown-Forsythe Test (more robust)
LeveneTest(trt, group, center = median)

library(MASS)
boxcox(fly.aov)