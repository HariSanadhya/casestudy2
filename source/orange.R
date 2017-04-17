#install.packages ('doBy')
library(doBy)
library(ggplot2)
Orange
require(stats); require(graphics)
coplot(circumference ~ age | Tree, data = Orange, show.given = FALSE)
fm1 <- nls(circumference ~ SSlogis(age, Asym, xmid, scal),
           data = Orange, subset = Tree == 3)
plot(circumference ~ age, data = Orange, subset = Tree == 3,
     xlab = "Tree age (days since 1968/12/31)",
     ylab = "Tree circumference (mm)", las = 1,
     main = "Orange tree data and fitted model (Tree 3 only)")
age <- seq(0, 1600, length.out = 101)
lines(age, predict(fm1, list(age = age)))

summaryBy(Orange$circumference  ~ Orange$Tree, data=Orange , FUN=c(mean,median),na.rm=TRUE,keep.names=TRUE)

ggplot(Orange, aes(x = Orange$age, y = Orange$circumference, shape = Orange$Tree)) + geom_point(size = 5)