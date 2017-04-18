#install.packages ('doBy')
#Load doBy and ggplot libraries to be utilized in this code.
library(doBy)
library(ggplot2)
#print out Orange data which is built-in.
Orange

# This prints out mean and median which are listed in FUN. This compares circumference by tree.
summaryBy(Orange$circumference  ~ Orange$Tree, data=Orange , FUN=c(mean,median),na.rm=TRUE,keep.names=TRUE)

# This prints out the age vs circumference.
# TODO: Get plot styles to change for tree.
ggplot(Orange, aes(x = Orange$age, y = Orange$circumference)) + geom_point(size = 5)
