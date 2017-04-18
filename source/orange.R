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

Orange$Tree <- factor(Orange$Tree, labels = c("1", "2", "3", "4", "5"))

# Create colors for box plot.
fill <- "#4271AE"
line <- "#1F3552"

# Generate box plot of circumference by tree.
ggplot(Orange, aes(x = Tree, y = circumference)) + geom_boxplot(fill = fill, colour = line, alpha = .7) + 
  scale_x_discrete(name = "Tree #") + scale_y_continuous(name = "Circumference in inches") +
  ggtitle("Boxplot of circumference by tree")
