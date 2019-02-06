#import data
setwd("/Users/Owner/Desktop/")
dat.xd <- read.csv("file:///C:/Users/Owner/Desktop/Xylem_Data - raw data.csv")
library(ggplot2)
library(dplyr)
summary(dat.xd)

#AVG VESSEL AREA
S <- c(dat.xd$Calculated.Area)
counts <- table(S)
summary(S)
hist(S)
barplot(counts)
tree_id <- as.factor(dat.xd$Tree.ID)
summary(tree_id)
summary(dat.xd$Species.of.Tree)
barplot(dat.xd$Calculated.Area, main = "Vessel Area", xlab = "Each Tree bc I don't know how to separate it", ylab = "Size of Vessel in mm^2", names.arg = (dat.xd$Species.of.Tree))
barplot(height = c(0,0.5), main = "Average Vessel Area", xlab = "Species", ylab = "Size of Vessel (mm^2)", names.arg = c("QUAL", "QURU"))
#as.numeric turns factors into numerics
#as.factor turns numerics into factors
#aggregate data, fun=mean

#ATTEMPT AT AGGREGATING
agg = aggregate(xylem.df2[, 8], list(xylem.df2$Species.of.Tree), mean)
barplot(agg, main = "Vessel Area", xlab = "trees", ylab = "Size of Vessel in mm^2", names.arg = (dat.xd$Species.of.Tree))


#AVG RING AREA
S <- c(dat.xd$Area.of.Ring)
counts <- table(S)
barplot(counts)
barplot(height = c(0,5), main = "Average Ring Area", xlab = "Species", ylab = "Size of Ring (mm^2)", names.arg = c("QUAL", "QURU"))

#AVG VESSEL DENSITY
S <- c(dat.xd$Vessel.Density)
counts <- table(S)
barplot(counts)
barplot(height = c(0,3), main = "Average Vessel Density", xlab = "Species", ylab = "Vessel Density", names.arg = c("QUAL", "QURU"))

#GRAPH THAT WORKS
ggplot(xylem.df2, aes(x=Species.of.Tree, y=Calculated.Area, fill=Species.of.Tree)) +  # This is the plot function
  geom_boxplot() + ggtitle("Area of Vessel per Species") + theme(plot.title = element_text(hjust = 0.5)) +labs(y= "Area of Vessel (mm^2)", x = "Species of Oak")

res.aov <- aov(Calculated.Area ~ Species.of.Tree, data = xylem.df2)
summary(res.aov)
TukeyHSD(res.aov)
