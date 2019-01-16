dat.cr = read.csv("more fun/Core_Metadata_EastWoods_2017 - CoreData_2017.csv", header=TRUE)
dat.fr <- read.csv("more fun/Core_Metadata_EastWoods_2017_FalseRings_Metadata_Data_FalseRings_2017.csv")
dat.br <- read.csv("more fun/URF2017_BurnInfo_editmanual.csv.csv", header=TRUE)
dat.plotsurvey <- read.csv("more fun/Tree_PlotSurvey_2017 - raw data.csv", header=TRUE)

summary(dat.cr)
summary(dat.fr)

names(dat.fr)
summary(dat.cr[,c("site", "plot", "tree", "core", "species", "yr.inner")])

summary(dat.fr)

dat.fr2 <- merge(dat.fr, dat.cr[,c("site", "plot", "tree", "core", "species", "yr.inner")])
summary(dat.fr2)

library(ggplot2)

#graph with plots and year 
ggplot(dat=dat.fr2, aes(x=plot,fill=year), binwidth=100)+
  facet_wrap(~plot)+
  geom_histogram(stat="count")


dat.fr2$year <- as.numeric(paste(dat.fr2$year))

#na.rm=T <- remove na 
#number of false rings as a function of age 
#oaks start with QU, subset to only oaks

######

#dat.fr22$falseage <- (dat.fr22$year-dat.fr22$yr.inner) 
  #plotvsyear <- aggregate(falseage~plot,data=dat.fr22 ,mean)

#plot vs age of tree when it got a false ring 
  
#ggplot(dat=plotvsyear, aes(x=plot,y=falseage))+
    #geom_bar(stat="identity")

######

summary(dat.fr2)
FRdata$year <- as.numeric(paste(FRdata$year))
FRdata$tree <- as.factor(FRdata$tree)
summary(dat.fr2)
aggregate(year~plot, data=dat.fr2, mean)

ag.year.plot <- aggregate(year~plot, data = dat.fr2, mean, na.rm=T) 
#get rid of NAs with na.rm=T directly in aggregate
summary(ag.year.plot)

ggplot(data = ag.year.plot, aes(x=plot, y=year))+
    stat_smooth(method = "lm", col="red")

ggplot(data = dat.fr2, aes(y=plot, x=year))+
  geom_point()


ggplot(data = dat.fr2, aes(y=plot, x=year))+
  geom_point(position="jitter")


FRdat.agg <- aggregate(dat.fr2$year, by=dat.fr2[,c("plot", "year")],FUN=length)
names(FRdat.agg)[3] <- "num.FR"
summary(FRdat.agg)


ggplot(data = FRdat.agg, aes(y=plot, x=year))+
  geom_point(aes(size=num.FR))


summary(FRdata.simplified$year)

summary(FRdata)

dat.plotsurvey <- dat.plotsurvey[c(6,14,22,28,31,32,35,37,39,41,65,77,80,83,86,94),]
names(dat.plotsurvey)
dat.plotsurvey$Sp_code <- as.character(dat.plotsurvey$Sp_code)
names(dat.plotsurvey)[3] <- "tree"
names(dat.plotsurvey)[2] <- "plot"
names(dat.plotsurvey) [6] <- "species"
dat.plotagg <- aggregate(dat.plotsurvey$plot, by=dat.plotsurvey[,c("plot","tree")], FUN=length)
names(dat.plotagg)[3]<- "count"
summary(dat.plotagg)

#plots with number of oaks
library(plyr)
count.plots <- count(dat.plotagg,"plot")
names(count.plots)
names(count.plots)[2] <- "num.oaks"

summary(dat.plotmerge)
dat.plotmerge <- merge(count.plots, FRdat.agg[,c("plot", "num.FR", "year")])
dat.plotmerge <- transform(dat.plotmerge, frequency = dat.plotmerge$num.FR / dat.plotmerge$num.oaks)

ggplot(data=dat.plotmerge, aes(y=frequency, x=year))+
  geom_point(aes(size=plot))
  
ggplot(dat=dat.plotmerge, aes(x=year), binwidth=10)+
  geom_histogram(aes(y=frequency), stat="identity")+
  facet_wrap(~plot)

#split data into 3 different plots
dat.plotmergeA <- dat.plotmerge[-c(2:25), ]
dat.plotmergeB <- dat.plotmerge[-c(1,13:25), ]
dat.plotmergeC <- dat.plotmerge[-c(1:12), ]

ggplot(dat=dat.plotmergeA, aes(x=year), binwidth=10)+
  geom_histogram(aes(y=frequency), stat="identity")+
  facet_wrap(~plot)

ggplot(dat=dat.plotmergeB, aes(x=year), binwidth=10)+
  geom_histogram(aes(y=frequency), stat="identity")+
  facet_wrap(~plot)

ggplot(dat=dat.plotmergeC, aes(x=year), binwidth=10)+
  geom_histogram(aes(y=frequency), stat="identity")+
  facet_wrap(~plot)


