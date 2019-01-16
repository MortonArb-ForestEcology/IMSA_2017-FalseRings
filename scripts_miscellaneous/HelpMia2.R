
#dat = read.table("more fun/MetStation_2017_EW_2017-09-27.csv",header=TRUE)
dat = read.csv(file="more fun/Dendroband_trees.csv", header=TRUE, sep=",")

#select variables need to analyze
dat1 <- subset(dat,select=c('Plot','Tag'))
#remove missing values 
dat1 <- dat1[complete.cases(dat1), ]
table(dat1$Tag)

# use if trees with tag none are not trees 
  #dat2 <- dat1[!dat1$Tag=='none',]

#get counts
  #treecounts <- table(dat2$Plot)
treecounts <- table(dat1$Plot)


#bar plot: x=Plot and y=Number of tree in the each plot
barp1 <- barplot(treecounts, main="Number of Trees by Plot", horiz=F,
        names.arg=names(treecounts),xlab='Plot',ylab='Number of Trees',ylim=c(0,50))
text(barp1, treecounts, treecounts,pos=1)
box()

#get counts for coreable
C2 <- factor(dat$Coreable,levels=c('Y','N',''))
Corecounts <- table(C2)
names(Corecounts) <- c('Y','N','Missing')

#bar plot for coreable
barp2 <- barplot(Corecounts, main="Distribution of Coreable", horiz=F,
                names.arg=names(Corecounts),xlab='',ylab='Numbers',ylim=c(0,100))
text(barp2, Corecounts, Corecounts,pos=3)
box()








dat = read.csv(file="more fun/Dendroband_trees.csv")
dat1 <- 

