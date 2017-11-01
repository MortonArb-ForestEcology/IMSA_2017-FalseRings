#set working directory
setwd("/Users/Owner/Desktop/SIR/Tree Rings/TEST/")

#import data set MetStation
dat <- read.csv("data/MetStation_2017_EW_2017-09-27.csv")

#set variables 
dat1 <- subset(dat,select = c("Plot",'Soil_Temp'))
#remove missing values
table(dat1$Soil_Temp)
dat1 <- dat1[complete.cases(dat1),]

soil_temp <-  c('Soil_Temp')

#make a bar graph where x=Plot and y=soil temp
#horiz = F means that the bars will be vertical; horiz = T would mean they are horizontal
barp1 <- barplot(soil_temp, main = "Soil Temperature per Plot", horiz = F,
                 names.arg = names(soil_temp),xlab = 'Plot', ylab = 'Soil_Temp')
text(barp1, soil_temp, soil_temp, pos = 1)

bar <- barplot(data=soil_temp)

box()
