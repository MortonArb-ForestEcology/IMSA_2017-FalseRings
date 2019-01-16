setwd("C:/Users/BZumwalde/Desktop/False_Rings_IMSA")

library(ggplot2)
library(plyr)
library(dplyr)



Daymet <- read.csv("ClimateData_Daymet.csv")
Core_data <- read.csv("Core_Metadata_EastWoods_2017_FalseRings_Metadata.csv")
summary(Core_data)
summary(Daymet)
View(Daymet)

summary(Core_data)

FR <- count(Core_data, "plot")
Counts <- table(Core_data$plot)
View(Counts)
barplot(Counts)

ggplot(data=Counts, aes(x=plot, y=values))+
  facet_wrap(~var, scales="free_y") +
  geom_line(aes(x=doy, y=values, color=plot)) +
  theme_bw()



Daymet.stack <- stack(Daymet[,c("tmin", "tmax", "prcp", "swdown", "vp", "GDD5")])
names(Daymet.stack) <- c("values", "var")
Daymet.stack[,c("year", "doy")] <- Daymet[,c("year", "doy")]

                             
df.1996 <- filter(Daymet.stack, year == 1994:1996)
View(Daymet.stack)
View(df.1996)

ggplot(data=df.1996, aes(x=doy, y=values))+
  facet_wrap(~var, scales="free_y") +
  geom_line(aes(x=doy, y=values, color=year)) +
  theme_bw()
