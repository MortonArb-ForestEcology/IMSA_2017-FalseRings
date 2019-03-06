library(ggplot2); library(stringr); library(googlesheets)

# 1.1 Reading in Jessica's data
xylem <- googlesheets::gs_title("Xylem_Data")
#token <- gs_auth()  #Will reset token if needed
xylem.df <- data.frame(googlesheets::gs_read(xylem, ws="raw data"))
xylem.df$Species.of.Tree <- as.factor(xylem.df$Species.of.Tree)
xylem.df$Tree.ID <- as.factor(xylem.df$Tree.ID)
xylem.df$Vessel <- as.factor(xylem.df$Vessel)
xylem.df$Tag <- as.factor(substr(xylem.df$Tree.ID, 1, 4)) # Making something that corresponds to columns in the 
# plot survey data
summary(xylem.df)

# Doing some additional data cleaning
unique(xylem.df$Tree.ID) # --> note that some have spaces, some don't
xylem.df$Tree.ID <- sub(" ", "", xylem.df$Tree.ID) # Get rid of the spaces by replacing them with nothing
xylem.df$core <- substr(xylem.df$Tree.ID, 5,5) # Most trees will ahve 2 cores
xylem.df$Core.ID <- paste(xylem.df$Tag, xylem.df$core, sep="-") # Trying to match things to 

summary(xylem.df)


# -----------------------
# Quick analysis of Vessel Density
# -----------------------
# Looking at the mean & standard deviation of Vessel Density in two groups
mean(xylem.df[xylem.df$Species.of.Tree=="QUAL", "Vessel.Density"], na.rm=T); sd(xylem.df[xylem.df$Species.of.Tree=="QUAL", "Vessel.Density"], na.rm=T)
mean(xylem.df[xylem.df$Species.of.Tree=="QURU", "Vessel.Density"], na.rm=T); sd(xylem.df[xylem.df$Species.of.Tree=="QURU", "Vessel.Density"], na.rm=T)

# Comparing the two as a ratio as Jessica has in her abstract
mean(xylem.df[xylem.df$Species.of.Tree=="QUAL", "Vessel.Density"], na.rm=T)/mean(xylem.df[xylem.df$Species.of.Tree=="QURU", "Vessel.Density"], na.rm=T)
# -----------------------

# -----------------------
# Quick analysis of individual vessel area
# -----------------------
# Looking at the mean & standard deviation of Vessel Density in two groups
mean(xylem.df[xylem.df$Species.of.Tree=="QUAL", "Calculated.Area"], na.rm=T); sd(xylem.df[xylem.df$Species.of.Tree=="QUAL", "Calculated.Area"], na.rm=T)
mean(xylem.df[xylem.df$Species.of.Tree=="QURU", "Calculated.Area"], na.rm=T); sd(xylem.df[xylem.df$Species.of.Tree=="QURU", "Calculated.Area"], na.rm=T)

# Comparing the two as a ratio as Jessica has in her abstract
mean(xylem.df[xylem.df$Species.of.Tree=="QUAL", "Calculated.Area"], na.rm=T)/mean(xylem.df[xylem.df$Species.of.Tree=="QURU", "Calculated.Area"], na.rm=T)
# -----------------------
