library(ggplot2); library(googlesheets); 
library(stringr)

# install.packages("ggplot2")

# Establishing a connection with our Google Sheets
sheet.fr <- gs_title("Core_Metadata_EastWoods_2017_FalseRings_Metadata")
sheet.fr

sheet.core <- gs_title("Core_Metadata_EastWoods_2017")
sheet.core

# Pulling the data from a specific worksheet into a dataframe
dat.fr <- data.frame(gs_read(dat.fr, ws="Data_FalseRings_2017"))
dat.core <- data.frame(gs_read(sheet.core, ws="CoreData_2017"))

# Converting things to factors & numbers
cols.fact <- c("site", "plot", "tree", "core", "species", "notes")
for(i in cols.fact){
  dat.fr[,i] <- as.factor(dat.fr[,i])
}

dat.fr$year <- as.numeric(dat.fr$year) # making year an actual number; this will get rid of some weirdos
dat.fr$year <- as.numeric(paste(dat.fr$year)) #
cols.fact <- c("site", "plot", "tree", "core", "species", "notes", "bark", "issues.crossdating", "rings.missing", "rings.false")
for(i in cols.fact){
  dat.core[,i] <- as.factor(dat.core[,i])
}

summary(dat.fr)
summary(dat.core)



# -----------
# Doing some exploratory graphing using ggplot
summary(dat.fr)

ggplot(dat=dat.fr) +
  facet_wrap(~species) +
  geom_histogram(aes(x=year, fill=plot), binwidth=10) +
  coord_cartesian(xlim=c(1980,2015))

