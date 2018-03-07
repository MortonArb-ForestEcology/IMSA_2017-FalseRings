# -------------------------------------
# Cleaning up the fire data to make it match the false ring data a bit better
# Christy Rollinson 
# -------------------------------------

library(ggplot2); library(googlesheets); 



path.google <- "/Volumes/GoogleDrive/My Drive/East Woods/"

# ---------------
# Getting the basic plot info with the crosswalk with the corners
# ---------------
plot.info <- read.csv(file.path(path.google, "Rollinson_Monitoring/Plot Selection/CandidatePlots.csv"))
plot.info$plot <- as.factor(paste0(plot.info$stand, plot.info$order))
summary(plot.info)
# ---------------


# ---------------
# Read the data directly from the google sheet
# ---------------
sheet.fr <- gs_title("Core_Metadata_EastWoods_2017_FalseRings_Metadata")
dat.fr <- data.frame(gs_read(sheet.fr, ws="Data_FalseRings_2017"))

# Making things factors for my sanity
for(i in 1:ncol(dat.fr)){
  if(is.numeric(dat.fr[,i])) next
  dat.fr[,i] <- as.factor(dat.fr[,i])
}
summary(dat.fr)
unique(dat.fr$plot)
# ---------------

# ---------------
# Merging in our corner info 
# ---------------
# Extract info for the plots where we're using a code instead of the corner name 
dat.fr2 <- merge(dat.fr, plot.info, all=F)
summary(dat.fr2)

# If our plot name is more than 2 characters, that's our corner and lets work with those separately
dat.fr3 <- dat.fr[nchar(paste(dat.fr$plot))>2,]
dat.fr3$CORNER <- as.factor(paste(substr(dat.fr3$plot, 1, 1), 
                                  substr(dat.fr3$plot, 2, nchar(paste(dat.fr3$plot))), 
                                  sep="-"))
summary(dat.fr3)

# Merge in the plot info; NOTE: This will overwrite the data frame we just created
dat.fr3 <- merge(dat.fr3[,c("site", "tree", "core", "species", "year", "notes", "CORNER")], plot.info)
summary(dat.fr3)

dat.fr.final <- merge(dat.fr2, dat.fr3, all=T)
summary(dat.fr.final)
write.csv(dat.fr.final, file.path(path.google, "IMSA_2017_Rollinson/Data_FalseRings_PlotInfo.csv"), row.names=F)
# ---------------
#



# ---------------
# Read in & clean up the fire data to make it easier to merge
# ---------------
"/Volumes/GoogleDrive/My Drive/East Woods/URF_2017_Rollinson/URF2017_BurnInfo.csv"

# /URF_2017_Rollinson/URF2017_BurnInfo.csv
dat.fire <- read.csv(file.path(path.google, "URF_2017_Rollinson/URF2017_BurnInfo.csv"))
dat.fire$Burn_Date <- as.Date(dat.fire$Burn_Date)
dat.fire$Burn_Year <- lubridate::year(dat.fire$Burn_Date)
dat.fire[!is.na(dat.fire$Burn_Date),"Burn_Season"] <- ifelse(lubridate::month(dat.fire[!is.na(dat.fire$Burn_Date),"Burn_Date"])<6, "Spring", "Fall")
dat.fire$Burn_Season <- as.factor(dat.fire$Burn_Season)
summary(dat.fire)

# summary(dat.fire[is.na(dat.fire$Burn_Date),])
# summary(dat.fire)



dat.fire <- dat.fire[,c("Corner", "Type", "Burn_Date", "Burn_Year", "Burn_Season", "Location", "Acres", "NOTES")]
dat.fire[!is.na(dat.fire$NOTES),"Burn_Year"] <- 2013
# dat.fire[!is.na(dat.fire$NOTES),]
names(dat.fire)[1] <- "CORNER"
summary(dat.fire)
write.csv(dat.fr.final, file.path(path.google, "IMSA_2017_Rollinson/Data_Fire_Clean.csv"), row.names=F)
# ---------------




fire.raw