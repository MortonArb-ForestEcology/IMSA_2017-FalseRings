# Purpose: This script will gather all of the tree data we need and put it into a single data frame.
# Authors: Christy Rollinson, Bethany Zumwalde, Jessica Oros
# 
# Workflow:
# 1. Read in tree data & Jessica's Data
# 2. Subset to just the trees Jessica needs
# 3. Read in the tree ring measurements for Jessica's trees
#    1. scan folder for files available to work with; I use dir() and grep() family of funcitons a lot
#    2. select those for the cores we want to work with
#    3. use read.rwl function in dplR to read in earlywood & latewood files for those trees

# ------ Maybe split into new script? ------ 
# 4. Calculate diameter for each EW/LW boundary point
# 5. Calculate cumulative area for each landmark (EW/LW boundary point)
# 6. Calculate area of each latewood/earlywood portion (EWA, LWA)


 library(dplR) # Tree ring software
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

# 1.2 Reading in Tree Data
plotsurvey <- read.csv("../data_raw/TreeSurveyData/Tree_PlotSurvey_2017 - raw data.csv", na.strings="")
#survey.add <- read.csv("./TreeSurveyData/Trees_Survey_Additional_URF_2017 - raw_data.csv")
summary(plotsurvey)



# 2.1 Subset to just the trees Jessica needs
xylem.df2 <- merge(xylem.df, plotsurvey[,c("Tag", "Date", "IMLS_Plot", "X", "Y", "Sp_code", "Canopy")], by = "Tag", all.x=T) # we don't want all of the plot survey data, so make sure we're not bringing unnecessary data along
summary(xylem.df2)

# 3. Read in the tree ring measurements for Jessica's trees
path.rw <- "../data_raw/RawRingWidths/"

# Looping through just the trees Jessica needs
dat.all <- data.frame()
for(CORE in unique(xylem.df$Core.ID)){
  f.tree <- dir(path.rw, CORE) # Get a list of files for the core we want
  
  # Read in the different datasets we want to work with
  rw.core <- dplR::read.rwl(file.path(path.rw, f.tree[grep("ring", f.tree)]))
  ew.core <- dplR::read.rwl(file.path(path.rw, f.tree[grep("earlywood", f.tree)]))
  lw.core <- dplR::read.rwl(file.path(path.rw, f.tree[grep("latewood", f.tree)]))
  
  # Organize the data for each core into a temporary data frame
  # Note that we're multiply everything by 0.1 to turn it from mm to cm (our DBH units)
  dat.core <- data.frame(Core.ID=CORE, year = as.numeric(row.names(rw.core)), 
                         earlywood=ew.core[,1]*.1, latewood=lw.core[,1]*.1, ring=rw.core[,1]*.1)
  
  # append (rbind) the data into what we already have;
  # NOTE: This gets exponentially slower as datasets get bigger, but 
  #       we're going to be lazy and not try to pre-determine the dimensions we need
  dat.all <- rbind(dat.all, dat.core)
  
}
dat.all$Tag <- substr(dat.all$Core.ID, 1, 4)
summary(dat.all)

# ---------------------------------------------
# Reconstructing diameter through time
# What we need to do: 
#  1. Work back through time subtracting latewood & earlywood to reconstruct diameter & basal area
#  2. Working back through time, convert subtract basal areas to get basal area increment
# NOTE: For simplicity and clarity, I'm not making this an efficient loop and am 'hard coding' some dates.
#       If I were doing this on a larger or less uniform dataset, I would do some things differently, but
#       it would probably be harder to follow.
# ---------------------------------------------
# First reconstruct diameter
for(CORE in unique(dat.all$Core.ID)){
  TAG <- unique(dat.all[dat.all$Core.ID==CORE, "Tag"])
  
  # Getting our starting point diameter from the plot survey data
  summary(plotsurvey)
  # put the survey diameter as the latewood diamter for our last year (2017)
  ind.yr <- which(dat.all$Core.ID==CORE & dat.all$year==max(dat.all[dat.all$Core.ID==CORE, "year"])) # Find the row for the year we're working with so I don't have to keep typing it out
  dat.all[ind.yr, "diam.latewood"] <- plotsurvey[plotsurvey$Tag==TAG,"DBH"] 
  # The diameter at the end of the earlywood is the (end of) latewood diameter minus latewood width
  dat.all[ind.yr, "diam.earlywood"] <- dat.all[ind.yr, "diam.latewood"] - dat.all[ind.yr, "latewood"]
  
  # Now that we have the most recent year primed, work backwards 
  # Note that we're basing the year ring off of what we have for each core
  for(YEAR in (max(dat.all[dat.all$Core.ID==CORE, "year"])-1):min(dat.all[dat.all$Core.ID==CORE, "year"])){
    ind.yr <- which(dat.all$Core.ID==CORE & dat.all$year==YEAR) # row index for the year we're working with
    ind.yr1 <- which(dat.all$Core.ID==CORE & dat.all$year==YEAR+1) # row index for the year we need to pull a starting diameter from
    
    dat.all[ind.yr, "diam.latewood"] <- dat.all[ind.yr1, "diam.earlywood"] - dat.all[ind.yr1, "earlywood"] # Note this is based off of year + 1 (e.g. subtract 2017 earlywodo info to get end of 2016)
    dat.all[ind.yr, "diam.earlywood"] <- dat.all[ind.yr, "diam.latewood"] - dat.all[ind.yr, "latewood"] # This is the same as we did for 2017
  }
}

# Covert diameter to basal area; note this will be in cm2
dat.all[c("BA.latewood", "BA.earlywood")] <- pi*(dat.all[c("diam.latewood", "diam.earlywood")]/2)^2

# Now loop through and do something very similar to what we just did to calculate basal area increment
for(CORE in unique(dat.all$Core.ID)){
  # CORE = unique(dat.all$Core.ID)[1]
  # Note that this is different from diameter; here we're subtracting the next year from the current, so we can't go all the way to the min
  for(YEAR in (max(dat.all[dat.all$Core.ID==CORE, "year"])):(min(dat.all[dat.all$Core.ID==CORE, "year"])+1)){
    ind.yr <- which(dat.all$Core.ID==CORE & dat.all$year==YEAR) # Find the row for the year we're working with so I don't have to keep typing it out
    ind.yr1 <- which(dat.all$Core.ID==CORE & dat.all$year==YEAR-1) # row index for the NEXT year we need to pull from (to get the starting point for the ring)
    
    dat.all[ind.yr, "bai.latewood"]  <- dat.all[ind.yr, "BA.latewood"] - dat.all[ind.yr, "BA.earlywood"]
    dat.all[ind.yr, "bai.earlywood"] <- dat.all[ind.yr, "BA.earlywood"] - dat.all[ind.yr1, "BA.latewood"]
    
  }
}

summary(dat.all)
# ---------------------------------------------

# ---------------------------------------------
# Doing some quick graphing to make sure things look okay before saving
# adding group=Core.ID shouldn't be necessary, but I'm putting it just in case we decide to get fancy
# ---------------------------------------------
# Getting a feel for the raw data
ggplot(data=dat.all) +
  geom_line(aes(x=year, y=earlywood, color=Tag, group=Core.ID))
ggplot(data=dat.all) +
  geom_line(aes(x=year, y=latewood, color=Tag, group=Core.ID))

# Making sure diameter increases through time; early & late should be VERY similar
ggplot(data=dat.all) +
  geom_line(aes(x=year, y=diam.earlywood, color=Tag, group=Core.ID))
ggplot(data=dat.all) +
  geom_line(aes(x=year, y=diam.latewood, color=Tag, group=Core.ID))

# Making sure these look somewhat reasonable; 
# Note: These should on the surface not look too diferent from the first pair
ggplot(data=dat.all) +
  geom_line(aes(x=year, y=bai.earlywood, color=Tag, group=Core.ID))
ggplot(data=dat.all) +
  geom_line(aes(x=year, y=bai.latewood, color=Tag, group=Core.ID))
# ---------------------------------------------

# One last check on the tree ring data and then saving it
summary(dat.all)
write.csv(dat.all, "../data_raw/TreeRingData_XylemCores.csv", row.names=F)
