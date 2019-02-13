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
plot survey data
summary(xylem.df)

# Doing some additional data cleaning
unique(xylem.df$Tree.ID) # --> note that some have spaces, some don't
xylem.df$Tree.ID <- sub(" ", "", xylem.df$Tree.ID) # Get rid of the spaces by replacing them with nothing
xylem.df$core <- substr(xylem.df$Tree.ID, 5,5) # Most trees will ahve 2 cores
xylem.df$Core.ID <- paste(xylem.df$Tag, xylem.df$core, sep="-") # Trying to match things to 

# 1.2 Reading in Tree Data
plotsurvey <- read.csv("../data_raw/TreeSurveyData/Tree_PlotSurvey_2017 - raw data.csv", na.strings="")
#survey.add <- read.csv("./TreeSurveyData/Trees_Survey_Additional_URF_2017 - raw_data.csv")
summary(plotsurvey)



# 2.1 Subset to just the trees Jessica needs
xylem.df2 <- merge(xylem.df, plotsurvey, by = "Tag", match = "all")


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
  dat.core <- data.frame(Core.ID=CORE, year = as.numeric(row.names(rw.core)), 
                         earlywood=ew.core[,1], latewood=lw.core[,1], ring=rw.core[,1])
  
  # append (rbind) the data into what we already have;
  # NOTE: This gets exponentially slower as datasets get bigger, but 
  #       we're going to be lazy and not try to pre-determine the dimensions we need
  dat.all <- rbind(dat.all, dat.core)
  
}
summary(dat.all)

write.csv(dat.all, "../data_raw/TreeRingData_XylemCores.csv", row.names=F)
