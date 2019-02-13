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
xylem.df$Tag <- as.factor(substr(xylem.df$Tree.ID, 1, 4)) # Making something that corresponds to columns in the plot survey data
summary(xylem.df)

# 1.2 Reading in Tree Data
plotsurvey <- read.csv("../data_raw/TreeSurveyData/Tree_PlotSurvey_2017 - raw data.csv", na.strings="")
#survey.add <- read.csv("./TreeSurveyData/Trees_Survey_Additional_URF_2017 - raw_data.csv")
summary(plotsurvey)



# 2.1 Subset to just the trees Jessica needs
xylem.df2 <- merge(xylem.df, plotsurvey, by = "Tag", match = "all")


# 3. Read in the tree ring measurements for Jessica's trees
path.rw <- "../data_raw/RawRingWidths/"
rwl.all <- dir(path.rw, ".rwl")
rwl.all[1:10]

rw.test <- list()
for (k in 1:length(rwl.all)){
  rw.test[[k]] <- dplR::read.rwl(file.path(path.rw, rwl.all[k]))
}
str(rw.test[[1]])


rw.df <- data.frame(matrix(unlist(rw.test), nrow=210, byrow=F)) #unlisting and putting into 1 df
