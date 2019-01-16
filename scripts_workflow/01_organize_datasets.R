# Purpose: This script will gather all of the tree data we need and put it into a single data frame.
# Authors: Christy Rollinson, Bethany Zumwalde, Jessica Oros
# 
# Workflow:
# 1. Read in tree data & Jessica's Data
# 2. Subset to just the trees Jessica needs
# 3. Read in the tree ring measurements for Jessica's trees
# ------ Maybe split into new script? ------ 
# 4. Calculate diameter for each EW/LW boundary point
# 5. Calculate cumulative area for each landmark (EW/LW boundary point)
# 6. Calculate area of each latewood/earlywood portion (EWA, LWA)



library(dplyr); library(ggplot2); library(stringr); library(googlesheets)

#Reading in Jessica's data
xylem <- gs_title("Xylem_Data")
xylem.df <- data.frame(gs_read(xylem, ws="raw data"))
xylem.df$Species.of.Tree <- as.factor(xylem.df$Species.of.Tree)
xylem.df$Tree.ID <- as.factor(xylem.df$Tree.ID)
xylem.df$Vessel <- as.factor(xylem.df$Vessel)
summary(xylem.df)

#Reading in Tree Data
plotsurvey <- read.csv("../data_raw/TreeSurveyData/Tree_PlotSurvey_2017 - raw data.csv", na.strings="")
#survey.add <- read.csv("./TreeSurveyData/Trees_Survey_Additional_URF_2017 - raw_data.csv")
summary(plotsurvey)
