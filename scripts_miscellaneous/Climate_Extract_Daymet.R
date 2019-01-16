# Extract climate data from Daymet so we can custom-build our met dataset


# Sourcing a pre-existing function that can be found here:
# https://github.com/crollinson/R_Functions

source("~/Desktop/Research/R_Functions/extract_Daymet.R")


# Download the met data (note: this only needs to be done once!)
met.out <- download.Daymet(outfolder="/Volumes/GoogleDrive/My Drive/East Woods/IMSA_2017_Rollinson/Daymet/", 
                            start_date="1980-01-01", end_date="2017-12-31", 
                            site_id="MortonArb", 
                            lat.in=41.81639, lon.in=-88.0438,
                            vars=c("dayl", "prcp", "srad", "swe", "tmax", "tmin", "vp")) 
  
