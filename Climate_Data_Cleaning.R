path.met <- "/Volumes/GoogleDrive/My Drive/East Woods/IMSA_2017_Rollinson/Daymet/MortonArb"

files.met <- dir(path.met, ".nc")

dat.all <- data.frame()
pb <- txtProgressBar(min=0, max=length(files.met), style=3)
for(i in 1:length(files.met)){
	yr.now <- as.numeric(stringr::str_split(files.met[i], "[.]")[[1]][2])	
	
	ncT <- ncdf4::nc_open(file.path(path.met, files.met[i]))

	dat.tmp <- data.frame(year   = yr.now, 
	                      doy    = ncdf4::ncvar_get(ncT, "time"),
	                      tmin   = ncdf4::ncvar_get(ncT, "minimum_air_temperature"),
	                      tmax   = ncdf4::ncvar_get(ncT, "maximum_air_temperature"),
	                      prcp   = ncdf4::ncvar_get(ncT, "precipitation_flux"),
	                      swe    = ncdf4::ncvar_get(ncT, "liquid_water_content_of_surface_snow"),
	                      swdown = ncdf4::ncvar_get(ncT, "surface_downwelling_shortwave_flux_in_air"),
	                      dayl   = ncdf4::ncvar_get(ncT, "day_length"), 
	                      vp     = ncdf4::ncvar_get(ncT, "water_vapor_partial_pressure_in_air"))
	                      
	# Calculating growing degree-days
	gdd <- apply(dat.tmp[,c("tmax", "tmin")], 1, mean) - 273.15 - 5	                      
	gdd[gdd<0] <- 0
	
	for(i in 2:length(gdd)){
		gdd[i] <- sum(gdd[(i-1):i])
	}
	dat.tmp$GDD5 <- gdd
	
	
	dat.all <- rbind(dat.all, dat.tmp)
	
	setTxtProgressBar(pb, i)
}

write.csv(dat.all, "/Volumes/GoogleDrive/My Drive/East Woods/IMSA_2017_Rollinson/ClimateData_Daymet.csv", row.names=F)

dat.stack <- stack(dat.all[,vars])
dat.stack$year <- dat.all$year
dat.stack$doy <- dat.all$doy
summary(dat.stack)

met.agg <- aggregate(dat.stack[,"values"], by= dat.stack[,c("ind", "doy")], FUN=mean)
names(met.agg)[3] <- "mean"
met.agg$sd <- aggregate(dat.stack[,"values"], by= dat.stack[,c("ind", "doy")], FUN=sd)[,3]
met.agg$ci.lwr <- aggregate(dat.stack[,"values"], by= dat.stack[,c("ind", "doy")], FUN=quantile, 0.025)[,3]
met.agg$ci.upr <- aggregate(dat.stack[,"values"], by= dat.stack[,c("ind", "doy")], FUN=quantile, 0.975)[,3]
summary(met.agg)

library(ggplot2)
ggplot(data=met.agg[,]) +
	facet_wrap(~ind, scales="free_y") +
	geom_ribbon(aes(x=doy, ymin=mean-sd, ymax=mean+sd), alpha=0.5) +
	# geom_ribbon(aes(x=doy, ymin=ci.lwr, ymax=ci.upr), alpha=0.5) +
	geom_line(aes(x=doy, y=mean))
	
set.seed(231); yrs.rand <- sample(1980:2017, 5)	
ggplot(data=met.agg[met.agg$doy<185 & met.agg$ind=="GDD5", ]) +
	geom_ribbon(aes(x=doy, ymin=ci.lwr, ymax=ci.upr), alpha=0.5) +
	geom_line(aes(x=doy, y=mean), size=2) +
	geom_line(data= dat.stack[dat.stack $doy<185 & dat.stack$ind=="GDD5" & dat.stack$year %in% yrs.rand, ], aes(x=doy, y=values, group=year), size=0.5) +
	geom_line(data= dat.stack[dat.stack $doy<185 & dat.stack$ind=="GDD5" & dat.stack$year==1995, ], aes(x=doy, y=values, group=year), size=0.75, color="red")
	
