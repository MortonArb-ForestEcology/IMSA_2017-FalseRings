# Analysis of differences in wood hydraulic structure
# Main Question: Are there differenes in hydraulic strategies of red & white oaks?
#   -- Total Conductive Area
#   -- Earlywood Vessel Density
#   -- Mean Earlywood Vessel Size
library(nlme); library(ggplot2)
path.figs <- "/Volumes/GoogleDrive/My Drive/OakVessels_Oros/Figures/"

ring.stats <- read.csv("../data_raw/TreeRingData_XylemCores.csv")
ring.stats <- ring.stats[ring.stats$year>=1980,]
ring.stats$Species <- car::recode(ring.stats$Species, "'QURU'='Q. rubra'; 'QUAL'='Q. alba'")
ring.stats <- ring.stats[ring.stats$Canopy!="I",]
summary(ring.stats)


# ---------------------------
# Comparing interannual variability of different ring traits
# ---------------------------
# Making an exploratory figure
ring.stats.stack <- stack(ring.stats[,c("Vessel.Area", "Vessel.Density", "Area.Cond.Tot", "bai.latewood", "bai.earlywood")])
ring.stats.stack[,c("Tag", "Sp_code", "Species", "year")] <- ring.stats[,c("Tag", "Sp_code", "Species", "year")]
summary(ring.stats.stack)

ring.stats.agg <- aggregate(ring.stats.stack[,"values"],
                            by=ring.stats.stack[,c("Sp_code", "Species", "year", "ind")],
                            FUN=mean)
names(ring.stats.agg)[names(ring.stats.agg)=="x"] <- "value.mean"
ring.stats.agg$value.sd <- aggregate(ring.stats.stack[,"values"],
                                     by=ring.stats.stack[,c("Sp_code", "Species", "year", "ind")],
                                     FUN=sd)$x
ring.stats.agg$value.LB <- aggregate(ring.stats.stack[,"values"],
                                     by=ring.stats.stack[,c("Sp_code", "Species", "year", "ind")],
                                     FUN=quantile, 0.025)$x
ring.stats.agg$value.UB <- aggregate(ring.stats.stack[,"values"],
                                     by=ring.stats.stack[,c("Sp_code", "Species", "year", "ind")],
                                     FUN=quantile, 0.975)$x
ring.stats.agg$Sp_code <- factor(ring.stats.agg$Sp_code, levels=c("QUAL", "QURU"))
ring.stats.agg$ind <- factor(ring.stats.agg$ind, levels=c( "Vessel.Density", "Vessel.Area", "Area.Cond.Tot", "bai.earlywood", "bai.latewood"))
summary(ring.stats.agg)

png(file.path(path.figs, "TimeSeries.png"), height=6, width=6, units="in", res=180)
ggplot(data=ring.stats.agg) +
  facet_grid(ind ~ ., scales="free_y") +
  # geom_ribbon(aes(x=year, ymin=value.mean - value.sd, ymax=value.mean + value.sd, fill=Sp_code), alpha=0.5) +
  geom_ribbon(aes(x=year, ymin=value.LB, ymax=value.UB, fill=Species), alpha=0.5) +
  geom_line(aes(x=year, y=value.mean, color=Species), size=1.5) +
  scale_color_manual(name="Species", values=c("darkslategray4", "coral3")) +
  scale_fill_manual(name="Species", values=c("darkslategray4", "coral3")) +
  scale_x_continuous(expand=c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position="top",
        legend.text = element_text(face="italic"))
dev.off()


# Looking at interannual variability in vessel density
mod.vd.t <- lme(Vessel.Density ~ Species*as.factor(year), data=ring.stats, random=list(IMLS_Plot=~1, Core.ID=~1), method="ML")
# summary(mod.vd.t) # this is a mess; don't look at it
anova(mod.vd.t)

# Total Conductive Area
mod.tca.t <- lme(Area.Cond.Tot ~ Species*as.factor(year), data=ring.stats, random=list(IMLS_Plot=~1, Core.ID=~1), method="ML")
# summary(mod.vd.t) # this is a mess; don't look at it
anova(mod.tca.t)

# Mean Vessel Area
mod.va.t <- lme(Vessel.Area ~ Species*as.factor(year), data=ring.stats, random=list(IMLS_Plot=~1, Core.ID=~1), method="ML")
# summary(mod.vd.t) # this is a mess; don't look at it
anova(mod.va.t)

# Earlywood Basal Area Increment
mod.ebai.t <- lme(bai.earlywood ~ Species*as.factor(year), data=ring.stats, random=list(IMLS_Plot=~1, Core.ID=~1), method="ML")
# summary(mod.vd.t) # this is a mess; don't look at it
anova(mod.ebai.t)

# Latewood Basal Area Increment
mod.lbai.t <- lme(bai.latewood ~ Species*as.factor(year), data=ring.stats, random=list(IMLS_Plot=~1, Core.ID=~1), method="ML")
# summary(mod.vd.t) # this is a mess; don't look at it
anova(mod.lbai.t)
# ---------------------------


# ---------------------------
# Trying some daily climate correlations with different stats
# ---------------------------
# Defining our climate predictors and response variabels of interest up front
vars.pred <- c("prcp.mm", "tmax.C", "tmin.C", "vp.Pa", "srad.Wm2")
vars.resp <- c("bai.latewood", "bai.earlywood", "Area.Cond.Tot", "Vessel.Area", "Vessel.Density")

# Read in daily climate data
dat.daymet <- read.csv("/Volumes/GoogleDrive/My Drive/Arboretum Met Data/Daymet/MortonArb-VisitorCenter/Daymet_MortonArb_1980-2018.csv")

# Doing a 7-day smoothing of the met data; anchored on the date so that it's a cumulative effect
for(i in 1:length(vars.pred)){
  dat.daymet[,paste0(vars.pred[i], ".wk")] <- zoo::rollapply(dat.daymet[,vars.pred[i]], width=7, align="right", FUN=mean, fill=NA)
  
}

# Setting up a ~6-month lag (after the solstice)
dat.lag <- dat.daymet[dat.daymet$yday>172 & dat.daymet$yday<366,]
dat.lag$year <- dat.lag$year+1
dat.lag$yday <- dat.lag$yday-365
summary(dat.lag)

dat.daymet <- rbind(dat.daymet, dat.lag)
summary(dat.daymet)

# Bring the met data & ring data together
dat.all <- merge(ring.stats, dat.daymet[,4:ncol(dat.daymet)], all.x=T)
summary(dat.all)

# vars.pred <- vars.pred[c(1,3)]
# vars.resp <- vars.resp[c(2,4)]

spp.use <- unique(dat.all$Species)
days.use <- min(dat.all$yday):max(dat.all$yday)
mod.out <- data.frame(yday=rep(days.use), 
                      species=rep(rep(spp.use, each=length(days.use)), length.out=length(days.use)*length(spp.use)*length(vars.resp)*length(vars.pred)),
                      resp=rep(rep(vars.resp, each=length(days.use)*length(spp.use)), length.out=length(days.use)*2*length(vars.resp)*length(vars.pred)),
                      pred=rep(vars.pred, each=length(days.use)*length(spp.use)*length(vars.resp)), 
                      t.stat=NA, p.val=NA, r.sq.m=NA)
mod.out$resp <- factor(mod.out$resp, levels=rev(vars.resp))
mod.out$pred <- factor(mod.out$pred, levels=vars.pred)
summary(mod.out)
summary(mod.out[mod.out$species=="Q. rubra",])

summary(mod.out[mod.out$species=="Q. rubra" & mod.out$resp=="Vessel.Area",])
summary(mod.out[mod.out$species=="Q. rubra" & mod.out$pred=="tmax.C",])
summary(mod.out[mod.out$species=="Q. rubra" & mod.out$pred=="prcp.mm",])

# head(mod.out)
# tail(mod.out)

# Looping through all of the models we could possibly want
pb <- txtProgressBar(min=0, max=nrow(mod.out), style = 3)
pb.ind <- 1
for(SPP in unique(dat.all$Species)){
  for(i in days.use){
    dat.tmp <- dat.all[dat.all$Species==SPP & dat.all$yday==i,]
    for(VAR in vars.pred){
      # dat.tmp$PRED <- dat.tmp[,VAR]
      dat.tmp$PRED <- dat.tmp[,paste0(VAR, ".wk")]
      
      if(VAR=="prcp.mm"){
        dat.tmp$PRED[dat.tmp$PRED==0] <- 1e-3
        dat.tmp$PRED <- log(dat.tmp$PRED)
      } 
      
      for(RESP in vars.resp){
        # Update our Progress bar
        setTxtProgressBar(pb, pb.ind)
        pb.ind = pb.ind+1
        
        # Set up the response variable for our model to make it generalzied
        dat.tmp$RESP <- dat.tmp[,RESP]

        # Run a simple mixed-effect model & save the summary so we can get the t-table
        mod.var <- lme(RESP ~ PRED, random=list(IMLS_Plot=~1, year=~1, Core.ID=~1), data=dat.tmp[,], na.action=na.omit)
        mod.sum <- summary(mod.var)
        
        # Save our t-stat & pvalue for the climate predictor
        out.ind <- which(mod.out$species==SPP & mod.out$pred==VAR & mod.out$yday==i & mod.out$resp==RESP)
        mod.out[out.ind, "t.stat"] <- mod.sum$tTable["PRED","t-value"]
        mod.out[out.ind, "p.val"] <- mod.sum$tTable["PRED","p-value"]
        mod.out[out.ind, "r.sq.m"] <- MuMIn::r.squaredGLMM(mod.var)[,"R2m"]
      }
    }
  }
}
summary(mod.out)
summary(mod.out[mod.out$species=="Q. rubra",])

write.csv(mod.out, file.path(path.figs, "ClimateCorrs_Daily.csv"), row.names=F)
# ---------------------------

# ---------------------------
# Graphing results
# ---------------------------
library(ggplot2)
mod.out <- read.csv(file.path(path.figs, "ClimateCorrs_Daily.csv"))
mod.out$pred <- car::recode(mod.out$pred, "'prcp.mm'='Precipitation'; 'tmax.C'='Max Temperature'; 'tmin.C'='Min Temperature'; 'vp.Pa'='Vapor Pressure Deficit'; 'srad.Wm2'='Shortwave Radiation'")
mod.out$resp <- car::recode(mod.out$resp, "'Area.Cond.Tot'='Tot. Cond. Area'; 'bai.earlywood'='Earlywood BAI'; 'bai.latewood'='Latewood BAI'; 'Vessel.Area'='Mean Vessel Area'; 'Vessel.Density'='Vessel Density'")
summary(mod.out)

mod.out$pred <- factor(mod.out$pred, levels=c("Min Temperature", "Max Temperature", "Precipitation", "Vapor Pressure Deficit", "Shortwave Radiation"))
mod.out$resp <- factor(mod.out$resp, levels=c("Latewood BAI", "Earlywood BAI", "Tot. Cond. Area", "Mean Vessel Area", "Vessel Density"))

yrs.mark <- data.frame(Label=c("p.Oct 1", "Jan 1", "Apr 1", "Jul 1", "Oct 1"), 
                       Date=c("2018-10-01", "2019-01-01", "2019-04-01", "2019-07-01", "2019-10-01"))
yrs.mark$mark.yday <- lubridate::yday(yrs.mark$Date)
yrs.mark$mark.yday[1] <- yrs.mark$mark.yday[1]-365

RdBu5 <- c("#ca0020", "#f4a582", "#f7f7f7", "#92c5de", "#0571b0")
RdBu5.b <- c("#ca0020", "#f4a582", "gray50", "#92c5de", "#0571b0")
PRGn5 <- c("#7b3294", "#c2a5cf", "gray50", "#a6dba0", "#008837")

png(file.path(path.figs, "ClimateEffects_daily_smooth07day_t-stat_sig_poster.png"), height=5, width=15, units="in", res=180)
dat.sub <- mod.out$resp %in% c("Tot. Cond. Area", "Mean Vessel Area", "Vessel Density") &
  mod.out$pred %in% c("Precipitation", "Max Temperature", "vp.Pa")
ggplot(data=mod.out[dat.sub, ]) +
  facet_grid(resp~pred) +
  geom_tile(data=mod.out[dat.sub & mod.out$p.val>=0.05,], aes(x=yday, y=species), fill="gray50") +
  geom_tile(data=mod.out[dat.sub & mod.out$p.val<0.05,], aes(x=yday, y=species, fill=t.stat)) +
  # geom_tile(aes(x=yday, y=resp, fill=t.stat)) +
  geom_vline(xintercept = 0, linetype="dashed") +
  scale_y_discrete(name="Response Variable", expand=c(0,0)) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=yrs.mark$mark.yday, labels = yrs.mark$Label) +
  scale_fill_gradientn(name="t-stat\n(sig. only)", colors=PRGn5, limits=max(mod.out$t.stat)*c(-1,1))+
  theme(legend.position="right",
        legend.title=element_text(hjust=0.5),
        strip.text.x = element_text(size=rel(1.5), face="bold", color="black"),
        strip.text.y = element_text(size=rel(1.25), face="bold", color="black"),
        axis.title = element_text(size=rel(1.5), face="bold", color="black"),
        axis.text = element_text(size=rel(1.25), color="black")) 
dev.off()


png(file.path(path.figs, "ClimateEffects_daily_smooth07day_t-stat_sig.png"), height=11, width=8, units="in", res=180)
ggplot(data=mod.out) +
  facet_grid(pred~species) +
  geom_tile(data=mod.out[mod.out$p.val>=0.05,], aes(x=yday, y=resp), fill="gray50") +
  geom_tile(data=mod.out[mod.out$p.val<0.05,], aes(x=yday, y=resp, fill=t.stat)) +
  # geom_tile(aes(x=yday, y=resp, fill=t.stat)) +
  geom_vline(xintercept = 0, linetype="dashed") +
  scale_y_discrete(name="Response Variable", expand=c(0,0)) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=yrs.mark$mark.yday, labels = yrs.mark$Label) +
  scale_fill_gradientn(name="t-stat", colors=PRGn5, limits=max(mod.out$t.stat)*c(-1,1))+
  theme()
dev.off()



png(file.path(path.figs, "ClimateEffects_daily_smooth07day_r-val_all.png"), height=11, width=8, units="in", res=180)
ggplot(data=mod.out) +
  facet_grid(pred~species) +
  # geom_tile(data=mod.out[mod.out$p.val>=0.05,], aes(x=yday, y=resp), fill="gray50") +
  # geom_tile(data=mod.out[mod.out$p.val<0.05,], aes(x=yday, y=resp, fill=t.stat)) +
  geom_tile(aes(x=yday, y=resp, fill=sqrt(r.sq.m)*sign(t.stat))) +
  geom_vline(xintercept = 0, linetype="dashed") +
  scale_y_discrete(name="Response Variable", expand=c(0,0)) +
  scale_x_continuous(name="Day of Year (julian)", expand=c(0,0)) +
  scale_fill_gradientn(name="Marginal\nR-value", colors=PRGn5, limits=max(sqrt(mod.out$r.sq.m))*c(-1,1))+
  theme()
dev.off()
# ---------------------------
