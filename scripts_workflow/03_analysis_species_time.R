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
summary(ring.stats)


# ---------------------------
# Comparing interannual variability of different ring traits
# ---------------------------
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
      
      for(RESP in vars.resp){
        # Update our Progress bar
        setTxtProgressBar(pb, pb.ind)
        pb.ind = pb.ind+1
        
        # Set up the response variable for our model to make it generalzied
        dat.tmp$RESP <- dat.tmp[,RESP]
        if(RESP=="prcp.mm"){
          dat.tmp$RESP[dat.tmp$RESP==0] <- 1e-3
          dat.tmp$RESP <- log(dat.tmp$RESP)
        } 
        
        # Run a simple mixed-effect model & save the summary so we can get the t-table
        mod.var <- lme(RESP ~ PRED, random=list(IMLS_Plot=~1, Core.ID=~1), data=dat.tmp[,], na.action=na.omit)
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
# ---------------------------

# ---------------------------
# Graphing results
# ---------------------------
library(ggplot2)

RdBu5 <- c("#ca0020", "#f4a582", "#f7f7f7", "#92c5de", "#0571b0")
RdBu5.b <- c("#ca0020", "#f4a582", "gray50", "#92c5de", "#0571b0")
PRGn5 <- c("#7b3294", "#c2a5cf", "gray50", "#a6dba0", "#008837")

png(file.path(path.figs, "ClimateEffects_daily_smooth07day_t-stat_sig.png"), height=11, width=8, units="in", res=180)
ggplot(data=mod.out) +
  facet_grid(pred~species) +
  geom_tile(data=mod.out[mod.out$p.val>=0.05,], aes(x=yday, y=resp), fill="gray50") +
  geom_tile(data=mod.out[mod.out$p.val<0.05,], aes(x=yday, y=resp, fill=t.stat)) +
  # geom_tile(aes(x=yday, y=resp, fill=t.stat)) +
  geom_vline(xintercept = 0, linetype="dashed") +
  scale_y_discrete(name="Response Variable", expand=c(0,0)) +
  scale_x_continuous(name="Day of Year (julian)", expand=c(0,0)) +
  scale_fill_gradientn(name="t-stat", colors=rev(RdBu5.b), limits=max(mod.out$t.stat)*c(-1,1))+
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
  scale_fill_gradientn(name="Marginal\nR-value", colors=rev(RdBu5.b), limits=max(sqrt(mod.out$r.sq.m))*c(-1,1))+
  theme()
dev.off()
# ---------------------------
