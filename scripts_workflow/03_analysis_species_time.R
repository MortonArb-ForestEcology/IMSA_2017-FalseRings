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


# Looking at interannual variability in vessel density
mod.vd.t <- lme(Vessel.Density ~ Species*as.factor(year), data=ring.stats, random=list(IMLS_Plot=~1, Core.ID=~1), method="ML")
# summary(mod.vd.t) # this is a mess; don't look at it
anova(mod.vd.t)

mod.tca.t <- lme(Area.Cond.Tot ~ Species*as.factor(year), data=ring.stats, random=list(IMLS_Plot=~1, Core.ID=~1), method="ML")
# summary(mod.vd.t) # this is a mess; don't look at it
anova(mod.tca.t)


mod.va.t <- lme(Vessel.Area ~ Species*as.factor(year), data=ring.stats, random=list(IMLS_Plot=~1, Core.ID=~1), method="ML")
# summary(mod.vd.t) # this is a mess; don't look at it
anova(mod.va.t)
