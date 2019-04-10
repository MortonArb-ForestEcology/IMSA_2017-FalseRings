# Analysis of differences in wood hydraulic structure
# Main Question: Are there differenes in hydraulic strategies of red & white oaks?
#   -- Total Conductive Area
#   -- Earlywood Vessel Density
#   -- Mean Earlywood Vessel Size
library(nlme); library(ggplot2)
path.figs <- "/Volumes/GoogleDrive/My Drive/IMSA_SIR/2017_FalseRings_Oros_Ye/SIR 2018-19/Figures/Figures_Christy/"

ring.stats <- read.csv("../data_raw/TreeRingData_XylemCores.csv")
ring.stats <- ring.stats[ring.stats$year>=1980 & ring.stats$year<=1985,]
ring.stats$Species <- car::recode(ring.stats$Species, "'QURU'='Q. rubra'; 'QUAL'='Q. alba'")
summary(ring.stats)


# Note that all of our data is small and zero-truncated
ggplot(data=ring.stats) +
  facet_grid(Species~.) +
  geom_histogram(aes(x=Area.Cond.Tot, fill=Species)) 


png(file.path(path.figs, "TotalConductiveArea.png"), height=5.5, width=7.5, units="in", res=180)
ggplot(data=ring.stats) +
  geom_boxplot(aes(x=Species, y=Area.Cond.Tot, fill=Species)) +
  scale_fill_manual(labels=c("Q. alba\n(White Oak)", "Q. rubra\n(Red Oak)"), values=c("cadetblue3", "coral2")) +
  scale_x_discrete(labels=c("Q. alba\n(White Oak)", "Q. rubra\n(Red Oak)")) +
  scale_y_continuous(name=expression(bold(paste("Total Conductive Area (mm"^"2", " yr"^"-1", ")"))), limits=c(0, 4.5), expand=c(0,0)) +
  guides(fill=F) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y=element_text(size=rel(2), face="bold", color="black"),
        axis.text.x=element_text(size=rel(2), face="bold.italic", color="black"),
        axis.text.y=element_text(size=rel(2), color="black"))
dev.off()

png(file.path(path.figs, "VesselDensity.png"), height=5.5, width=7.5, units="in", res=180)
ggplot(data=ring.stats) +
  geom_boxplot(aes(x=Species, y=Vessel.Density, fill=Species)) +
  scale_fill_manual(labels=c("Q. alba\n(White Oak)", "Q. rubra\n(Red Oak)"), values=c("cadetblue3", "coral2")) +
  scale_x_discrete(labels=c("Q. alba\n(White Oak)", "Q. rubra\n(Red Oak)")) +
  scale_y_continuous(name=expression(bold(paste("Vessel Density (vessels mm"^"-2", " yr"^"-1",")")))) +
  guides(fill=F) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y=element_text(size=rel(2), face="bold", color="black"),
        axis.text.x=element_text(size=rel(2), face="bold.italic", color="black"),
        axis.text.y=element_text(size=rel(2), color="black"))
dev.off()

png(file.path(path.figs, "VesselArea.png"), height=5.5, width=7.5, units="in", res=180)
ggplot(data=ring.stats) +
  geom_boxplot(aes(x=Species, y=Vessel.Area, fill=Species)) +
  scale_fill_manual(labels=c("Q. alba\n(White Oak)", "Q. rubra\n(Red Oak)"), values=c("cadetblue3", "coral2")) +
  scale_x_discrete(labels=c("Q. alba\n(White Oak)", "Q. rubra\n(Red Oak)")) +
  scale_y_continuous(name=expression(bold(paste("Mean Vessel Area (mm"^"2",")")))) +
  guides(fill=F) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y=element_text(size=rel(2), face="bold", color="black"),
        axis.text.x=element_text(size=rel(2), face="bold.italic", color="black"),
        axis.text.y=element_text(size=rel(2), color="black"))
dev.off()

# Comparing Total conductive area across species
# Testing for differences in year; no difference, but we need to keep the year effect so our data is accounted for properly
mod.tca.rml <- lme(Area.Cond.Tot ~ Species, data=ring.stats, random=list(IMLS_Plot=~1, Core.ID=~1, year=~1), method="REML")
mod.tca.rml2 <- lme(Area.Cond.Tot ~ Species, data=ring.stats, random=list(IMLS_Plot=~1, Core.ID=~1), method="REML")
mod.tca.rml3 <- lme(Area.Cond.Tot ~ Species, data=ring.stats, random=list(year=~1, IMLS_Plot=~1, Core.ID=~1), method="REML")
anova(mod.tca.rml, mod.tca.rml2, mod.tca.rml3)


# The difference makes sense, but the fact that QUAL is not significantly differnet from 0 is worrysome
mod.tca <- lme(Area.Cond.Tot ~ Species, data=ring.stats, random=list(year=~1, IMLS_Plot=~1, Core.ID=~1), method="ML")
# mod.tca2 <- lme(Area.Cond.Tot ~ Species, data=ring.stats, random=list(year=~1, IMLS_Plot=~1, Core.ID=~1))
sum.tca <- summary(mod.tca)
sum.tca$coefficients$fixed[2]/sum.tca$coefficients$fixed[1] # Looking at the percent difference

# summary(mod.tca2)
# summary(mod.tca.rml)

hist(resid(mod.tca))
plot(predict(mod.tca) ~ ring.stats$Area.Cond.Tot)
# plot(resid(mod.tca) ~ predict(mod.tca))
summary(predict(mod.tca))


# Looking at differences in Vessel Density
mod.vd <- lme(Vessel.Density ~ Species, data=ring.stats, random=list(year=~1, IMLS_Plot=~1, Core.ID=~1), method="ML")
# mod.vd2 <- lme(Vessel.Density ~ Species, data=ring.stats, random=list(year=~1, IMLS_Plot=~1, Core.ID=~1))
summary(mod.vd)
summary(mod.vd2)

# Looking at the percent difference
sum.vd <- summary(mod.vd)
sum.vd$coefficients$fixed[2]/sum.vd$coefficients$fixed[1] # Looking at the percent difference

hist(resid(mod.vd))
plot(predict(mod.vd) ~ ring.stats$Vessel.Density)
# plot(resid(mod.vd) ~ predict(mod.vd))

hist(resid(mod.vd2))
plot(predict(mod.vd2) ~ ring.stats$Vessel.Density)
# plot(resid(mod.vd2) ~ predict(mod.vd2))


# Looking at differences in mean vessel area
mod.va <- lme(Vessel.Area ~ Species, data=ring.stats, random=list(year=~1, IMLS_Plot=~1, Core.ID=~1), method="ML")
# mod.va2 <- lme(Vessel.Area ~ Species, data=ring.stats, random=list(year=~1, IMLS_Plot=~1, Core.ID=~1))
summary(mod.va)
# summary(mod.va2)

# Looking at the percent difference
sum.va <- summary(mod.va)
sum.va$coefficients$fixed[2]/sum.va$coefficients$fixed[1] # Looking at the percent difference

hist(resid(mod.va))
plot(predict(mod.va) ~ ring.stats$Vessel.Area)
# plot(resid(mod.va) ~ predict(mod.va))

hist(resid(mod.va2))
plot(predict(mod.va2) ~ ring.stats$Vessel.Area)
# plot(resid(mod.va2) ~ predict(mod.va2))
