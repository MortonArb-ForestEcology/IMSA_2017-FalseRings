# Analysis of differences in wood hydraulic structure
# Main Question: Are there differenes in hydraulic strategies of red & white oaks?
#   -- Total Conductive Area
#   -- Earlywood Vessel Density
#   -- Mean Earlywood Vessel Size
library(nlme); library(ggplot2)

ring.stats <- read.csv("../data_raw/TreeRingData_XylemCores.csv")
summary(ring.stats)


# Note that all of our data is small and zero-truncated
ggplot(data=ring.stats) +
  facet_grid(Species~.) +
  geom_histogram(aes(x=Area.Cond.Tot, fill=Species))

# Comparing Total conductive area across species
# Testing for differences in year; no difference, but we need to keep the year effect so our data is accounted for properly
mod.tca.rml <- lme(Area.Cond.Tot ~ Species, data=ring.stats, random=list(IMLS_Plot=~1, Core.ID=~1, year=~1), method="REML")
mod.tca.rml2 <- lme(Area.Cond.Tot ~ Species, data=ring.stats, random=list(IMLS_Plot=~1, Core.ID=~1), method="REML")
mod.tca.rml3 <- lme(Area.Cond.Tot ~ Species, data=ring.stats, random=list(year=~1, IMLS_Plot=~1, Core.ID=~1), method="REML")
anova(mod.tca.rml, mod.tca.rml2, mod.tca.rml3)


# The difference makes sense, but the fact that QUAL is not significantly differnet from 0 is worrysome
mod.tca <- lme(Area.Cond.Tot ~ Species, data=ring.stats, random=list(year=~1, IMLS_Plot=~1, Core.ID=~1), method="ML")
mod.tca2 <- lme(Area.Cond.Tot ~ Species, data=ring.stats, random=list(year=~1, IMLS_Plot=~1, Core.ID=~1))
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
mod.vd2 <- lme(Vessel.Density ~ Species, data=ring.stats, random=list(year=~1, IMLS_Plot=~1, Core.ID=~1))
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
mod.va2 <- lme(Vessel.Area ~ Species, data=ring.stats, random=list(year=~1, IMLS_Plot=~1, Core.ID=~1))
summary(mod.va)
summary(mod.va2)

# Looking at the percent difference
sum.va <- summary(mod.va)
sum.va$coefficients$fixed[2]/sum.va$coefficients$fixed[1] # Looking at the percent difference

hist(resid(mod.va))
plot(predict(mod.va) ~ ring.stats$Vessel.Area)
# plot(resid(mod.va) ~ predict(mod.va))

hist(resid(mod.va2))
plot(predict(mod.va2) ~ ring.stats$Vessel.Area)
# plot(resid(mod.va2) ~ predict(mod.va2))
