### install if necessary, and open tidyverse library
#install.packages("tidyverse")
library("tidyverse")

### read in data file made from script 1
Dat <- read_csv("SeedPredationDataForAnalysis.csv")
  

### take a look at distributions of removal rates
library(ggplot2)
ggplot(Dat, aes(Prop_rmv)) + geom_histogram()
ggplot(Dat, aes(Prop_rmv, fill=Species)) + geom_histogram()
ggplot(Dat, aes(Prop_rmv, fill=Canopy)) + geom_histogram()


### mean of proportion removed
Prop_rmv <- Dat$Prop_rmv
mean(Prop_rmv)

### mean of sunflower seeds removed 
Species <- Dat$Species
Dat$Species<- as.factor(Dat$Species)

sun <- Dat[which(Dat$Species == "sun"),]
mean(sun$Prop_rmv)

### mean of oats removed
oats <- Dat[which(Dat$Species == "oats"),]
mean(oats$Prop_rmv)


###  predation x elevation x canopy graphs 

ggplot(Dat, aes(x=SiteElevation, y=Prop_rmv, colour = Canopy)) + 
  geom_point() +
  geom_smooth(method="glm")


### let's do some stats
library(lme4)
library(lmerTest)

# this model runs but we know it's inappropriate because it doesn't deal wtih bounded nature of response variable
mod1 <- lmer(Prop_rmv ~ SiteElevation*Canopy + (1|Site), data=Dat)
summary(mod1)

# this is appropritate but won't run
mod2 <- glmer(Prop_rmv ~ SiteElevation*Canopy + (1|Site), family="binomial", control= glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000)), data=Dat)

# this has proper handling of bounded response variable but has pseudoreplication by dropping random effect
mod3 <- glm(Prop_rmv ~ SiteElevation*Canopy, family="binomial", data=Dat)
summary(mod3)
library(visreg)
visreg(mod3, xvar="SiteElevation", by="Canopy", overlay=T)










