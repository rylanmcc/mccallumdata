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

### mean by day of study
plot(Prop_rmv ~ Date2, data=Dat)

###  predation x elevation x canopy graphs 

ggplot(Dat, aes(x=SiteElevation, y=Prop_rmv, colour = Canopy)) + 
  geom_point() +
  geom_smooth(method="glm") + xlab("Site Elevation") + ylab("Proportion of Seeds Removed") 


### let's do some stats
library(lme4)
library(lmerTest)

# this model runs but we know it's inappropriate because it doesn't deal wtih bounded nature of response variable
mod1 <- lmer(Prop_rmv ~ SiteElevation*Canopy + (1|Species) + (1|Date2) + (1|Site), data=Dat)
summary(mod1)
library(visreg)
visreg(mod1, xvar="SiteElevation", by="Canopy", overlay=T)


############################

#### scaled canopy but it is only two categories, do not know if that is allowed**? 

### changing canopy into a numerical variable to scale it 
##### closed = 1 , open = 2 

Dat$CanopyNum <- ifelse(Dat$Canopy == 'closed', 2, 1)

###install.packages("lme4")
library(lme4)

#Try scaling predictor variables
####install.packages("standardize")
library(standardize)
Dat$scaledsitelev <- scale(Dat$SiteElevation)
###Dat$scaledCanopy <- scale(Dat$CanopyNum)


mod2 <- glmer(Prop_rmv ~ scaledsitelev*Canopy + (1|Species) + (1|Date2) + (1|Site), family="binomial", control= glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000)), data=Dat)

visreg(mod2, xvar="scaledsitelev", by="Canopy", overlay=T, scale ="response")
summary(mod2)

install.packages("ggeffects")
library(ggeffects)

ggtest <- ggpredict(mod2,terms=c("scaledsitelev","Canopy"))
plot(ggtest, rawdata = TRUE )


############################


# this is appropriate but won't run (back up cause i'm playing around to try and get mod2 working)
mod2 <- glmer(Prop_rmv ~ SiteElevation*Canopy + (1|Species) + (1|Date2) + (1|Site), family="binomial", control= glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000)), data=Dat)

# this has proper handling of bounded response variable but has pseudoreplication by dropping random effect of Site and doesn't adjust for differences between species or sampling dates
mod3 <- glm(Prop_rmv ~ SiteElevation*Canopy, family="binomial", data=Dat)
summary(mod3)
library(visreg)
visreg(mod3, xvar="SiteElevation", by="Canopy", overlay=T, scale ="response")


### unbounded general linear model comparing Species types removed across elevation gradient 

Species <- Dat$Species

ggplot(Dat, aes(x=SiteElevation, y=Prop_rmv, colour = Species)) + 
  geom_point() +
  geom_smooth(method="glm") + theme_minimal() 




