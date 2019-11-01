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
sun <- Dat[which(Dat$Species == "sun"),]
mean(sun$Prop_rmv)

### mean of oats removed
oats <- Dat[which(Dat$Species == "oats"),]
mean(oats$Prop_rmv)

### mean by day of study
plot(Prop_rmv ~ Date2, data=Dat)

##### ASK AMY ABOUT SITE ELEVATION ##################

SiteElevation <- dat$SiteElevation.x

###  predation x elevation x canopy graph
#install.packages("ggpmisc")
library(dplyr) 
library(ggplot2)
library(ggpmisc)
ggplot(Dat, aes(x=SiteElevation, y=Prop_rmv, colour = Canopy)) + geom_point() + geom_smooth(method="glm") + xlab("Site Elevation") + ylab("Proportion of Seeds Removed") +stat_fit_glance(method = 'lm', method.args = list(formula = formula),geom = 'text',aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")), label.x.npc = 'right', label.y.npc = 0.35, size = 3)



ggplot(Dat, aes(x= SiteElevation, y= Prop_rmv, color = Canopy)) + geom_point() + geom_smooth() + geom = 'text' + aes((label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),  label.x.npc = 'right', label.y.npc = 0.35, size = 3)

##########









### let's do some stats
library(lme4)
library(lmerTest)

# this model runs but we know it's inappropriate because it doesn't deal wtih bounded nature of response variable
mod1 <- lmer(Prop_rmv ~ SiteElevation*Canopy + (1|Species) + (1|Date2) + (1|Site), data=Dat)
summary(mod1)
library(visreg)
visreg(mod1, xvar="SiteElevation", by="Canopy", overlay=T)


############################
#### Trying to fix mod2 - scaled elevation to make it work

### changing canopy into a numerical variable to scale it 
##### closed = 1 , open = 2 

Dat$CanopyNum <- ifelse(Dat$Canopy == 'closed', 2, 1)

###install.packages("lme4")
library(lme4)

#Try scaling predictor variables
####install.packages("standardize")
library(standardize)
Dat$scaledsitelev <- scale(Dat$SiteElevation.x)
###Dat$scaledCanopy <- scale(Dat$CanopyNum)

## missing CI's but good model graph 
mod2 <- glmer(Prop_rmv ~ scaledsitelev*Canopy + (scaledsitelev|Species) + (1|Date2) + (1|Transect/Site), family="binomial", control= glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000)), data=Dat)

visreg(mod2, xvar="scaledsitelev", by="Canopy", overlay=T, scale ="response")
summary(mod2)

### mod 2 through gg instead of visreg 

#install.packages("ggeffects")
#install.packages("effects")
library(ggeffects)

## Scaled elevation x prop rem. x canopy graph 
ggtest <- ggeffect(mod2,terms=c("scaledsitelev[all]","Canopy")) 
plot(ggtest, rawdata = FALSE) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line=element_line(colour = "green")) + xlab("Scaled Site Elevation") + ylab("Proportion Removed")


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




