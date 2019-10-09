### install if necessary, and open tidyverse library
#install.packages("tidyverse")
library("tidyverse")


### read in data file made from script 1
Dat <- read_csv("SeedPredationDataForAnalysis.csv")
  
#test for normality of data
#run linear models
#interactions?
#want temp and elevation as well
lm.1<-lm(Prop_rmv~Species+Canopy,data=Dat)
summary(lm.1)
anova(lm.1)

### read in raw data file and make new columns for proportion removed
Dat <- read_csv("SeedPredationDataForR2019.csv") %>% 
  mutate(N_start = if_else(Species=="sun", 5, ifelse(Species=="oats", 8, NA)),
         Prop_rmv = 1-N_end/N_start)


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

#test for normality of data
#run linear models
#interactions?
#want temp and elevation as well
lm.1<-lm(Prop_rmv~Species+Canopy,data=Dat)
summary(lm.1)
anova(lm.1)

### Trying to figure out how to link canopy to site names
### Link "closed" to **RP-3-1** and "open" to **RP-3-2** 

summary(canopy)

canopy <- Dat$Canopy
summary(canopy)

### saved the elevation, latitude, and longitude as an easier name to work with 

LatLongElv_2017 <- SeedAdditionTransectGPSPoints2017

### attempt to try and and create predation x elevation graphs 

Elevation <- SeedPredationDataForAnalysis$SiteElevation

ggplot(SeedPredationDataForAnalysis, aes(x=Elevation, y=Prop_rmv)) + geom_point()

ggplot(SeedPredationDataForAnalysis, aes(x=Elevation, y=Prop_rmv, fill = canopy)) + geom_point()

ggplot(SeedPredationDataForAnalysis, aes(x=Elevation, y=Prop_rmv, col=c("red", "blue")[canopy])) + geom_point()

ggplot(SeedPredationDataForAnalysis, aes(x=Elevation, y=Prop_rmv, by=canopy)) + geom_line()


### this ^^^ previous graph is messed up ... possibly due to the y axis being non - continuos but i am treating it as such  



####### graph predation x elevation but with the prop remov. as an average per site... tried to mutate data


Site2 <- SeedPredationDataForAnalysis$Site2
SeedPredationDataForAnalysis$Site3 <- as.factor(SeedPredationDataForAnalysis$Site2)

Site3 <- SeedPredationDataForAnalysis$Site3

prop_X_elev <- SeedPredationDataForAnalysis %>% 
  select(Site3,Prop_rmv,SiteElevation)
  group_by(Site3) %>% 
  mutate(Prop_rmv=mean(Prop_rmv)) %>% 
  select(-Prop_rmv) %>% 
  unique()
  


### bar graph does not work either..... 

ggplot(SeedPredationDataForAnalysis, aes(x=Elevation)) +
  geom_bar(fill = "forestgreen")











