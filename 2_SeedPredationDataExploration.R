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




