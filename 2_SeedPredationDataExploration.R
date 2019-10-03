### install if necessary, and open tidyverse library
#install.packages("tidyverse")
library("tidyverse")

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

### Making a new data frame to test if i can merge my two existing data frames together 

TestMergeData <- Dat

MergedDataFrame <- merge(TestMergeData, LatLongElv_2017)

cbind(LatLongElv_2017$`Site Name`, TestMergeData)

TestMergeData$GivenSiteName <- ifelse((TestMergeData$Site = RP-1B) & (TestMergeData$Canopy = open), LatLongElv_2017$`Site Name`







