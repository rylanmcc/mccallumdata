#install.packages("tidyverse")
library("tidyverse")

### read in raw data file of seed predation and make new columns for proportion removed
Dat <- read_csv("SeedPredationDataForR2019.csv") %>% 
  mutate(N_start = if_else(Species=="sun", 5, ifelse(Species=="oats", 8, NA)),
         Prop_rmv = 1-N_end/N_start)


### make site names match those of GPS waypoints
Dat <- Dat %>% 
  mutate(Site2 = ifelse(Site=="RP-1B"&Canopy=="open", "RP-1B-6", 
                        ifelse(Site=="RP-1B"&Canopy=="closed", "RP-1B-5",
                               ifelse(Site=="RP-2"&Canopy=="open", "RP-2-6",
                                      ifelse(Site=="RP-2"&Canopy=="closed", "RP-2-5",
                                             ifelse(Site=="RP-3"&Canopy=="open", "RP-3-2",
                                                    ifelse(Site=="RP-3"&Canopy=="closed", "RP-3-1",
                                                           ifelse(Site=="RP-4"&Canopy=="open", "RP-4-6",
                                                                  ifelse(Site=="RP-4"&Canopy=="closed", "RP-4-5",
                                                                         ifelse(Site=="RP-5"&Canopy=="open", "RP-5-6",
                                                                                ifelse(Site=="RP-5"&Canopy=="closed", "RP-5-5",
                                                                                       ifelse(Site=="RP-6"&Canopy=="open", "RP-6-6",
                                                                                              ifelse(Site=="RP-6"&Canopy=="closed", "RP-6-5",
                                                                                                     ifelse(Site=="RP-7"&Canopy=="open", "RP-7-6",
                                                                                                            ifelse(Site=="RP-7"&Canopy=="closed", "RP-7-5",
                                                                                                                   ifelse(Site=="RP-8"&Canopy=="open", "RP-8-2",
                                                                                                                          ifelse(Site=="RP-8"&Canopy=="closed", "RP-8-1",
                                                                                                                                 ifelse(Site=="RP-9"&Canopy=="open", "RP-9-6",
                                                                                                                                        ifelse(Site=="RP-9"&Canopy=="closed", "RP-9-5",
                                                                                                                                               ifelse(Site=="RP-11"&Canopy=="open", "RP-11-2",
                                                                                                                                                      ifelse(Site=="RP-11"&Canopy=="closed", "RP-11-1",
                                                                                                                                                             ifelse(Site=="RP-15"&Canopy=="open", "RP-15-2",
                                                                                                                                                                    ifelse(Site=="RP-15"&Canopy=="closed", "RP-15-1",
                                                                                                                                                                           ifelse(Site=="MB-1"&Canopy=="open", "MB-1-2",
                                                                                                                                                                              ifelse(Site=="MB-1"&Canopy=="closed", "MB-1-1",
                                                                                                                                                                                     ifelse(Site=="MB-2"&Canopy=="open", "MB-2-2",
                                                                                                                                                                                            ifelse(Site=="MB-2"&Canopy=="closed", "MB-2-1",
                                                                                                                                                                                                   ifelse(Site=="MB-3"&Canopy=="open", "MB-3-2",
                                                                                                                                                                                                          ifelse(Site=="MB-3"&Canopy=="closed", "MB-3-1",
                                                                                                                                                                                                                 ifelse(Site=="MB-4"&Canopy=="open", "MB-4-2",
                                                                                                                                                                                                                        ifelse(Site=="MB-4"&Canopy=="closed", "MB-4-1",
                                                                                                                                                                                                                               ifelse(Site=="MB-5"&Canopy=="open", "MB-5-2",
                                                                                                                                                                                                                                      ifelse(Site=="MB-5"&Canopy=="closed", "MB-5-1", NA)))))))))))))))))))))))))))))))))

### read in GPS waypoints with lat, long, and elevation of each plot
locs <- read_csv("SeedAdditionTransectGPSPoints2017.csv")

### merge data with locations
dat <- left_join(Dat, locs, by=c("Site2"="SiteName"))



#### converting Date to julian days 
dat$Date2 <- format(dat$Date, "%j")


### write new csv for next script
write_csv(dat, "SeedPredationDataForAnalysis.csv")




