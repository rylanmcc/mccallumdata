### install if necessary, and open tidyverse library
#install.packages("tidyverse")
library("tidyverse")

### read in raw data file and make new columns for proportion removed
Dat <- read_csv("SeedPredationDataForR2019.csv") %>% 
  mutate(N_start = if_else(Species=="sun", 5, ifelse(Species=="oats", 8, NA)),
         Prop_rmv = 1-N_end/N_start)

### take a look at distributions of removal rates
ggplot(Dat, aes(Prop_rmv)) + geom_histogram()
ggplot(Dat, aes(Prop_rmv, fill=Species)) + geom_histogram()
ggplot(Dat, aes(Prop_rmv, fill=Canopy)) + geom_histogram()
