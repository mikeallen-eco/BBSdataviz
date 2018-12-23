### THIS CODE IS FOR CREATING GEO-FACET TIME SERIES PLOTS OF NORTH AMERICAN BREEDING BIRD SURVEY STATE-LEVEL DATA
### PLOTS OF POPULATION INDEX VALUES OR ANNUAL GROWTH RATE (YEAR-OVER-YEAR % CHANGE)
### SPECIES OR YEARS CAN BE CUSTOMIZED BY CHANGING 'SPECIES', 'YEARSTART', AND 'YEAREND' BELOW
### STATE-LEVEL DATA EXCLUDING CANADA FOR NOW
### ULTIMATE GOAL: MAKE PART OF A PACKAGE THAT ALSO INCLUDES DATAVIS OUTPUTS, INCLUDING AT THE ROUTE-LEVEL

library(ggplot2)
library(dplyr)
library(geofacet)

### SET SPECIES AND YEAR RANGE
species = "s05460" # this is the AOU species code which you can look up here: https://www.pwrc.usgs.gov/bbl/manual/speclist.cfm
      # entually will add a lookup table
yearstart = 1966
yearend = 2015

### READ IN AND PROCESS DATA

# State-level Breeding Bird Survey data (annual state-level population index)
# Download file from here: https://www.mbr-pwrc.usgs.gov/bbs/BBS_Annual_Indices_Estimates_2015_7-29-2016.csv
bbs = read.csv("data/BBS_Annual_Indices_Estimates_2015_7-29-2016.csv") # read in BBS data; takes a while: ~ 850,000 records
colnames(bbs)<-c("sp","state","year","index","cred") # rename columns
#rename region codes to match state abbreviations
levels(bbs$state)=c("AL","CAN","AZ","AR","CAN","CA","X","X","CO","CT", "DE", "X", "FL", "GA", "IA",
                    "ID", "IL", "IN", "KS", "KY", "LA", "CAN", "MA", "MD", "ME", "MI", "MN", "MS", 
                    "MO", "MT", "CAN", "NC", "ND", "NE", "NV", "NH", "NJ", "NM", "CAN", "NY", "OH",
                    "OK", "CAN", "OR", "PA", "CAN", "CAN", "RI", "X", "X", "X", "X", "X", "X", "X",
                    "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
                    "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "CAN", "SC", "SD", "X", "TN", 
                    "TX", "X", "UT", "VA", "VT", "WA", "X", "WI", "WV", "WY")

# filtering by species, geography, and years and creating "population growth rate" (r) variable
bird =
  bbs %>%
  filter(sp==species & state!= "X" & state != "CAN" & year > yearstart - 1 & year < yearend + 1) %>%
  arrange(state,year) %>%
  mutate(r = ifelse(year==yearstart,NA,100*c(NA,diff(index))/lag(index)))
bird=droplevels(bird)

# creating geofacet plot of all state-level time series: BBS indices
#X11(26,18)
ggplot(bird) + 
  geom_line(aes(x=year,y=index)) +
  facet_geo(~state, scales = "free_y") +
  theme_bw() +
  xlab("Year") + ylab("BBS Index") +
  theme(axis.text=element_text(size=3.25))
#ggsave("figures/BBSindex_all_states_1966-2015.png")

# creating geofacet plot of all state-level time series: BBS index annual growth rate
#X11(26,18)
ggplot(bird) + 
  geom_line(aes(x=year,y=r)) +
  facet_geo(~state, scales = "free_y") +
  theme_bw() +
  xlab("Year") + ylab("BBS Index Annual Change (%)") +
  theme(axis.text=element_text(size=3.25)) +
  geom_smooth(aes(x=year,y=r),method = "loess", se = F, size=.5, color="red")
#ggsave("figures/BBSchange_all_states_1966-2015.png")