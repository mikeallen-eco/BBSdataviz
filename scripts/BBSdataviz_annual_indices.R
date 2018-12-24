### This code is for creating geo-facet time-series plots of North American Breeding Bird Survey state-level data
### Two types of plots: population index values and annual growth rate (year-over-year % change)
### Species and years can be customized by changing 'species', 'yearstart', AND 'yearend' below
### Info: currently only set up for state-level data excluding Canada; ultimate goal is to make part of a package
        # that also includes other BBS dataviz outputs (as functions), including at the route level.

### LOAD THE REQUIRED PACKAGES. INSTALL THEM FIRST IF NEEDED; e.g., install.packages("ggplot2")
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

# filtering by species and years; excluding Canada and regional estimates; creating annual % change variable (r)
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

# creating geofacet plot of all state-level time series: BBS indices (same thing, but y-axes fixed)
#X11(26,18)
ggplot(bird) + 
  geom_line(aes(x=year,y=index)) +
  facet_geo(~state) +
  theme_bw() +
  xlab("Year") + ylab("BBS Index") +
  theme(axis.text=element_text(size=3.25))
#ggsave("figures/BBSindex_all_states_1966-2015_yfixed.png")

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