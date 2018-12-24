### This code is for creating animated maps of bird population time-series from the North American Breeding Bird Survey
### Species and years can be customized by changing 'species', 'yearstart', AND 'yearend' below
### Info: currently only set up for state-level data excluding Canada; ultimate goal is to make part of a package
        # that also includes other BBS dataviz outputs (as functions), including at the route level.

### LOAD THE REQUIRED PACKAGES. INSTALL THEM FIRST IF NEEDED; e.g., install.packages("ggplot2")
#devtools::install_github('thomasp85/gganimate/releases/tag/v0.1.1', force = T)
library(gganimate)
library(gifski)
library(ggplot2)
#library(utils)
library(rgdal)
library(sp)
library(maps)
library(ggthemes)
#library(rgeos)
#library(maptools)
#library(mapmisc)
#library(sgeostat)
library(dplyr)
#library(RColorBrewer)

### SET SPECIES AND YEAR RANGE
species = "s05460" # this is the AOU species code which you can look up here: https://www.pwrc.usgs.gov/bbl/manual/speclist.cfm
# need to preface the code with "s0"; e.g., Grasshopper Sparrow = "s05460"
# entually will add a lookup table to allow the use of bird names
yearstart = 1966
yearend = 2015

### READ IN AND PROCESS DATA

# census shapefile for US states and territories
states <- readOGR("data/cb_2014_us_state_5m.shp")
# downloaded from: https://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_state_5m.zip
states.latlong = spTransform(states,CRS("+proj=longlat +datum=WGS84"))
states.coords = 
  cbind.data.frame(states.latlong@data,coordinates(states.latlong)) %>%
  select(state = STUSPS, state.fips = STATEFP, state.name = NAME, lon = "1", lat = "2")

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


bird.spatial = merge(bird,states.coords, by="state",all.x=T)

##### MAPPING BBS INDEX BY STATE

us <- ggplot() +
  borders("state", colour = "gray85", fill = "gray80") +
  theme_map() + 
  coord_map('albers', lat0=30, lat1=60)

#x11(20,10)
map <- us +
  geom_point(aes(x = lon, y = lat, size = index),
             data = bird.spatial, 
             colour = 'darkred', alpha = .5) + 
  scale_size_continuous(range = c(1, 20), 
                        breaks = c(50, 100, 150, 200)) +
  labs(size = 'BBS Index') +
  labs(title = 'Grasshopper Sparrow, Year: {frame_time}') + 
  theme(plot.title = element_text(size=25)) +
  transition_time(year) +
  ease_aes('linear') + 
  theme(legend.background = element_rect(fill="transparent")) +
  theme(legend.position = "none") # or "right", but it makes the map smaller

animate(map)
anim_save("figures/BBSindex_all_states_1966-2015.gif")

# Resources used:
# https://gganimate.com/
# https://d4tagirl.com/2017/05/how-to-plot-animated-maps-with-gganimate
# https://www.rdocumentation.org/packages/gganimate/versions/0.1.1
