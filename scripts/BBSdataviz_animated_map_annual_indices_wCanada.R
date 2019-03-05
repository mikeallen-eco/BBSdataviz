### This code is a function for generating animated maps of bird population time-series from the North American Breeding Bird Survey (1966-2015 state-level indices published by USGS)
### Species and years can be customized by changing 'common_name', 'yearstart', AND 'yearend' below
### Code written by Mike Allen. Much of the animation code repurposed from resources listed below.
### Credit USGS database and citizen scientist data collectors if you use these maps

# To do in the future: 
## 1) address problem with names database (use official BBS database instead? ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/SpeciesList.txt or similar)
## 2) Add dynamic text that shows % decline/increase survey-wide
## 3) figure out how to adjust resolution and add as function argument
## NOTE: something is strange in the names database (a code matching problem):
        # Eastern Whip-poor-will mistakenly gets labeled Puerto Rican Nightjar
        # so this species need to be run and labeled manually. Other species?

### LOAD THE REQUIRED PACKAGES. INSTALL THEM FIRST IF NEEDED; e.g., install.packages("ggplot2")
# note: that doesn't currently work for gganimate. Not sure the best way to install it, but this worked:
# devtools::install_github('thomasp85/gganimate/releases/tag/v0.1.1', force = T)
library(gganimate)
library(gifski)
library(ggplot2)
library(rgdal)
library(sp)
library(maps)
library(ggthemes)
library(dplyr)
library(maps)
#library(raster)


### READ IN AND PROCESS DATA

# census shapefiles for US and Canada states/provinces/territories
states <- readOGR("data/cb_2014_us_state_5m.shp") # downloaded from: https://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_state_5m.zip
canada = readOGR("data/gpr_000b11a_e.shp") # downloaded from Canadian census website
can = readOGR("data/province.shp") # retrieved from NOAA: https://www.weather.gov/source/gis/Shapefiles/Misc/province.zip
#can=subset(can,NAME %in% c("CA01","CA11","CA03","CA02","CA09","CA04","CA07","CA08"))
states.latlong = spTransform(states,CRS("+proj=longlat +datum=WGS84"))
states.coords = 
  cbind.data.frame(states.latlong@data,coordinates(states.latlong)) %>%
  rename(state = STUSPS, state.fips = STATEFP, state.name = NAME, lon = "1", lat = "2") %>%
  dplyr::select(state,state.fips,state.name,lon,lat) %>%
  mutate(country = "USA") 

canada.fort = ggplot2::fortify(can) %>%
  filter(lat <60.0001)

can.prov=data.frame(can.prov = c("BC","QC","X","PE","SK","X","MB","ON","NB","X","AB","X","NS"))
canada.coords = coordinates(canada) %>%
  cbind(can.prov,canada@data) %>%
  dplyr::select(state=can.prov, state.fips = PRUID, state.name = PRENAME, lon = 1, lat = 2) %>%
  mutate(country = "CAN") %>%
  filter(state != "X")

na.coords = rbind.data.frame(states.coords, canada.coords)

# State-level Breeding Bird Survey data (annual state-level population index)
# Download file from here: https://www.mbr-pwrc.usgs.gov/bbs/BBS_Annual_Indices_Estimates_2015_7-29-2016.csv
bbs = read.csv("data/BBS_Annual_Indices_Estimates_2015_7-29-2016.csv") # read in BBS data; takes a while: ~ 850,000 records
colnames(bbs)<-c("spcode","state","year","index","cred") # rename columns
bbs$spcode = as.character(bbs$spcode) # makes spcode a character vector
#rename region codes to match state abbreviations
levels(bbs$state)=c("AL","AB","AZ","AR","BC","CA","X","X","CO","CT", "DE", "X", "FL", "GA", "IA",
                    "ID", "IL", "IN", "KS", "KY", "LA", "MB", "MA", "MD", "ME", "MI", "MN", "MS", 
                    "MO", "MT", "NB", "NC", "ND", "NE", "NV", "NH", "NJ", "NM", "NS", "NY", "OH",
                    "OK", "ON", "OR", "PA", "PE", "QC", "RI", "X", "X", "X", "X", "X", "X", "X",
                    "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
                    "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "SK", "SC", "SD", "SUR", "TN", 
                    "TX", "X", "UT", "VA", "VT", "WA", "X", "WI", "WV", "WY")

# AOS bird names and codes from: https://www.pwrc.usgs.gov/bbl/manual/speclist.cfm 
# should look them up here or somewhere else on BBS site: ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/SpeciesList.txt
birdnames = read.csv("data/AOS_bird_names.csv") %>%
  mutate(spcode = as.character(formatC(round(Species.Number/10000,4), format='f', digits=4))) %>%
  mutate(spcode = substring(spcode,3,6)) %>%
  mutate(spcode = as.character(paste("s0",spcode,sep=""))) %>%
  mutate(Common.Name = as.character(Common.Name)) %>%
  right_join(data.frame(spcode = as.character(unique(bbs$spcode))),by="spcode") %>%
  filter(is.na(Common.Name)==F)


################################
### function to produce animated gif of state-level bbs index data
################################

bbsanimate = function(common_name,yearstart,yearend) {

### use this code if you want to run the code without running it as a function (e.g., to fix Eastern Whip-poor-will)
# common_name = "Puerto Rican Nightjar"
# yearstart = 1966
# yearend = 2015

  
# filtering by species and years; excluding Canada and regional estimates; creating annual % change variable (r)
bird =
  bbs  %>%
  left_join(birdnames,by = "spcode") %>%
  dplyr::select(species = Common.Name,alpha.code = Alpha.Code, state,year,index) %>%
  dplyr::filter(species==common_name & state!= "X" & state != "SUR" & year > yearstart - 1 & year < yearend + 1) %>%
  arrange(state,year) %>%
  mutate(r = ifelse(year==yearstart,NA,100*c(NA,diff(index))/lag(index)))
bird=droplevels(bird)
  
bird.spatial = merge(bird,na.coords, by="state",all.x=T)

# define NA map
na <- ggplot() +
  #borders("world", xlim = c(-150, -60), ylim = c(30, 40), colour = "gray85", fill = "gray80")  +
  borders("state", colour = "gray85", fill = "gray80") +
  geom_polygon(data=canada.fort, aes(x = long, y = lat, group=group), colour = "gray85", fill = "gray80") +
  theme_map() + 
  coord_map('albers', lat0=30, lat1=60)

#x11(20,10)
map <- na +
  geom_point(aes(x = lon, y = lat, size = index),
             data = bird.spatial, 
             colour = 'darkred', alpha = .5) + 
  scale_size_continuous(range = c(1, 15), 
                        breaks = c(50, 100, 150, 200)) + 
  labs(title = '{common_name}: {frame_time}') + 
  theme(plot.title = element_text(size=25)) +
  # you may need to edit 'size' depending on the abundance of the species
  transition_time(year) +
  ease_aes('linear') +
  theme(legend.position = "none") 
  
gganimate::animate(map)
anim_save(paste("figures/animated_BBS_us/",common_name,".BBSindex_states_",yearstart,"-",yearend,".gif",sep=""))
}

####
#### enter the species common name (in quotes) into the function below
#### plus the desired start and end year
#### function will save to file path in line 127
####

bbsanimate("Greater Yellowlegs",1966,2015)


####
#### use the code below to make and save gifs for multiple (even all!) species
####

for (index in 181:205){
  bbsanimate(birdnames[index,"Common.Name"],1966,2015)
}  

# Resources used:
# https://gganimate.com/
# https://d4tagirl.com/2017/05/how-to-plot-animated-maps-with-gganimate
# https://www.rdocumentation.org/packages/gganimate/versions/0.1.1
# https://stackoverflow.com/questions/32302836/ggplot-in-r-with-fortify-takes-too-long-to-process-small-geospatial-data
