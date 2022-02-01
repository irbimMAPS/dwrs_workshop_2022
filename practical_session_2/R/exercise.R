setwd("C:/AIS/AIS/R/GFCM_DWF/workshop/github/gfcm_dwrs-workshop_2022-")
library(tidyverse)
# Exercise: form fishing point to indicators

sample_dat=readRDS("C:/AIS/AIS/R/GFCM_DWF/workshop/github/gfcm_dwrs-workshop_2022-/lecture2/exercise/sample_vessel_gsa23_pp.rData")

ggplot()+
  geom_sf(data = st_as_sf(sample_dat, coords =c('longitude','latitude'))$geometry)




### Q 1: extract fishing segments from sample data

# Tip: the process to load function etc. is provided: run from here to X
dirmaps="lecture2/workflow/maps" # path of the maps directory
file_parameters="lecture2/workflow/data/parameters.csv" # import parameter table
file_centroids="lecture2/workflow/data/centroids.csv" # import centroids 
outdir="lecture1/results"
wgs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # Insert your coordinates system
install.missing.packages=T # set to TRUE if want to allow automatic installation of missing packages.
source("lecture2/workflow/R/global_functions.R") # load all internal function required
ports<-read_sf(file.path(dirmaps, "med_harb_gsa")) # import list of ports
port_buf<-st_buffer(ports, 0.001) # create a buffer
st_crs(port_buf)=wgs # set crs
coastal_ban_zone=read_sf(file.path(dirmaps, "coastal_ban_zone")) # import managment depth layer
st_crs(coastal_ban_zone)=wgs # set crs
grid<-read_sf(file.path(dirmaps, "grid01degrees")) # import a grid
grid$grid_id=seq(1:nrow(grid)) # create cell grid id
st_crs(grid)=wgs # set crs
centroids=inport_parameters(file.path(file_parameters), file.path(file_centroids))[[2]]
pars=inport_parameters(file.path(file_parameters), file.path(file_centroids))[[1]]


vessels=unique(sample_dat$MMSI)
all_fishing_tracks=list()
for(i in 1:length(vessels)){
  cat("\n", "vessel", i, "of", length(vessels))
  cat("\n")
  xvessel=sample_dat[which(sample_dat$MMSI == vessels[i]),]
  fishing_tracks=classification_workflow(data=xvessel,
                                         ports=ports, 
                                         ports_buffer=port_buf,
                                         coastal_ban_zone=coastal_ban_zone,
                                         pars=pars,
                                         coord_sys=wgs,
                                         output.type="tracks",
                                         write.output=F)
  all_fishing_tracks[[i]]=fishing_tracks
  names(all_fishing_tracks)[i]=vessels[i]
}


fishing_data=plyr::ldply(all_fishing_tracks)

