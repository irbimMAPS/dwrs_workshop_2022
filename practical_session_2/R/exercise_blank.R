# Exercise: starting from the raw fishing data of 5 vessels operating in GSA 23 apply the R4ais workflow to obtain fishing segments, and then analyse the data to obtain a map of the fishing effort. The exercise is divided into 4 questions which helps you to get the desired result. All the code you need is part of the script seen today.


# Before start: Check Working directory ####
getwd() # The WD have to be "dwrs_workshop_2022"
setwd('..') # Push one to set the WD to a backward place
getwd() # check the WD: it have to be "practical_session_2. If is not yet right, push again on line 6 and check"


# Load Sample Data ####
# Run to load the data needed for the exercise
sample_data=readRDS("practical_session_2/data/exercise_data.rData")

# The steps to load all the functions of the R4ais is a bit cumbersome. To load the functions needed you need just to run the following lines of code
base_dir=getwd()
setwd(file.path(base_dir, 'practical_session_1'))
dirmaps="AIS_data_processing/maps" # path of the maps directory
file_parameters="AIS_data_processing/data/parameters.csv" # import parameter table
file_centroids="AIS_data_processing/data/centroids.csv" # import centroids 
wgs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # Insert your coordinates system
install.missing.packages=T # set to TRUE if want to allow automatic installation of missing packages.
source("AIS_data_processing/R/global_functions.R") # load all internal function required
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
setwd(base_dir)


# Q1 ####
# Obtain the fishing segments from one single vessel (data provided): you need to identify the right function from the workflow.
# Tip: the output of the function is a list, so you need to process the output as x=plyr::ldply(x)

sample_dat_Q1=sample_data[sample_data$MMSI==unique(sample_data$MMSI)[1],] # This may be used as input data




### Q 2 ####
#extract fishing segments from all the vessels in the sample data at once: you need to identify the right function from the workflow. # Tip: the output of the function is a list of lists, so you need to process the output as explained. 

# post process 1
for(i in 1:length(all_fishing_tracks)){
all_fishing_tracks[[i]]=plyr::ldply(all_fishing_tracks[[i]])
}
# post process 2
all_fishing_tracks=plyr::ldply(all_fishing_tracks)


### Q3 ####
#intersect the obtained data with the grid of GSA 23. Tip: the piece of code you need is a for loop

source("practical_session_2/R/supporting_code/global_functions_light.R")
grid=read_sf("practical_session_2/maps/grid")
# Set parameters
activate_filter='Y'
ref_years=2015:2018


# Q4 ####
#create a map of the fishing effort

# Tip: load data
grid_whole <- read_sf("practical_session_2/maps/grid")
land=read_sf("practical_session_2/maps/land")%>% 
  st_crop(., xmin=-10, xmax=43, ymin=20, ymax=48) #
gsa<-read_sf("practical_session_2/maps/gsa")%>%
  dplyr::filter(SMU_CODE == '23' )
dep = read_sf("practical_session_2/maps/depth_contours")%>%
  st_cast(., to = "POLYGON")%>%
  st_set_crs(st_crs(gsa))%>%
  st_buffer(0)%>%
  st_crop(gsa)
lims=c(st_bbox(gsa[c(1,3)], st_bbox(gsa[c(2,4)])))
text_size=8
plot_height=25
plot_width=18
library(ggspatial)
library(ggnewscale)
source("practical_session_2/R/supporting_code/plot_specifications.R")





