# Check Working directory
setwd('..')
getwd() # The WD have to be "practical_session_2"

# Load libraries and functions
source("R/supporting_code/global_functions_light.R")
library(sf)
library(lubridate)
library(tidyverse)
wgs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
options(warn=-1)
# Load base grid ####
grid=read_sf("maps/grid")
st_crs(grid)=wgs

# Set parameters
activate_filter='Y'
ref_years=2015:2018

# Create file to store loop data
fishing_data=list()

# Loop over years
begin=Sys.time()
for(j in 1:length(ref_years)){
  
  # information on the run
  jini=Sys.time()
  cat(j) 
  
  j_yr=ref_years[j]
  
  # Read data and prepare it for next steps
  f_tracks=read_sf(file.path("data",j_yr,paste0("f_segments_sub_", j_yr, ".shp")))
  if(nrow(f_tracks)==0){next}
  f_tracks$s_time=as_datetime(f_tracks$s_time, tz='UTC')
  f_tracks$f_time=as_datetime(f_tracks$f_time, tz='UTC')
  f_tracks=split(f_tracks, f_tracks$MMSI)
  
  # Apply function to estimate fishing activity
  f_tracks_grid=suppressMessages(estimate_fishing_effort(f_tracks, grid))
  
  # Format dataset
  f_tracks_grid=plyr::ldply(f_tracks_grid,.id = NULL)
  f_tracks_grid=f_tracks_grid[ , -which(names(f_tracks_grid) %in% c("grid_id", 'geometry'))]  ## remove not useful information
  f_tracks_grid$month=lubridate::month(f_tracks_grid$s_time)
  f_tracks_grid$year=lubridate::year(f_tracks_grid$s_time)
  
  # Activate to remove outlier cells
  if(activate_filter=="Y"){
    print("Filtering activated")
    outlier_filter=aggregate(x=f_tracks_grid$f_hours, by=list('G_ID'=f_tracks_grid$G_ID,'year'=f_tracks_grid$year ), FUN=sum)
    outlier_filter=outlier_filter[outlier_filter$x>=0.33,c('G_ID','year')]
    f_tracks_grid=merge(outlier_filter, f_tracks_grid, by=c("G_ID", "year"), all.x=T)
  }
  
  # Store data
  fishing_data[[j]]=f_tracks_grid
  rm(f_tracks_grid)
  
  # Display time elapsed
  jfin=Sys.time()
  print(paste('Year ',j_yr, 'succesfully processed in'   ,difftime(jfin, jini, units='mins') ) )
}

# Display time elapsed
end=Sys.time()
difftime(end, begin, units='mins')  

# Format data and save
fishing_data=plyr::ldply(fishing_data)
write.csv(fishing_data, "data/fishing_data.csv", row.names = F)



