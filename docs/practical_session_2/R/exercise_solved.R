# Exercise pt 1: starting from the raw fishing data of 5 vessels operating in GSA 23 apply the R4ais workflow to obtain fishing segments, and then analyse the data to obtain a map of the fishing effort. The exercise is divided into 4 questions which helps you to get the desired result. All the code you need is part of the script seen today. 

# Exercise pt2 (advanced level): you are provided with all the information regarding the fishing trips 5 vessels (nationality, harbour of arrival etc..) and you are asked to deploy your imagination. More info on Q5.


# Before start: Check Working directory ####
getwd() # The WD have to be "dwrs_workshop_2022"
#setwd('..') # 
getwd() # check again wd


# Load Sample Data ####
# Run to load the data needed for the exercise
sample_data=readRDS("practical_session_2/data/exercise_data.rData")

# The steps to load all the functions of the R4ais is a bit cumbersome. To load the functions needed you need just to run the following lines of code
base_dir=getwd()
setwd('practical_session_1')
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

sample_dat_Q1=sample_data[sample_data$MMSI==unique(sample_data$MMSI)[1],] # This may be used as input data

fishing_tracks=classification_workflow(data=sample_dat_Q1,
                                       ports=ports, 
                                       ports_buffer=port_buf,
                                       coastal_ban_zone=coastal_ban_zone,
                                       pars=pars,
                                       coord_sys=wgs,
                                       output.type="tracks",
                                       write.output = F)


### Q 2 ####
# Extract fishing tracks from all the vessels in the sample data at once: you need to identify the right function from the workflow. # Tip: the output of the function is a list of lists, so you need to process the output as explained. 
all_dat=sample_data
all_dat=all_dat[,c("MMSI", "datetime", "longitude", "latitude", "speed")]
all_dat$MMSI=as.character(all_dat$MMSI)
vessels=unique(all_dat$MMSI)
all_fishing_tracks=list()
for(i in 1:length(vessels)){
  cat("\n", "vessel", i, "of", length(vessels))
  cat("\n")
  xvessel=all_dat[which(all_dat$MMSI == vessels[i]),]
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
  f_tracks=all_fishing_tracks[all_fishing_tracks$year==j_yr,] #### 
  if(nrow(f_tracks)==0){next}
  f_tracks$s_time=as_datetime(f_tracks$s_time, tz='UTC')
  f_tracks$f_time=as_datetime(f_tracks$f_time, tz='UTC')
  f_tracks=split(f_tracks, f_tracks$MMSI)
  
  # Apply function to estimate fishing activity
  f_tracks_grid=suppressMessages(estimate_fishing_effort_DWRS(f_tracks, grid))
  
  # Format dataset
  f_tracks_grid=plyr::ldply(f_tracks_grid,.id = NULL)
  f_tracks_grid=f_tracks_grid[ , -which(names(f_tracks_grid) %in% c("grid_id", 'geometry'))]  ## remove not useful information
  f_tracks_grid$month=lubridate::month(f_tracks_grid$s_time)
  f_tracks_grid$year=lubridate::year(f_tracks_grid$s_time)
  
  # Activate to remove outlier cells
  if(activate_filter=="Y"){
    print("Filtering activated")
    outlier_filter=aggregate(x=f_tracks_grid$f_hours, 
                             by=list('G_ID'=f_tracks_grid$G_ID,'year'=f_tracks_grid$year ), FUN=sum)
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

# Format data 
fishing_data=plyr::ldply(fishing_data)


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


### Step 1: aggregate data to get mean annual fishing intensity by grid cell ####
cumulative_hours_by_year=aggregate(list(f_hours_year=fishing_data$f_hours), 
                                   by=list('G_ID'= fishing_data$G_ID ,'year'=fishing_data$year), FUN=sum)

mean_hours_over_year=aggregate(list(f_hours_overall=cumulative_hours_by_year$f_hours_year), 
                               by=list('G_ID'=cumulative_hours_by_year$G_ID),FUN=mean)

### Step 2: paste information to the grid ####
grid_fishing=merge(grid, mean_hours_over_year, by='G_ID', all.x=F)
grid_fishing$f_hours_log=log(grid_fishing$f_hours_overall + 1)

rm(grid,cumulative_hours_by_year , mean_hours_over_year)

### Step 3: divide data basing on fishing position and normalize data ####
grid_DW=grid_fishing[grid_fishing$DW==1,]
grid_DW$f_hours_log_st=st_fun(grid_DW$f_hours_log)

grid_SW=grid_fishing[grid_fishing$DW==0,]
grid_SW$f_hours_log_st=st_fun(grid_SW$f_hours_log)

## Specifications for the plot ####
### Customize labelling ####
brks_DW = seq(0, 1, length.out = 10)[c(2,4,6,8,10)]
brks_DW_lab = round(exp((brks_DW*(max(grid_DW$f_hours_log) - min(grid_DW$f_hours_log))) + min(grid_DW$f_hours_log)))

brks_SW = seq(0, 1, length.out = 10)[c(2,4,6,8,10)]
brks_SW_lab = round(exp((brks_SW*(max(grid_SW$f_hours_log) - min(grid_SW$f_hours_log))) + min(grid_SW$f_hours_log)))

## Plot it! ####

# Plot format of the report
plot_effort_map = defo_map+
  geom_sf(data = grid_SW, aes(fill = f_hours_log_st), colour = NA, alpha = 0.8, show.legend = "point" ) +
  scale_fill_gradient(low = "lightskyblue1", high = "blue", 
                      breaks = brks_SW, labels = brks_SW_lab, guide = guide_colorbar(title = "SW (h/km2)")) +
  new_scale("fill") +  
  geom_sf(data = grid_DW, aes(fill = f_hours_log_st), colour = NA) +
  scale_fill_gradient("DW (h/km2)", breaks = brks_DW, labels = brks_DW_lab, 
                      low = "yellow", high = "firebrick") +
  coord_sf(xlim=c(lims[[1]], lims[[3]]), ylim=c(lims[[2]], lims[[4]]) , 
           label_axes = list(bottom = "E", right = "N")) +
  xlab("") ; plot_effort_map


# Q5 ####
# Looking at the fishing track data you may notice that there is poor information regarding the fishing trips and the fishing vessels, so our possibility of data exploration are limited. Here you are provided with the tools to get a bite of information related to the destination of the fishing trip and on the nationality of the fishing vessel. 
# Having this additional information you may create your own thematic map focusing on an aspect of your choice (for example effort by country, effort by port of destination etc..).

# Tip 1: fishing trips
#By applying the correct function (within those seen this morning) it is possible to get information on the harbour of arrival of each trip. Then, this information can be linked to the tracks.
all_fishing_trips=list()
for(i in 1:length(vessels)){
  cat("\n", "vessel", i, "of", length(vessels))
  cat("\n")
  xvessel=sample_data[which(sample_data$MMSI == vessels[i]),]
  dat_trip=create_fishing_trip(data=xvessel,
                               ports=ports, 
                               ports_buffer=port_buf,
                               coastal_ban_zone=coastal_ban_zone)
  all_fishing_trips[[i]]=dat_trip
}
all_fishing_trips=plyr::ldply(all_fishing_trips)

head(all_fishing_trips)


# The information of the ports contained in the object 'ports' (code provided). 
all_fishing_trips_ports=merge(all_fishing_trips, data.frame(ports)[,c('harbour', 'GSA')], by.x = 'arrival', by.y = 'harbour')
head(all_fishing_trips_ports)


# Nationality of the sample vessels: detailed vessel information is not provided for privacy reasons, however here we give the nationality of registration for the sample data.
vessel_country=data.frame(MMSI=as.character(2:9),
           country=c(rep('GRC',6), rep('ITA',2)),
           GSA_registration=c(rep('GSA23',6), rep('GSA16',2)))

# Combine all
fishing_data_plus=merge(fishing_data, all_fishing_trips_ports, by.x=c('MMSI','trip'))
head(fishing_data_plus)

fishing_data_plus=merge(fishing_data_plus,vessel_country,by.x='MMSI')
head(fishing_data_plus)

grid <- read_sf("practical_session_2/maps/grid")

# Plot I: number of vessels
fishing_aggregation=unique(fishing_data_plus[,c('G_ID','MMSI')])
fishing_aggregation=aggregate(fishing_aggregation$MMSI, by=list('G_ID'=fishing_aggregation$G_ID),FUN=length)
#
grid_fishing=merge(grid,fishing_aggregation,by.x='G_ID')
grid_fishing=grid_fishing[!is.na(grid_fishing$x),]

#
plot_numb_vessels=defo_map+
  geom_sf(data = grid_fishing, aes(fill = factor(x)), colour = NA) +
  coord_sf(xlim=c(lims[[1]], lims[[3]]), ylim=c(lims[[2]], lims[[4]]) , 
           label_axes = list(bottom = "E", right = "N")) +
  xlab("")+
  scale_fill_viridis_d()+
  labs(fill='Number of vessels');plot_numb_vessels

# Plot II: number of countries
fishing_aggregation=unique(fishing_data_plus[,c('G_ID','GSA_registration')])
fishing_aggregation=aggregate(fishing_aggregation$GSA_registration, by=list('G_ID'=fishing_aggregation$G_ID),FUN=length)
#
grid_fishing=merge(grid,fishing_aggregation,by.x='G_ID')
grid_fishing=grid_fishing[!is.na(grid_fishing$x),]

#
plot_numb_countries=defo_map+
  geom_sf(data = grid_fishing, aes(fill = factor(x)), colour = NA) +
  coord_sf(xlim=c(lims[[1]], lims[[3]]), ylim=c(lims[[2]], lims[[4]]) , 
           label_axes = list(bottom = "E", right = "N")) +
  xlab("")+
  scale_fill_viridis_d()+
  labs(fill='Number of Countries');plot_numb_countries

# Plot I: by country
fishing_aggregation=aggregate(fishing_data_plus$f_hours, by=list('G_ID'=fishing_data_plus$G_ID, 'GSA_reg'=fishing_data_plus$country),FUN=sum)

#
grid_fishing=merge(grid,fishing_aggregation,by.x='G_ID')
grid_fishing=grid_fishing[!is.na(grid_fishing$x),]

#
plot_by_countries=defo_map+
  geom_sf(data = grid_fishing, aes(fill = GSA_reg), colour = NA) +
  coord_sf(xlim=c(lims[[1]], lims[[3]]), ylim=c(lims[[2]], lims[[4]]) , 
           label_axes = list(bottom = "E", right = "N")) +
  xlab("")+
  facet_wrap(~GSA_reg)+
  scale_fill_viridis_d()+
  labs(fill='Country');plot_by_countries



