# This code is based on the workflow adopted to elaborate Automatic Identification System (AIS) data for the report "Identification and mapping of bottom trawl fishing grounds for deep-water red shrimp in the Eastern-Central Mediterranean Sea (GSAs 12-16, 18-27)", requested by the General Fisheries Commitee for the Mediterranean Sea (GFCM). 
# ---
# title: "Aggregate fishing effort grid to produce fishing pressure metrics"
# authors: "Enrico Nicola Armelloni, Jacopo Pulcinella"
# date: "February 1st, 2022"
# ---
# contact address: maps.irbim@irbim.cnr.it

# Check Working directory
getwd() # The WD have to be "practical_session_2"
setwd('practical_session_2/') # 
getwd() # check again wd

# Load libraries and functions
source("R/supporting_code/global_functions_light.R")
library(tidyverse)
library(sf)
library(ggspatial)
library(ggnewscale)

# Load data ####
# Fishing data
fishing_data= read_csv("data/fishing_data.csv")
# Optional: retain only vessels that visited DW at least once in the month
vessel_selection=unique(fishing_data[fishing_data$DW==1,c('MMSI','month','year')])
fishing_data=merge(vessel_selection, fishing_data, by=c("MMSI", "year", "month"), all.x = T)

rm(vessel_selection)
# Shapefiles 
grid <- read_sf("maps/grid")

land=read_sf("maps/land")%>% 
  st_crop(., xmin=-10, xmax=43, ymin=20, ymax=48)

gsa=read_sf("maps/gsa")%>%
  dplyr::filter(SMU_CODE == '23' )

dep = read_sf("maps/depth_contours")%>%
  st_cast(., to = "POLYGON")%>%
  st_set_crs(st_crs(gsa))%>%
  st_buffer(0)%>%
  st_crop(gsa)

lims=c(st_bbox(gsa[c(1,3)], st_bbox(gsa[c(2,4)])))

text_size=8
plot_height=25
plot_width=18
source("R/supporting_code/plot_specifications.R")

defo_map # Show the default theme of the maps

#  Map  #### 
## Calculations ####
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

ggsave(plot=plot_effort_map, 
       "results/effort_map.png",
       width = 20, height = 20, units='cm', dpi=500)

# Easygoing plot format
defo_map+
  geom_sf(data = grid_fishing, aes(fill = f_hours_log), colour = NA, alpha = 0.8, show.legend = "point" )+
  scale_fill_gradient("Log Fishing effort (Log(h)/km2)",low = "yellow", high = "firebrick") +
  coord_sf(xlim=c(lims[[1]], lims[[3]]), ylim=c(lims[[2]], lims[[4]]) , 
           label_axes = list(bottom = "E", right = "N")) +
  xlab("") 

rm(plot_effort_map, grid_DW, grid_SW, grid_fishing, gsa, dep, land, defo_map)

#  Time-series  ####
## Calculations ####
### Step 1: compute monthly data ####
monthly_effort=aggregate(list(nhours=fishing_data$f_hours), 
          by=list('year'= fishing_data$year, 'month'= fishing_data$month ,'DW'=fishing_data$DW), FUN=sum)

monthly_effort$temporal_ref=as.Date(paste(monthly_effort$year, monthly_effort$month, "01", sep = "-"))

vessel_by_month=unique(fishing_data[,c('MMSI','month','year')])
vessel_by_month=aggregate(vessel_by_month$MMSI, 
                          by=list('year'=vessel_by_month$year , 'month'=vessel_by_month$month), FUN=length)
vessel_by_month$temporal_ref=as.Date(paste(vessel_by_month$year, vessel_by_month$month, "01", sep = "-"))

### Step 2:  expand to full ts ####
index=data.frame(order=seq(1, 48, 1), 
           temporal_ref=seq(as.Date(paste("2015", "01", "01", sep = "-")),
                   as.Date(paste("2018", "12", "01", sep = "-")),"month"))

monthly_effort=merge(index, monthly_effort, by="temporal_ref", all.x = T)
monthly_effort[!is.na(monthly_effort$DW) & monthly_effort$DW==1,]$DW='DW'
monthly_effort[!is.na(monthly_effort$DW) & monthly_effort$DW==0,]$DW='SW'

vessel_by_month=merge(index, vessel_by_month, by="temporal_ref", all.x = T)

## Specifications for the plot ####
### Customize labeling ####
plot_breaks=seq(min(monthly_effort$order),max(monthly_effort$order),3)
plot_labels=paste(substr(seq(min(monthly_effort$temporal_ref), 
                             max(monthly_effort$temporal_ref),"quarters"),1,4), 
                  month.abb[as.numeric(substr(seq(min(monthly_effort$temporal_ref),max(monthly_effort$temporal_ref),"quarters"),6,7))], sep="-")
## Plot it ####
# Plot format of the report
plot_effort_ts=defo_ts +
  geom_bar(data = monthly_effort, aes(x=order,y= nhours, fill = reorder(DW, desc(DW))), 
           stat = "identity", colour = 1, size=0.2) +
  scale_fill_manual(values = c("deepskyblue3", "tomato3")) + 
  xlab("")  + 
  scale_x_reverse(breaks=plot_breaks,labels=plot_labels)+
  guides(fill = guide_legend(title = "Fishing ground")) +
  ylab("Hours") +
  geom_line(data=vessel_by_month, aes(x=order, y = x*50), size = 0.7) +
  scale_y_continuous(sec.axis = sec_axis(~ . /50, name=expression(paste(""[""["No. DW trawlers"]], paste(bold(" __"))))), name=expression( ""[""["F hours"]])) ; plot_effort_ts

ggsave(plot=plot_effort_ts, 
       "results/effort_TS.png",
       width = 20, height = 20, units='cm', dpi=500)

# Easygoing plots

defo_ts +
  geom_bar(data = monthly_effort, aes(x=order,y= nhours, fill = reorder(DW, desc(DW))), 
           stat = "identity", colour = 1, size=0.2) +
  scale_fill_manual(values = c("deepskyblue3", "tomato3")) + 
  xlab("")  + 
  scale_x_reverse(breaks=plot_breaks,labels=plot_labels)+
  guides(fill = guide_legend(title = "Fishing ground")) +
  ylab("Hours") # Only monthly effort


ggplot() +
  geom_col(data=vessel_by_month, aes(x=order, y = x), size = 0.7)+
  scale_x_continuous(breaks=seq(min(vessel_by_month$order), max(vessel_by_month$order)) , 
                     labels = vessel_by_month$temporal_ref)+
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90)) +
  guides(colour = guide_legend(title = "Vessel", override.aes = list(size = 1)))

