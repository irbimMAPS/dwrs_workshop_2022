# This code is based on the workflow adopted to elaborate Automatic Identification System (AIS) data for the report "Identification and mapping of bottom trawl fishing grounds for deep-water red shrimp in the Eastern-Central Mediterranean Sea (GSAs 12-16, 18-27)", requested by the General Fisheries Commitee for the Mediterranean Sea (GFCM). 
# ---
# title: "Aggregate fishing effort grid to produce fishing pressure metrics"
# authors: "Jacopo Pulcinella, Enrico Nicola Armelloni"
# date: "February 1st, 2022"
# ---
# contact address: maps.irbim@irbim.cnr.it

rm(list=ls())
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
### Step 1: aggregate data to get cumulative fishing intensity by grid cell ####
DWfishing_data=fishing_data[fishing_data$DW==1,]
DWfishing_data=aggregate(list('f_hours'=DWfishing_data$f_hours), 
                         by=list('G_ID'= DWfishing_data$G_ID), FUN=sum)

### Step 2: calculate indicators ####
ind5_map=ind5(DWfishing_data)
ind6_map=ind6(DWfishing_data)

rm(DWfishing_data)
### Step 3: paste information to the grid ####
grid_ind5=grid[grid$G_ID%in%ind5_map,]
grid_ind6=merge(grid, ind6_map, by='G_ID', all.x=F)

rm(ind5_map, ind6_map)

## Plot it! ####
plot_indicator_map = defo_map+
  geom_sf(data = grid_ind5, aes(fill = "orange"), colour = NA) +
  geom_sf(data = grid_ind6, aes(fill = "tomato3"), colour = NA) +
  scale_fill_identity(guide='legend',
                      breaks = c("orange", "tomato3"),
                      labels = c("Ext", "Agg"),
                      name='')+
  coord_sf(xlim=c(lims[[1]], lims[[3]]), ylim=c(lims[[2]], lims[[4]]) , label_axes = list(bottom = "E", right = "N"));plot_indicator_map


ggsave(plot=plot_indicator_map, 
       "results/indicator_map.png",
       width = 20, height = 20, units='cm', dpi=500)

rm(plot_indicator_map, grid_ind5, grid_ind6, gsa,land,dep)

#  Time-series  ####
## Calculations ####
### Step 1: compute monthly indicator values ####
index=tibble(order=seq(1, 48, 1),
           temporal_ref=seq(as.Date(paste("2015", "01", "01", sep = "-")),
                   as.Date(paste("2018", "12", "01", sep = "-")),"month"))

ind_ts=NULL
for (i in 1:nrow(index)){
  # Subset data
  idat=fishing_data[fishing_data$DW==1&
                      fishing_data$year==lubridate::year(index[i,]$temporal_ref)&
                      fishing_data$month==lubridate::month(index[i,]$temporal_ref),]
  if(nrow(idat)==0){
    next
    }else{
      i5=ind5(idat)
      idat=aggregate(list('f_hours'=idat$f_hours), by=list('G_ID'=idat$G_ID), FUN=sum)
      i6=ind6(idat)
      ind_ts = rbind(ind_ts, data.frame(temporal_ref=index[i,]$temporal_ref, dcf5=length(i5),dcf6=nrow(i6))) 
      rm(i5,i6, idat)
    }
}

grid_cells=data.frame(table(grid$DW, grid$GSA))
ind_ts$ncell_rbs=grid_cells[grid_cells$Var1==1,]$Freq
ind_ts$dcf5_ratio = round(ind_ts$dcf5/ind_ts$ncell_rbs,4)
ind_ts$dcf6_ratio = round(ind_ts$dcf6/ind_ts$ncell_rbs, 4) 
ind_ts$dcf_ratio = round((ind_ts$dcf6)/(ind_ts$dcf5),4)
ind_ts$temporal_ref=as.Date(ind_ts$temporal_ref)

rm(grid_cells, grid)
### Step 2:  expand to full ts ####
ind_ts=merge(index, ind_ts, all.x = T)
ind_ts[is.na(ind_ts)]=0

ind_ts=reshape2:::melt(ind_ts[,c('temporal_ref','order', 'dcf5_ratio', 'dcf6_ratio')], id.var=1:2)
ind_ts$variable=factor(ifelse(ind_ts$variable == "dcf5_ratio", "Ext", "Agg"), levels = c("Ext", "Agg"))

## Specifications for the plot ####
### Customize labeling ####
plot_breaks=seq(min(ind_ts$order),max(ind_ts$order),3)
plot_labels=paste(substr(seq(min(ind_ts$temporal_ref),max(ind_ts$temporal_ref),"quarters"),1,4),
                  month.abb[as.numeric(substr(seq(min(ind_ts$temporal_ref),max(ind_ts$temporal_ref),"quarters"),6,7))], sep="-")
## Plot it ####
plot_indicators_ts = defo_ts +
  geom_bar(data = ind_ts , aes(x=order, value, fill = variable), stat = "identity", colour = 1, na.rm = F, size=0.2) +
  scale_fill_manual(values = c("orange", "tomato3")) + 
  scale_x_reverse(breaks=plot_breaks, labels=plot_labels)+
  geom_hline( aes(yintercept = mean(ind_ts[is.na(ind_ts$value)==F & ind_ts$variable =="Ext", ]$value)))+
  geom_hline( aes(yintercept = mean(ind_ts[is.na(ind_ts$value)==F & ind_ts$variable =="Agg", ]$value)), linetype=2)+
  xlab("") +
  ylab("Proportion of DW")  + 
  guides(fill = guide_legend(title = "Indicator", size = 15), linetype=guide_legend())+
  scale_y_continuous(sec.axis = sec_axis(~ . /50,breaks=c(1),labels="", name=expression(paste(""[""["Ext "]],   bold("__"), ""[""["  Agg "]],bold("_ _") ))), name=expression(""[""["Proportion of DW"]]));plot_indicators_ts

ggsave(plot=plot_indicators_ts, 
       "results/indicator_TS.png",
       width = 20, height = 20, units='cm', dpi=500)

