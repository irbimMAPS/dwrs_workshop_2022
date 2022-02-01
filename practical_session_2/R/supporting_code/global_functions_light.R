"%ni%"=Negate('%in%')


###--- Estimate fishing effort ####
estimate_fishing_effort <- function(fishing_tracks, grid){
  lapply(fishing_tracks, function(x){
    if(is.data.frame(x)){
      x = st_sf(x)
    }
    xgear = unique(x$gear)
    st_crs(x)=wgs # set crs 
    if(xgear == "PS"){
      x$duration=difftime(x$f_time, x$s_time, units="secs")
      x=st_as_sf(x)
      xint=st_intersection(grid, x)
      xint=data.frame(xint)
      xint=xint[,c("grid_id", "gear", "duration")]
      xint$fishing_hours=xint$duration/3600
      f_hours = aggregate(fishing_hours~grid_id+gear, data = xint, sum)
      colnames(f_hours)[ncol(f_hours)] = "f_hours"
      grid_edit=merge(grid, f_hours, by="grid_id") # combine effort to grid
      return(grid_edit)
    }else{
      #st_crs(x)=4326 
      #x=st_transform(x, 2100) 
      xnames=names(x)
      x$distance=st_length(x$geometry, units="m") # estimated fishing track 
      x$duration=difftime(x$f_time, x$s_time, units="secs")
      x$speed_ms=as.numeric(x$distance)/as.numeric(x$duration)
      x$speed_kn=x$speed_ms*1.94384
      x=st_as_sf(x)
      xint=st_intersection(grid, x)
      if(nrow(xint) > 0){
        xint$observed_swept=st_length(xint)
        xint=data.frame(xint)
        xint=xint[,c("grid_id","MMSI", "trip", "s_time" ,"gear","observed_swept", "speed_ms")]
        xint$fishing_hours=(as.numeric(xint$observed_swept)/xint$speed_ms)/3600
        xint
        f_hours = aggregate(fishing_hours~grid_id+gear+MMSI+trip+s_time, data = xint, sum)  # estimate fishing effort in cells grid
        colnames(f_hours)[ncol(f_hours)] = "f_hours"
        grid_edit=merge(grid, f_hours, by="grid_id") # combine effort to grid
        grid_edit=grid_edit[grid_edit$f_hours>0,]
        grid_edit=grid_edit[,c("MMSI","gear", "trip", "s_time" , 'G_ID', 'DW', 'f_hours')]
        return(grid_edit)
      }
    }
  })
  
}

# standardize data
st_fun = function(x){
  (x-min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))
}

# Indicator - fishing effort

ind6=function(x){
  x = x[order(x$f_hours, decreasing = T),]
  x$cum = cumsum(x$f_hours)
  xthr = sum(x$f_hours, na.rm = T) * 0.9
  x=x[which(x$cum <= xthr),]
  return(x)
}

ind5=function(x){
  x = unique(x[,c('G_ID')])
  return(x)
}


