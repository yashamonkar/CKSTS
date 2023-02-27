#______________________________________________________________________________#
#####FUNCTION TO GET WIND SOLAR AND TEMPERATURE DATA FOR NERC REGION#############

###INPUT
#1. Year
#2. Grid-Region


###OUTPUT
#1. Grid points
#2. Wind-Solar-Temperature Data

#year <- 2020
#rto <- 10
#Shapefiles <- nerc_sf$Shapefiles
#Labels <- nerc_labels
#All_Grids <- nerc_pop_temp[[2]]


get_nerc_gridpoints <- function(year, rto, Labels, All_Grids){
  
  #Packages
  library(ncdf4) 
  library(ggplot2)
  library(dplyr)
  
  
  #ggplot shape files
  world <- map_data("world")
  us <- map_data("state")
  
  
  #RTO Parameters
  RTO_Label <- Labels[rto]
  
  
  #Identify unique lat-lon locations
  grid_locs <- All_Grids[[rto]]
  grid_locs <- unique(grid_locs[c("Longitude","Latitude")]) #Subset unique
  
  
  
  
  #______________________________________________________________________________#
  ###Solar
  
  #Read a single ERA-5 download file
  name <- c(paste0("data/solar/",year,".nc"))
  nc_data <- nc_open(name)
  print(nc_data) 
  
  #Get the attributes
  lon <- ncvar_get(nc_data, "longitude")
  lat <- ncvar_get(nc_data, "latitude")
  t <- ncvar_get(nc_data, "time")
  tvar <- ncvar_get(nc_data, "ssrd") 
  
  nc_close(nc_data) #Closing the Netcdf file. 
  
  
  #Subset to needed values
  ssrd  <- matrix(ncol = nrow(grid_locs),
                  nrow = dim(tvar)[3])
  
  for(j in 1:ncol(ssrd)){
    lon_gr <- which(lon == grid_locs$Longitude[j])
    lat_gr <- which(lat == grid_locs$Latitude[j])
    
    ssrd[,j] <- tvar[lon_gr,lat_gr,]
  }
  
  #Time consistency Check
  t1 <- as.POSIXct("01-01-1900 00:00", format = "%m-%d-%Y %H:%M")
  t2 <- as.POSIXct(paste0("01-01-",year," 00:00"), format = "%m-%d-%Y %H:%M")
  tdiff <- t[1] - difftime(t2, t1, units="hours")
  plot(rowMeans(ssrd)[1:120], type='l', main = paste0(RTO_Label, 
                                                      " - Solar \n The time difference is ",
                                                      tdiff))
  
  
  #______________________________________________________________________________#
  ###Temperature
  
  #Read a single ERA-5 download file
  name <- c(paste0("data/temp/",year,".nc"))
  nc_data <- nc_open(name)
  print(nc_data) 
  
  #Get the attributes
  lon <- ncvar_get(nc_data, "longitude")
  lat <- ncvar_get(nc_data, "latitude")
  t <- ncvar_get(nc_data, "time")
  tvar <- ncvar_get(nc_data, "t2m") 
  
  nc_close(nc_data) #Closing the Netcdf file. 
  
  
  #Subset to needed values
  t2m  <- matrix(ncol = nrow(grid_locs),
                 nrow = dim(tvar)[3])
  
  for(j in 1:ncol(t2m)){
    lon_gr <- which(lon == grid_locs$Longitude[j])
    lat_gr <- which(lat == grid_locs$Latitude[j])
    
    t2m[,j] <- tvar[lon_gr,lat_gr,]
  }
  
  #Time consistency Check
  t1 <- as.POSIXct("01-01-1900 00:00", format = "%m-%d-%Y %H:%M")
  t2 <- as.POSIXct(paste0("01-01-",year," 00:00"), format = "%m-%d-%Y %H:%M")
  tdiff <- t[1] - difftime(t2, t1, units="hours")
  plot(rowMeans(t2m)[1:120], type='l', main = paste0(RTO_Label, 
                                                     " - Temperature \n The time difference is ",
                                                     tdiff))
  
  
  
  #______________________________________________________________________________#
  ###Wind
  
  #Read a single ERA-5 download file
  name <- c(paste0("data/wind/",year,".nc"))
  nc_data <- nc_open(name)
  print(nc_data) 
  
  #Get the attributes
  lon <- ncvar_get(nc_data, "longitude")
  lat <- ncvar_get(nc_data, "latitude")
  t <- ncvar_get(nc_data, "time")
  tvar_u <- ncvar_get(nc_data, "u100") 
  tvar_v <- ncvar_get(nc_data, "v100") 
  
  nc_close(nc_data) #Closing the Netcdf file. 
  
  #Convert to wind speed
  tvar <- sqrt(tvar_u^2 + tvar_v^2)
  
  #Subset to needed values
  WS  <- matrix(ncol = nrow(grid_locs),
                nrow = dim(tvar)[3])
  
  for(j in 1:ncol(WS)){
    lon_gr <- which(lon == grid_locs$Longitude[j])
    lat_gr <- which(lat == grid_locs$Latitude[j])
    
    WS[,j] <- tvar[lon_gr,lat_gr,]
  }
  
  #Time consistency Check
  t1 <- as.POSIXct("01-01-1900 00:00", format = "%m-%d-%Y %H:%M")
  t2 <- as.POSIXct(paste0("01-01-",year," 00:00"), format = "%m-%d-%Y %H:%M")
  tdiff <- t[1] - difftime(t2, t1, units="hours")
  plot(rowMeans(WS)[1:120], type='l', main = paste0(RTO_Label, 
                                                    " - Wind \n The time difference is ",
                                                    tdiff))
  
  
  ###Consolidate the points
  grid_fields <- list()
  grid_fields[[1]] <- WS
  grid_fields[[2]] <- ssrd
  grid_fields[[3]] <- t2m
  grid_fields[[4]] <- grid_locs
  
  return(grid_fields)  
  
}