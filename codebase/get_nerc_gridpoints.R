#______________________________________________________________________________#
#####FUNCTION TO GET WIND SOLAR AND TEMPERATURE DATA FOR NERC REGION#############

###INPUT
#1. Year
#2. Grid-Region


###OUTPUT
#1. Grid points
#2. Wind-Solar-Temperature Data


#Setting Working Directory
setwd("~/GitHub/CKSTS")

#______________________________________________________________________________#
###Load Dependencies and Data

#Packages
library(ncdf4) 
library(rgdal)
library(ggplot2)
library(dplyr)

#Load Functions
source("functions/Get_Conus_Regions.R")

#Load the data files
nerc_pop_temp <- get(load("data/NERC_Regions_Temp_Population.RData"))


#NERC Shapefiles
egrids <- readOGR(dsn= paste0("data/sf/egrid2020_subregions"),
                  layer="eGRID2020_subregions")
nerc_sf <- get_egrids(egrids_sf = egrids) #Convert to needed regions
n_regions <- length(nerc_sf$Labels)
nerc_labels <- c("Arizona/New Mexico", "CAISO", "ERCOT", "Florida", 
                 "Wisconsin (Rural)", "Midwest (MISO)", "ISO New England", 
                 "Northwest", "NYISO", "PJM (East)", "Michigan", "PJM (West)", 
                 "Colorado", "Kansas", "Oklahoma", "Arkansas/Louisiana" , 
                 "Missouri" ,"Southeast", "Tennesse Valley", "Carolinas")

#ggplot shape files
world <- map_data("world")
us <- map_data("state")




#______________________________________________________________________________#
###Data Wrangling and Hyper-parameter selection

#Hyper-parameters 
yr <- 2020 #Select the year
sel_rto <- 2 #Select the Grid Sub_region
sub_region <- nerc_sf$Shapefiles[[sel_rto]]
RTO_Label <- nerc_labels[sel_rto]


#Identify unique lat-lon locations
grid_locs <- nerc_pop_temp[[2]][[sel_rto]]
grid_locs <- unique(grid_locs[c("Longitude","Latitude")]) #Subset unique



###----------------------Plot the sub-region---------------------------------###

#Get Lat-Lon Extend
lat_lon <- sub_region %>% fortify() %>% select(long,lat)
lat_min <- min(lat_lon$lat)-0.5
lat_max <- max(lat_lon$lat)+0.5
lon_min <- min(lat_lon$long)-0.5
lon_max <- max(lat_lon$long)+0.5

p1 <-  ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "#000000", size = 0.15) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = NA) +
  geom_polygon(data = sub_region, mapping = aes( x = long, y = lat, group = group), 
               fill = "#FFFFFF", color = 'black', size = 1) + 
  geom_point(data = grid_locs, mapping = aes(x = Longitude, y = Latitude), col='red') +
  #geom_polygon(data = pop_regions, mapping = aes( x = long, y = lat, group = group), 
  #            fill = "black", color = NA, alpha = 0.2) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(lon_min, lon_max)) +
  scale_y_continuous(name = " ", limits = c(lat_min, lat_max)) +
  ggtitle(paste0("  ",RTO_Label)) +
  theme_bw() +
  theme(axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=28))

print(p1)


#______________________________________________________________________________#
###Solar

#Read a single ERA-5 download file
name <- c(paste0("data/solar/",yr,".nc"))
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
t2 <- as.POSIXct(paste0("01-01-",yr," 00:00"), format = "%m-%d-%Y %H:%M")
tdiff <- t[1] - difftime(t2, t1, units="hours")
plot(rowMeans(ssrd)[1:120], type='l', main = paste0(RTO_Label, 
                                                    " - Solar \n The time difference is ",
                                                    tdiff))


#______________________________________________________________________________#
###Temperature

#Read a single ERA-5 download file
name <- c(paste0("data/temp/",yr,".nc"))
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
t2 <- as.POSIXct(paste0("01-01-",yr," 00:00"), format = "%m-%d-%Y %H:%M")
tdiff <- t[1] - difftime(t2, t1, units="hours")
plot(rowMeans(t2m)[1:120], type='l', main = paste0(RTO_Label, 
                                                    " - Temperature \n The time difference is ",
                                                    tdiff))



#______________________________________________________________________________#
###Wind

#Read a single ERA-5 download file
name <- c(paste0("data/wind/",yr,".nc"))
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
t2 <- as.POSIXct(paste0("01-01-",yr," 00:00"), format = "%m-%d-%Y %H:%M")
tdiff <- t[1] - difftime(t2, t1, units="hours")
plot(rowMeans(WS)[1:120], type='l', main = paste0(RTO_Label, 
                                                   " - Temperature \n The time difference is ",
                                                   tdiff))


###Save the datasets
grid_fields <- list()
grid_fields[[1]] <- WS
grid_fields[[2]] <- ssrd
grid_fields[[3]] <- t2m
save(grid_fields, file = paste0("Trial_Fields.RData"))

