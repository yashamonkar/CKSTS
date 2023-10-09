#______________________________________________________________________________#
####CODE TO GET GRID POINTS INSIDE ERCOT INTERCONNECTION################

###INPUT
#1. GRID BOX USED TO DOWNLOAD THE ERA-5 data. 
#2. NERC Shape Files. 

#______________________________________________________________________________#
###Load Dependencies and Data

#Setting Working Directory
setwd("~/KSTS-Variant")

#Packages
library(ncdf4) 

#Load the shape-file data 
grid_locs <- read.table("data/ERCOT_lat_lon_index_key.csv", header = TRUE,
                      sep=",")
grid_locs$lon <- grid_locs$lon-360

#Hyper-parameters
Years <- c(2018:2022)

#______________________________________________________________________________#
###ERA-5 Gridbox
setwd("~/ERA_5") #Point to the ERA-5 data source

####Solar
tx_solar <- list()

for(i in 1:length(Years)){
  
  #Current year
  yr <- Years[i]
  
  #Read the file
  name <- paste0("solar_radiation/",yr,".nc")
  nc_data <- nc_open(name)
  print(nc_data) 

  #Get the attributes
  lon <- ncvar_get(nc_data, "longitude")
  lat <- ncvar_get(nc_data, "latitude")
  ssrd <- ncvar_get(nc_data, "ssrd")


  #Subset to needed values
  sub_land <- matrix(ncol = nrow(grid_locs),
                     nrow = dim(ssrd)[3])

  for(j in 1:ncol(sub_land)){
    lon_gr <- which(lon == grid_locs$lon[j])
    lat_gr <- which(lat == grid_locs$lat[j])
  
    sub_land[,j] <- ssrd[lon_gr,lat_gr,]
  }

  tx_solar[[i]] <- sub_land
}

#Convert to a single dataframe. 
tx_solar <- do.call("rbind", tx_solar)



####Wind
tx_wind <- list()

for(i in 1:length(Years)){
  
  #Current year
  yr <- Years[i]
  
  #Read the file
  name <- paste0("wind_speeds/",yr,".nc")
  nc_data <- nc_open(name)
  print(nc_data) 
  
  #Get the attributes
  lon <- ncvar_get(nc_data, "longitude")
  lat <- ncvar_get(nc_data, "latitude")
  u100 <- ncvar_get(nc_data, "u100")
  v100 <- ncvar_get(nc_data, "v100")
  
  
  #Subset to needed values
  sub_land <- matrix(ncol = nrow(grid_locs),
                     nrow = dim(u100)[3])
  
  for(j in 1:ncol(sub_land)){
    lon_gr <- which(lon == grid_locs$lon[j])
    lat_gr <- which(lat == grid_locs$lat[j])
  
    sub_land[,j] <- sqrt(u100[lon_gr,lat_gr,]^2 + v100[lon_gr,lat_gr,]^2)
  }
  
  tx_wind[[i]] <- sub_land
}

#Convert to a single dataframe. 
tx_wind <- do.call("rbind", tx_wind)


#####Sace the data-sets
setwd("~/KSTS-Variant/data")
write.table(tx_solar, "ERCOT_Solar_Rad_Hourly_18_22.txt", sep=" ")
write.table(tx_wind, "ERCOT_Wind_Speed_Hourly_18_22.txt", sep=" ")
