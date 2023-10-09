#______________________________________________________________________________#
#Energy Droughts -- Hourly Timescales
#Code to break down wind and solar to efficiently use memory. 




#Set-up Directory Path
setwd("~/GitHub/CKSTS") #Code for personal device


#______________________________________________________________________________#


###Load Packages
library(maps)       
library(dplyr)
library(ggplot2)
library(foreach)    #Parallel Execution
library(doParallel) #Backend to foreach
library(lubridate)
library(gridExtra)

#Load the Functions
source("functions/Get_Wind_Capacity_Factor.R")
source("functions/Get_Energy_Droughts.R")
source("functions/Get_Energy_Production_Plots_Single.R")
source("functions/Get_Total_Production_Profiles_Single.R")

#______________________________________________________________________________#
###Reading the Data
grid_locs <- read.csv("data/ERCOT_lat_lon_index_key.csv", 
                      header = TRUE, sep=",")


ssrd <- read.table("data/ERCOT_Solar_Rad_Hourly_18_22.txt",  
                   sep =" ", 
                   header = TRUE) #Surface Solar Radiation Downwards

WP <- read.table("data/ERCOT_Wind_Speed_Hourly_18_22.txt",  
                 sep =" ", 
                 header = TRUE) #Wind Capacity Factors

ercot_generators <- read.csv("data/ERCOT_Generators.csv",
                             header = TRUE, sep=",")


#Load the Simulations
load("sims/Clust_Raw_Simulations_5_yrs.RData")

#Divide into wind and solar sims
nsim <- length(ynew_results)
wind_sims <- solar_sims <- list()
for(i in 1:nsim){
  wind_sims[[i]] <- ynew_results[[i]]$WPnew
  solar_sims[[i]] <- ynew_results[[i]]$SSnew
}
ynew_results <- NULL

pdf("Energy_Production_Profiles.pdf")







#______________________________________________________________________________#
#----------------------------Get Weights---------------------------------------#

#Wind
wind_gen <- ercot_generators %>% filter(PrimSource == "wind")
tx_wind_weights <- rep(0,216)
for(i in 1:nrow(wind_gen)){
  tx_wind_weights[wind_gen$lat_lon_index[i]] <- tx_wind_weights[wind_gen$lat_lon_index[i]] + wind_gen$Install_MW[i]
}


#Solar
solar_gen <- ercot_generators %>% filter(PrimSource == "solar")
tx_solar_weights <- rep(0,216)
for(i in 1:nrow(solar_gen)){
  tx_solar_weights[solar_gen$lat_lon_index[i]] <- tx_solar_weights[solar_gen$lat_lon_index[i]] + solar_gen$Install_MW[i]
}

Total_Cap <- sum(tx_solar_weights)+sum(tx_wind_weights)


#______________________________________________________________________________#
#----------------------------Uniform Capacity-----------------------------#
plt_title = "Uniform Capacity"

#Wind Aggregate Production
wind_prod <- get_total_production_wind(Data_Wind = WP,
                                       Simulations = wind_sims,
                                       Wind_Weights = rep(sum(tx_wind_weights)/216, 216))

#Solar Aggregate Production
solar_prod <- get_total_production_solar(Data_Solar = ssrd,
                                         Simulations = solar_sims,
                                         Solar_Weights = rep(sum(tx_solar_weights)/216, 216))

#Get the plots of total production
get_total_production_plots(Wind_Production = wind_prod$Data,
                           Solar_Production = solar_prod$Data,
                           Wind_Production_Sims = wind_prod$Sims,
                           Solar_Production_Sims = solar_prod$Sims,
                           plt_title = plt_title)



#______________________________________________________________________________#
#----------------------------Different Start Hours-----------------------------#
#strftime(as.POSIXct("10-15-1979 00:00", format = "%m-%d-%Y %H:%M"), format = "%j")

p1 <- get_initiation_plot(Wind_Production = wind_prod$Data,
                          Solar_Production = solar_prod$Data,
                          Wind_Production_Sims = wind_prod$Sims,
                          Solar_Production_Sims = solar_prod$Sims,
                          start_date = "01-01-2018 00:00",
                          start_day = 15,
                          start_hours = c(4,8,12,16,20),
                          horizon = 12,
                          plt_title = paste0("Jan 15 \n",plt_title))


p2 <- get_initiation_plot(Wind_Production = wind_prod$Data,
                          Solar_Production = solar_prod$Data,
                          Wind_Production_Sims = wind_prod$Sims,
                          Solar_Production_Sims = solar_prod$Sims,
                          start_date = "01-01-2018 00:00",
                          start_day = 105,
                          start_hours = c(4,8,12,16,20),
                          horizon = 12,
                          plt_title = paste0("Apr 15 \n",plt_title))


p3 <- get_initiation_plot(Wind_Production = wind_prod$Data,
                          Solar_Production = solar_prod$Data,
                          Wind_Production_Sims = wind_prod$Sims,
                          Solar_Production_Sims = solar_prod$Sims,
                          start_date = "01-01-2018 00:00",
                          start_day = 196,
                          start_hours = c(4,8,12,16,20),
                          horizon = 12,
                          plt_title = paste0("Jul 15 \n",plt_title))

p4 <- get_initiation_plot(Wind_Production = wind_prod$Data,
                          Solar_Production = solar_prod$Data,
                          Wind_Production_Sims = wind_prod$Sims,
                          Solar_Production_Sims = solar_prod$Sims,
                          start_date = "01-01-2018 00:00",
                          start_day = 288,
                          start_hours = c(4,8,12,16,20),
                          horizon = 12,
                          plt_title = paste0("Oct 15 \n",plt_title))



#______________________________________________________________________________#
#------------------------Different Time Horizons-------------------------------#
#strftime(as.POSIXct("10-15-1979 00:00", format = "%m-%d-%Y %H:%M"), format = "%j")


p5 <- get_horizon_plot(Wind_Production = wind_prod$Data,
                       Solar_Production = solar_prod$Data,
                       Wind_Production_Sims = wind_prod$Sims,
                       Solar_Production_Sims = solar_prod$Sims,
                       start_date = "01-01-2018 00:00",
                       start_day = 15,
                       start_hour = 8,
                       horizons = c(6,9,12,15,18),
                       plt_title = paste0("Jan 15 \n",plt_title))


p6 <- get_horizon_plot(Wind_Production = wind_prod$Data,
                       Solar_Production = solar_prod$Data,
                       Wind_Production_Sims = wind_prod$Sims,
                       Solar_Production_Sims = solar_prod$Sims,
                       start_date = "01-01-2018 00:00",
                       start_day = 105,
                       start_hour = 8,
                       horizons = c(6,9,12,15,18),
                       plt_title = paste0("Apr 15 \n",plt_title))


p7 <- get_horizon_plot(Wind_Production = wind_prod$Data,
                       Solar_Production = solar_prod$Data,
                       Wind_Production_Sims = wind_prod$Sims,
                       Solar_Production_Sims = solar_prod$Sims,
                       start_date = "01-01-2018 00:00",
                       start_day = 196,
                       start_hour = 8,
                       horizons = c(6,9,12,15,18),
                       plt_title = paste0("Jul 15 \n",plt_title))


p8 <- get_horizon_plot(Wind_Production = wind_prod$Data,
                       Solar_Production = solar_prod$Data,
                       Wind_Production_Sims = wind_prod$Sims,
                       Solar_Production_Sims = solar_prod$Sims,
                       start_date = "01-01-2018 00:00",
                       start_day = 288,
                       start_hour = 8,
                       horizons = c(6,9,12,15,18),
                       plt_title = paste0("Oct 15 \n",plt_title))





#______________________________________________________________________________#
#----------------------------Installed Capacity-----------------------------#
plt_title = "Installed Capacity"

#Wind Aggregate Production
wind_prod <- get_total_production_wind(Data_Wind = WP,
                                       Simulations = wind_sims,
                                       Wind_Weights = tx_wind_weights)

#Solar Aggregate Production
solar_prod <- get_total_production_solar(Data_Solar = ssrd,
                                         Simulations = solar_sims,
                                         Solar_Weights = tx_solar_weights)

#Get the plots of total production
get_total_production_plots(Wind_Production = wind_prod$Data,
                           Solar_Production = solar_prod$Data,
                           Wind_Production_Sims = wind_prod$Sims,
                           Solar_Production_Sims = solar_prod$Sims,
                           plt_title = plt_title)



#______________________________________________________________________________#
#----------------------------Different Start Hours-----------------------------#
#strftime(as.POSIXct("10-15-1979 00:00", format = "%m-%d-%Y %H:%M"), format = "%j")

p9 <- get_initiation_plot(Wind_Production = wind_prod$Data,
                          Solar_Production = solar_prod$Data,
                          Wind_Production_Sims = wind_prod$Sims,
                          Solar_Production_Sims = solar_prod$Sims,
                          start_date = "01-01-2018 00:00",
                          start_day = 15,
                          start_hours = c(4,8,12,16,20),
                          horizon = 12,
                          plt_title = paste0("Jan 15 \n",plt_title))


p10 <- get_initiation_plot(Wind_Production = wind_prod$Data,
                          Solar_Production = solar_prod$Data,
                          Wind_Production_Sims = wind_prod$Sims,
                          Solar_Production_Sims = solar_prod$Sims,
                          start_date = "01-01-2018 00:00",
                          start_day = 105,
                          start_hours = c(4,8,12,16,20),
                          horizon = 12,
                          plt_title = paste0("Apr 15 \n",plt_title))


p11 <- get_initiation_plot(Wind_Production = wind_prod$Data,
                          Solar_Production = solar_prod$Data,
                          Wind_Production_Sims = wind_prod$Sims,
                          Solar_Production_Sims = solar_prod$Sims,
                          start_date = "01-01-2018 00:00",
                          start_day = 196,
                          start_hours = c(4,8,12,16,20),
                          horizon = 12,
                          plt_title = paste0("Jul 15 \n",plt_title))

p12 <- get_initiation_plot(Wind_Production = wind_prod$Data,
                          Solar_Production = solar_prod$Data,
                          Wind_Production_Sims = wind_prod$Sims,
                          Solar_Production_Sims = solar_prod$Sims,
                          start_date = "01-01-2018 00:00",
                          start_day = 288,
                          start_hours = c(4,8,12,16,20),
                          horizon = 12,
                          plt_title = paste0("Oct 15 \n",plt_title))




#______________________________________________________________________________#
#------------------------Different Time Horizons-------------------------------#
#strftime(as.POSIXct("10-15-1979 00:00", format = "%m-%d-%Y %H:%M"), format = "%j")


p13 <- get_horizon_plot(Wind_Production = wind_prod$Data,
                       Solar_Production = solar_prod$Data,
                       Wind_Production_Sims = wind_prod$Sims,
                       Solar_Production_Sims = solar_prod$Sims,
                       start_date = "01-01-2018 00:00",
                       start_day = 15,
                       start_hour = 8,
                       horizons = c(6,9,12,15,18),
                       plt_title = paste0("Jan 15 \n",plt_title))


p14 <- get_horizon_plot(Wind_Production = wind_prod$Data,
                       Solar_Production = solar_prod$Data,
                       Wind_Production_Sims = wind_prod$Sims,
                       Solar_Production_Sims = solar_prod$Sims,
                       start_date = "01-01-2018 00:00",
                       start_day = 105,
                       start_hour = 8,
                       horizons = c(6,9,12,15,18),
                       plt_title = paste0("Apr 15 \n",plt_title))


p15 <- get_horizon_plot(Wind_Production = wind_prod$Data,
                       Solar_Production = solar_prod$Data,
                       Wind_Production_Sims = wind_prod$Sims,
                       Solar_Production_Sims = solar_prod$Sims,
                       start_date = "01-01-2018 00:00",
                       start_day = 196,
                       start_hour = 8,
                       horizons = c(6,9,12,15,18),
                       plt_title = paste0("Jul 15 \n",plt_title))


p16 <- get_horizon_plot(Wind_Production = wind_prod$Data,
                       Solar_Production = solar_prod$Data,
                       Wind_Production_Sims = wind_prod$Sims,
                       Solar_Production_Sims = solar_prod$Sims,
                       start_date = "01-01-2018 00:00",
                       start_day = 288,
                       start_hour = 8,
                       horizons = c(6,9,12,15,18),
                       plt_title = paste0("Oct 15 \n",plt_title))

#Plotting the figures
grid.arrange(p1, p9, p3, p11, nrow = 2)
grid.arrange(p2, p10, p4, p12, nrow = 2)

grid.arrange(p5, p13, p7, p15, nrow = 2)
grid.arrange(p6, p14, p8, p16, nrow = 2)

dev.off()