#______________________________________________________________________________#
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
source("functions/Get_Simulation_Skill.R")
source("functions/Get_PCWavelet_Sim_Skill.R")
source("functions/Get_Annual_Cycle.R")
source("functions/Get_Site_Correlation.R")
source("functions/Get_PCA_ggplot.R")
source("functions/Get_Seasonal_Correlation.R")
source("functions/Get_Wind_PDF.R")
source("functions/Get_Wind_Power.R")


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

pdf("figures/Simulation_Check_Plots.pdf")


#------------------------------------------------------------------------------# 
###Wind
Field <- "Wind"
Get_Simulation_Skill(True_Data = WP,
                     Simulations = wind_sims,
                     Field_Name = Field)

Get_PCWavelet_Sim_Skill(Data_Field = WP,
                        Field_Name = Field,
                        Sims = wind_sims,
                        PCs=2)

Get_Annual_Cycle(True_Data = WP,
                 Field_Name = Field,
                 Simulations = wind_sims,
                 Resolution = "hourly",
                 Start_Date = "01-01-2018 00:00")

get_pca_plot(X = WP, 
             Grid = grid_locs, 
             Field = Field,
             Sims = wind_sims)

#------------------------------------------------------------------------------#
###Solar
Field <- "Solar"
Get_Simulation_Skill(True_Data = ssrd,
                     Simulations = solar_sims,
                     Field_Name = Field)

Get_PCWavelet_Sim_Skill(Data_Field = ssrd,
                        Field_Name = Field,
                        Sims = solar_sims,
                        PCs=2)

Get_Annual_Cycle(True_Data = ssrd, Field_Name = Field,
                 Simulations = solar_sims,
                 Resolution = "hourly",
                 Start_Date = "01-01-2018 00:00")

get_pca_plot(X = ssrd, 
             Grid = grid_locs, 
             Field = Field,
             Sims = solar_sims)



#------------------------------------------------------------------------------#
###Cross - Correlation across Sites. 
Get_Site_Correlation(Fld1 = WP, #Wind
                     Fld2 = ssrd, #Solar
                     Fld1_Sims = wind_sims,
                     Fld2_Sims = solar_sims, 
                     Grid = grid_locs)


Get_Seasonal_Correlation(Fld1 = WP, #Wind
                         Fld2 = ssrd, #Solar
                         Fld1_Sims = wind_sims,
                         Fld2_Sims = solar_sims, 
                         Grid = grid_locs,
                         start_date = "01-01-1979 00:00",
                         col_hx = "#af8dc3")

dev.off()
