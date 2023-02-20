#______________________________________________________________________________#
#Code to combine Simulations and Visualize the Results





#setwd("~/ERCOT") #Code for personal device


#______________________________________________________________________________#
.libPaths("/rigel/cwc/users/yva2000/rpackages/")
###Load Packages
library(maps)       
library(dplyr)
library(ggplot2)
library(logspline)
library(foreach)    #Parallel Execution
library(doParallel) #Backend to foreach


#______________________________________________________________________________#
###Reading the Data
grid_locs <- read.csv("data/ERCOT_0_5_deg_lat_lon_index_key.csv", 
                      header = TRUE, sep=",")
map('state', region = c("Texas","OKlahoma", "New Mexico"))
points(grid_locs$lon-360,grid_locs$lat,pch=19,cex=.5)
title("Electric Reliability Council of Texas", cex = 2)


ssrd <- read.table("data/ERCOT_Solar_Rad_Daily.txt",  
                   sep =" ", 
                   header = TRUE) #Surface Solar Radiation Downwards
WP <- read.table("data/ERCOT_Wind_Power_Daily.txt",  
                 sep =" ", 
                 header = TRUE) #Surface Solar Radiation Downwards

#______________________________________________________________________________#
source("functions/Get_Simulation_Skill.R")
source("functions/Get_PCWavelet_Sim_Skill.R")
source("functions/Get_Annual_Cycle.R")
source("functions/Get_Site_Correlation.R")
source("functions/Get_Seasonal_Correlation.R")

#Details of the Data Field

#Field Type - Either Wind, Solar or Joint
Type <- "Joint"
nneib <- 60
nfiles <- 3


if(Type == "Joint"){
  #Field
  Fld <- as.matrix(cbind(WP,ssrd))

  #Compute Mean and SD
  solar_mean <- apply(ssrd,2,mean)
  wind_mean <- apply(WP, 2, mean)
  solar_sd <- apply(ssrd,2,sd)
  wind_sd <- apply(WP,2,sd)


  #Standardize
  Fld <- scale(Fld)

  #Load and combine the Simulations
  Sims <- list()
  l_c <- 0
  for(i in 1:nfiles){
    load(paste0(Type, "_Raw_Simulations_",nneib,"_",i,".RData"))
    u_c <- l_c + length(ynew_results)
    for(j in (l_c+1):u_c){
      Sims[[j]] <- ynew_results[[j-l_c]]
    }
    l_c <- u_c
    ynew_results <- NULL
  }
  
  
  
  #Hyperparameters
  nsim <- length(Sims)
  ngrids <- ncol(Fld)         #Number of grid points. Fields times Sites.
  N_valid <- nrow(Fld)        #Number of Time Steps
  n_site <- ngrids/2        #Number of Sites in the Field



  
  #Saving Simulation Skill
  pdf(paste0("ERCOT_Daily_",Type,"_Scaled_",nneib,".pdf"))

  ###Seperating the Simulations and Scaling the Data
  wind_sims <- solar_sims <- list()
  for(i in 1:nsim){
    temp <- sweep(Sims[[i]]$WPnew, 2, wind_mean)
    wind_sims[[i]] <- sweep(temp, 2, wind_sd, `/`)
    temp <- sweep(Sims[[i]]$SSnew, 2, solar_mean)
    solar_sims[[i]] <- sweep(temp, 2, solar_sd, `/`)
    Sims[[i]] <- NA
  }
  print(length(Sims))


  ###Wind
  Field <- "Wind"
  fld <- Fld[,1:n_site]
  Get_Simulation_Skill(True_Data = fld,
                     Simulations = wind_sims,
                     Field_Name = Field)
  
  Get_PCWavelet_Sim_Skill(Data_Field = fld,
                          Field_Name = Field,
                          Sims = wind_sims,
                          PCs=2)



  Get_Annual_Cycle(True_Data = fld, Field_Name = Field,
                 Simulations = wind_sims,
                 Resolution = "daily",
                 Start_Date = "01-01-1970")



  ###Solar
  Field <- "Solar"
  fld <- Fld[,(n_site+1):ncol(Fld)]
  Get_Simulation_Skill(True_Data = fld,
                     Simulations = solar_sims,
                     Field_Name = Field)
  
  Get_PCWavelet_Sim_Skill(Data_Field = fld,
                          Field_Name = Field,
                          Sims = solar_sims,
                          PCs=2)


  Get_Annual_Cycle(True_Data = fld, Field_Name = Field,
                 Simulations = solar_sims,
                 Resolution = "daily",
                 Start_Date = "01-01-1970")


  ###Cross - Correlation across Sites. 
  Get_Site_Correlation(Fld1 = Fld[,1:n_site], #Wind
                     Fld2 = Fld[,(n_site+1):ncol(Fld)], #Solar
                     Fld1_Sims = wind_sims,
                     Fld2_Sims = solar_sims,
                     Grid = grid_locs)
  
  Get_Seasonal_Correlation(Fld1 = Fld[,1:n_site],
                           Fld2 = Fld[,(n_site+1):ncol(Fld)],
                           Fld1_Sims = wind_sims, 
                           Fld2_Sims = solar_sims, 
                           Grid = grid_locs,
                           start_date = "01-01-1979")



  dev.off()
} else {
  
  if(Type == "Solar"){
    #Field
    Fld <- as.matrix(ssrd)
  } else {
    Fld <- as.matrix(WP)
  }
  
  #Compute Mean and SD
  fld_mean <- apply(Fld,2,mean)
  fld_sd <- apply(Fld,2,sd)
  
  #Standardize
  Fld <- scale(Fld)
  
  #Load and combine the Simulations
  Sims <- list()
  l_c <- 0
  for(i in 1:nfiles){
    load(paste0(Type, "_Raw_Simulations_",nneib,"_",i,".RData"))
    u_c <- l_c + length(ynew_results)
    for(j in (l_c+1):u_c){
      Sims[[j]] <- ynew_results[[j-l_c]]
    }
    l_c <- u_c
    ynew_results <- NULL
  }
  print(length(Sims))
  
  #Hyperparameters
  nsim <- length(Sims)

  
  ###Seperating the Simulations
  for(i in 1:nsim){
    temp <- sweep(Sims[[i]]$Xnew, 2, fld_mean)
    Sims[[i]] <- sweep(temp, 2, fld_sd, `/`)
    temp <- NULL
  }
  print(length(Sims))
  
  
  #Saving Simulation Skill
  pdf(paste0("ERCOT_Daily_",Type,"_Scaled_",nneib,".pdf"))
  
  Get_Simulation_Skill(True_Data = Fld,
                       Simulations = Sims,
                       Field_Name = Type)
  
  Get_PCWavelet_Sim_Skill(Data_Field = Fld,
                          Field_Name = Field,
                          Sims = Sims,
                          PCs=2)
  
  Get_Annual_Cycle(True_Data = Fld, Field_Name = Type,
                   Simulations = Sims,
                   Resolution = "daily",
                   Start_Date = "01-01-1970")
  
  
  
  
  
  dev.off()
  
}