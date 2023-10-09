#______________________________________________________________________________#
#CODE TO GENERATE PLOTS FOR CONTRACT INITIATION and CONTRACT HORIZON


#Variables
#1. Data (Wind and Solar separate)
#2. Start Date #"01-01-1979 00:00"
#3. Start Day (DOY) #15
#4. Start Hour #8 [Single or Multiple]
#5. Time Horizon #12 hours [Single or Multiple]
#6. Simulations #Consolidated 
#7. Plot Title (Basically the date)


#Output
#Plot




#______________________________________________________________________________#
#FUNCTION WITH SINGLE HORIZONS AND MULTIPLE START TIME

get_initiation_plot <- function(Wind_Production,Solar_Production, 
                                Wind_Production_Sims,Solar_Production_Sims,
                                start_date, start_day,
                                start_hours,
                                horizon, 
                                plt_title){
  
  #___________________________________________________________________#
  #-----------------------Total Production----------------------------#
  #Number of simulations
  nsim = length(Solar_Production_Sims)
  
  #Data
  Total_Prod <- Wind_Production + Solar_Production
  
  #Simulations
  Total_Prod_Sims <- list()
  for(i in 1:nsim){
    Total_Prod_Sims[[i]] = Wind_Production_Sims[[i]] + Solar_Production_Sims[[i]]
  }
  
  #___________________________________________________________________#
  #-----------------------------Data---------------------------#
  #Get the production level
  Data_Production <- list()
  for(i in 1:length(start_hours)) {
    Data_Production[[i]] <- get_production_profile(Total_Supply = Total_Prod,
                                                   start_date = start_date,
                                                   start_day = start_day,
                                                   start_hour = start_hours[i],
                                                   horizon = horizon,
                                                   Type = start_hours[i]) }
  Data_Production <- bind_rows(lapply(Data_Production,data.frame))
  
  
  
  #___________________________________________________________________#
  #-----------------------------Simulations---------------------------#
  nsim <- length(Wind_Production_Sims)
  
  #Start parallel processing
  cores=detectCores()
  registerDoParallel(cores)
  
  #Run the Simulator
  sims <- list()
  start.time <- Sys.time()
  Sims_Production <- foreach(ns = 1:nsim, .verbose = TRUE) %dopar% {
    
    #Load Libraries
    library(lubridate)
    
    #Load the Functions
    source("functions/Get_Energy_Droughts.R")
    
    
    #Get the production level
    Production <- list()
    for(i in 1:length(start_hours)) {
      Production[[i]] <- get_production_profile(Total_Supply = Total_Prod_Sims[[ns]],
                                                start_date = start_date,
                                                start_day = start_day,
                                                start_hour = start_hours[i],
                                                horizon = horizon,
                                                Type = start_hours[i])}
    sims[[ns]] <- Production
    
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  stopImplicitCluster()
  
  
  #Consolidate the simulations
  for(i in 1:nsim){
    Sims_Production[[i]] <- bind_rows(lapply(Sims_Production[[i]],data.frame))}
  Sims_Production <- bind_rows(lapply(Sims_Production,data.frame))
  Sims_Production$Type <- as.numeric(Sims_Production$Type)
  
  #Convert to Hours 
  hrs <- c("12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM",
           "6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM",
           "12 PM", "1 PM", "2 PM", "3 PM", "4 PM", "5 PM",
           "6 PM", "7 PM", "8 PM", "9 PM", "10 PM", "11 PM")
  Sims_Production$Hour <- hrs[Sims_Production$Type]
  
  #Basic Plot
  p1 <- ggplot(Sims_Production) +
    geom_violin(mapping = aes(y = Generation/1000, x = Type, group = Type), 
                col = 'white', fill = 'red') +
    geom_point(Data_Production, mapping = aes(x = Type, y = Generation/1000), 
               size = 3) +
    xlab("Contract Initiation Hour") +
    ylab ("Total Production (GWhr)") +
    ylim(0, 2.5e2) +
    ggtitle(paste0(plt_title)) +
    theme_bw() +
    theme(plot.title = element_text(size=18),
          axis.text=element_text(size=15),
          axis.title=element_text(size=15))
  
  return(p1)
  
}


#______________________________________________________________________________#
#FUNCTION WITH MULTIPLE HORIZONS AND SINGLE START TIME


get_horizon_plot <- function(Wind_Production,Solar_Production, 
                             Wind_Production_Sims,Solar_Production_Sims,
                             start_date, 
                             start_day,
                             start_hour,
                             horizons,
                             plt_title){
  
  #___________________________________________________________________#
  #-----------------------Total Production----------------------------#
  #Number of simulations
  nsim = length(Solar_Production_Sims)
  
  #Data
  Total_Prod <- Wind_Production + Solar_Production
  
  #Simulations
  Total_Prod_Sims <- list()
  for(i in 1:nsim){
    Total_Prod_Sims[[i]] = Wind_Production_Sims[[i]] + Solar_Production_Sims[[i]]
  }
  
  #Get the production level
  Data_Production <- list()
  for(i in 1:length(horizons)) {
    Data_Production[[i]] <- get_production_profile(Total_Supply = Total_Prod,
                                                   start_date = start_date,
                                                   start_day = start_day,
                                                   start_hour = start_hour,
                                                   horizon = horizons[i],
                                                   Type = horizons[i]) }
  Data_Production <- bind_rows(lapply(Data_Production,data.frame))
  
  
  
  #___________________________________________________________________#
  #-----------------------------Simulations---------------------------#
  
  #Start parallel processing
  cores=detectCores()
  registerDoParallel(cores)
  
  #Run the Simulator
  sims <- list()
  start.time <- Sys.time()
  Sims_Production <- foreach(ns = 1:nsim, .verbose = TRUE) %dopar% {
    
    #Load Libraries
    library(lubridate)
    
    #Load the Functions
    source("functions/Get_Energy_Droughts.R")

    #Get the production level
    Production <- list()
    for(i in 1:length(horizons)) {
      Production[[i]] <- get_production_profile(Total_Supply = Total_Prod_Sims[[ns]],
                                                start_date = start_date,
                                                start_day = start_day,
                                                start_hour = start_hour,
                                                horizon = horizons[i],
                                                Type = horizons[i])}
    sims[[ns]] <- Production
    
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  stopImplicitCluster()
  
  
  
  #Consolidate the simulations
  for(i in 1:nsim){
    Sims_Production[[i]] <- bind_rows(lapply(Sims_Production[[i]],data.frame))}
  Sims_Production <- bind_rows(lapply(Sims_Production,data.frame))
  Sims_Production$Type <- as.numeric(Sims_Production$Type)
  
  #Convert to Hours 
  hrs <- c("12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM",
           "6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM",
           "12 PM", "1 PM", "2 PM", "3 PM", "4 PM", "5 PM",
           "6 PM", "7 PM", "8 PM", "9 PM", "10 PM", "11 PM")
  Sims_Production$Hour <- hrs[Sims_Production$Type]
  
  #Basic Plot
  p1 <- ggplot(Sims_Production) +
    geom_violin(mapping = aes(y = Generation/1000, x = Type, group = Type), 
                col = 'white', fill = 'red') +
    geom_point(Data_Production, mapping = aes(x = Type, y = Generation/1000), 
               size = 3) +
    xlab("Contract Horizon (Hours)") +
    ylab ("Total Production (GWhr)") +
    ylim(0, 3.25e2) +
    ggtitle(paste0(plt_title)) +
    theme_bw() +
    theme(plot.title = element_text(size=18),
          axis.text=element_text(size=15),
          axis.title=element_text(size=15))
  
  return(p1)
  
}



#______________________________________________________________________________#
#-----------------------DEFUNCT FUNCTION---------------------------------------#

### Function to get the Energy Production Distribution Plot###


#Variables
#1. Data (Wind and Solar separate)
#2. Start Date #"01-01-1979 00:00"
#3. Start Day (DOY) #15
#4. Start Hour #8
#5. Time Horizon #12 hours
#6. Simulations #Consolidated


#Output
#Plot


get_ed_plot <- function(Data_Wind,Data_Solar, Simulations,
                        start_date, start_day,
                        start_hour, horizon){
  
  #___________________________________________________________________#
  #-----------------------------Data----------------------------------#
  #Wind
  for(i in 1:ncol(Data_Wind)){
    tx <- as.data.frame(Data_Wind[,i])
    Data_Wind[,i] <- apply(tx, 1, get_wind_CF)
  }
  Wind_Production <- rowSums(Data_Wind)
  
  ###Solar 
  Solar_Production <- rowSums(Data_Solar)/1000
  
  #Total Production
  Total_Prod <- Wind_Production + Solar_Production
  
  #Get the production level
  Data_Production <- get_production_profile(Total_Supply = Total_Prod,
                                            start_date = start_date,
                                            start_day = start_day,
                                            start_hour = start_hour,
                                            horizon = horizon,
                                            Type = "Data")
  
  
  #___________________________________________________________________#
  #-----------------------------Simulations---------------------------#
  nsim <- length(Simulations)
  
  #Start parallel processing
  cores=detectCores()
  registerDoParallel(cores)
  
  #Run the Simulator
  start.time <- Sys.time()
  Sims_Production <- foreach(ns = 1:nsim, .verbose = TRUE) %dopar% {
    
    #Load Libraries
    library(lubridate)
    
    #Load the Functions
    source("functions/Get_Wind_Capacity_Factor.R")
    source("functions/Get_Energy_Droughts.R")
    
    ###Wind
    WT <-  Simulations[[ns]]$WPnew
    for(i in 1:ncol(WT)){
      tx <- as.data.frame(WT[,i])
      WT[,i] <- apply(tx, 1, get_wind_CF)
    }
    Wind_Production <- rowSums(WT)
    
    ###Solar 
    Solar_Production <- rowSums(Simulations[[ns]]$SSnew)/1000
    
    #Total Production
    Total_Prod <- Wind_Production + Solar_Production
    
    
    #Get the production level
    get_production_profile(Total_Supply = Total_Prod,
                           start_date = start_date,
                           start_day = start_day,
                           start_hour = start_hour,
                           horizon = horizon,
                           Type = "Sims")
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  stopImplicitCluster()
  
  #Delete the Simulations
  Simulations <- NULL
  
  
  #Consolidate the simulations
  Sims_Production <- bind_rows(lapply(Sims_Production,data.frame))
  #Total_Prod <-  rbind(Sims_Production,Data_Production)
  
  
  
  # Basic density plot in ggplot2
  p1 <- ggplot(Sims_Production, aes(x = Generation, colour = Type)) +
    geom_density() +
    geom_vline(xintercept = Data_Production$Generation) + 
    ggtitle(paste0("Energy Production Distribution \n Start Jun 15th ", start_hour,
                   ":00 AM \n Time Horizon - ",  horizon," hrs"))
  
  return(p1)
  
}

