#______________________________________________________________________________#
######---------------Energy Droughts for the hourly level--------------------###


#Input
#1. Data (Single Time Series)
#2. Simulations (One series per simulation)
#3. Start Date #start_date = "01-01-1979 00:00"
#4. Date of start of delivery period
#5. Hour of start of delivery period
#6. Planning Horizon (hours)

#sel_doy <- 15 #Jan 15th
#sel_hr <- 8 #8:00 AM
#horizon <- 12 #12 hour horizon


#Load the Functions
#source("functions/Get_Wind_Capacity_Factor.R")

####Custom Functions

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
close_ind <- function(curr,max,window_lag, window_front){
  if((curr-window_lag) < 1){
    
    indx <- c(tail(1:max,(1+abs(curr-window_lag))),
              1:(curr+window_front))
    
  } else if((curr+window_front) > max) {
    
    indx <- c(((curr-window_lag):max),
              (1:((curr+window_front)-max)))
    
  } else {
    
    indx <- (curr-window_lag):(curr+window_front)
    
  }
  return(indx)
}

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
get_production_profile <- function(Total_Supply, start_date, start_day,
                                   start_hour, horizon, Type){
  
  
  #Get the dates
  st_date <- as.POSIXct(start_date, format = "%m-%d-%Y %H:%M")
  time_stamps <- seq(st_date, by = "hour", length.out = length(Total_Supply))
  day_index <- yday(time_stamps)
  hour_index <- as.numeric(format(time_stamps,'%H')) + 1  #Hours from 1-24. 
  
  
  #Get the power computed in the Data
  sel_days <- close_ind(curr = start_day,
                        max = 366, 
                        window_lag =  0, 
                        window_front =  0)
  
  #Get all potential starting points
  sel <- day_index %in% sel_days & hour_index %in% start_hour
  sel <- which(sel == TRUE)
  
  #Get the profiles for all productions
  pot_prod <- tot_prod <- list()
  for(i in 1:length(sel)) {
    pot_prod[[i]] <- Total_Supply[sel[i]:(sel[i]+horizon)]
    tot_prod[[i]] <- sum(Total_Supply[sel[i]:(sel[i]+horizon)])
  }
  
  #Get the total production
  Tot_prod <- data.frame(Generation = unlist(tot_prod),
                         Type = Type)
  
  return(Tot_prod)
  
}



#______________________________________________________________________________#
#--------------------------Data----------------------------------------------###

###Wind
#Wind_Production <-  WP 
#
#for(i in 1:ncol(WP)){
#  tx <- as.data.frame(WP[,i])
#  Wind_Production[,i] <- apply(tx, 1, get_wind_CF)
#}
#Wind_Production <- rowSums(Wind_Production)

###Solar 
#Solar_Production <- rowSums(ssrd)/1000

#Total Production
#Total_Prod <- Wind_Production + Solar_Production


#Get the production level

#Data_Production <- get_production_profile(Total_Supply = Total_Prod,
#                                          start_date = "01-01-1979 00:00",
#                                          start_day = 15,
#                                          start_hour = 8,
#                                          horizon = 12,
#                                          Type = "Data")


#______________________________________________________________________________#
#--------------------------Simulations---------------------------------------###
#load("sims/Clust_Raw_Simulations.RData")
#nsim <- length(ynew_results)
#Sims_Production <- list()

#for(ns in 1:nsim){
#  
#  print(ns)


###Wind
#WT <-  ynew_results[[ns]]$WPnew
#Wind_Production <- WT 

#for(i in 1:ncol(WT)){
#  tx <- as.data.frame(WT[,i])
#  Wind_Production[,i] <- apply(tx, 1, get_wind_CF)
#}
#Wind_Production <- rowSums(Wind_Production)

###Solar 
#Solar_Production <- rowSums(ynew_results[[ns]]$SSnew)/1000

#Total Production
#Total_Prod <- Wind_Production + Solar_Production


#Get the production level

#Sims_Production[[ns]] <- get_production_profile(Total_Supply = Total_Prod,
#                                                start_date = "01-01-1979 00:00",
#                                                start_day = 15,
#                                                start_hour = 8,
#                                                horizon = 12,
#                                                Type = "Sims")
#}
#ynew_results <- NULL

#Consolidate the simulations
#Sims <- bind_rows(lapply(Sims_Production,data.frame))
#Total_Prod <-  rbind(Sims,Data_Production)



# Basic density plot in ggplot2
#ggplot(Sims, aes(x = Generation, colour = Type)) +
#  geom_density() +
#  geom_vline(xintercept = Data_Production$Generation) + 
#  ggtitle("Energy Production Distribution \n Start Jan 15th 8:00 AM \n Time Horizon - 12 hrs")
