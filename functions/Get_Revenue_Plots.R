#______________________________________________________________________________#
#FUNCTION TO PLOT THE REVENUE FOR EACH CONTRACT BID

#CONTRACT PARAMETERS
#1. Multiple Bid Values (4)
#2. Multiple Days (4) with a single initiation time (8:00 AM)
#3. Single Time Horizon (12 hours)
#4. Multiple Penlaty to Cost Ratios (3)


Data_Wind = WP
Data_Solar = ssrd
Simulations = ynew_results
start_date = "01-01-1979 00:00"
start_day = 15
start_hour = 8
horizon = 12
plt_title = "Jan 15"
pc_ratio = c(0.5,1,1.5)
bids <- c(1400,1600,1800)


get_revenue_plot <- function(Data_Wind,Data_Solar,
                             Simulations,
                             start_date, 
                             start_day,
                             start_hour,
                             horizons,
                             plt_title){
  
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
  
  #Delete the Simulations -- Data Storage
  #Simulations <- NULL
  
  
  #Consolidate the simulations
  Sims_Production <- bind_rows(lapply(Sims_Production,data.frame))
  
  
  ###COMPUTE THE REVENUE BASED ON THE BID 
  get_revenue <- function(bid,supply,cost,penalty){
    if(supply > bid) {revenue = bid*cost
    } else {
      revenue = supply*cost - (bid-supply)*cost*penalty
    }
    
    return(revenue)
  }
  
  
  ###Data Revenue
  Data_Revenue <- data.frame(Revenue = rep(NA, length(pc_ratio)*length(bids)),
                             Penalty = rep(NA, length(pc_ratio)*length(bids)),
                             Bids = rep(NA, length(pc_ratio)*length(bids)))
  t = 1
  for(i in 1:length(pc_ratio)){
    for(j in 1:length(bids)){
      Revenue = get_revenue(bid = bids[j],
                            supply = Data_Production$Generation,
                            cost = cost,
                            penalty = pc_ratio[i])
      Data_Revenue[t,] = c(Revenue, pc_ratio[i],bids[j])
      t=t+1
    }
  }
  
  Data_Revenue$Bids <- as.factor(Data_Revenue$Bids)
  Data_Revenue$Penalty <- as.factor(Data_Revenue$Penalty)
  
  ###Simulations Revenue
  Sims_Revenue <- list()
  for(ns in 1:nsim){
    
    ###Data Revenue
    Temp_Revenue <- data.frame(Revenue = rep(NA, length(pc_ratio)*length(bids)),
                               Penalty = rep(NA, length(pc_ratio)*length(bids)),
                               Bids = rep(NA, length(pc_ratio)*length(bids)))
    t = 1
    for(i in 1:length(pc_ratio)){
      for(j in 1:length(bids)){
        Revenue = get_revenue(bid = bids[j],
                              supply = Sims_Production$Generation[ns],
                              cost = cost,
                              penalty = pc_ratio[i])
        Temp_Revenue[t,] = c(Revenue, pc_ratio[i],bids[j])
        t=t+1
      }
    }
    
    Sims_Revenue[[ns]] <- Temp_Revenue
    
  }
  
  #Convert to Single Dataframe
  Sims_Revenue <- bind_rows(lapply(Sims_Revenue,data.frame))
  Sims_Revenue$Bids <- as.factor(Sims_Revenue$Bids)
  Sims_Revenue$Penalty <- as.factor(Sims_Revenue$Penalty)
  
  
  
  #Basic Plot
  p1 <- ggplot() +
    geom_boxplot(Sims_Revenue, mapping = aes(x=Bids, y = Revenue, color = Penalty)) +
    geom_point(Data_Revenue, mapping = aes(x = Bids, y = Revenue, fill = Penalty)) +
    xlab("Contract Horizon (Hours)") +
    ylab ("Total Production (MWhr)") +
    ggtitle(paste0(plt_title)) +
    theme_bw() +
    theme(plot.title = element_text(size=18),
          axis.text=element_text(size=15),
          axis.title=element_text(size=15))
  
  return(p1)
  
  
  ggplot() +
    geom_point(Data_Revenue, mapping = aes(x = factor(Bids), 
                                          y = Revenue,
                                          color = factor(Penalty))) +
    geom_boxplot(Sims_Revenue, mapping = aes(x=factor(Bids), 
                                             y = Revenue, 
                                             color = factor(Penalty))) +
    
 
  
}
