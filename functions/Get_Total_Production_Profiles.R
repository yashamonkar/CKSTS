#______________________________________________________________________________#
#CODE TO GENERATE PLOTS FOR TOTAL PRODUCTION FOR 
#1. Wind
#2. Solar
#3. Total


#Variables
#1. Data (Wind and Solar separate)
#6. Simulations #Consolidated 
#7. Plot Title (Sub_Title Indicating the capacity type)
#8. Weights (Installed Wind and Solar Capacity Weights at each grid point)


#Output
#Plot
#Wind_Weights <- rep(1, 216)
#Solar_Weights <- rep(1,216)

#______________________________________________________________________________#
#FUNCTION WITH SINGLE HORIZONS AND MULTIPLE START TIME

get_total_production <- function(Data_Wind,Data_Solar, 
                                Simulations,
                                plt_title,
                                Wind_Weights, Solar_Weights){
  
  
  #___________________________________________________________________#
  #-----------------------------Data----------------------------------#
  #Wind
  for(i in 1:ncol(Data_Wind)){
    tx <- as.data.frame(Data_Wind[,i])
    Data_Wind[,i] <- apply(tx, 1, get_wind_CF)
  }
  Data_Wind <- t(Wind_Weights*t(Data_Wind))
  Wind_Production <- rowSums(Data_Wind)
  Data_Wind <- NULL
  
  ###Solar 
  Data_Solar <- t(Solar_Weights*t(Data_Solar))
  Solar_Production <- rowSums(Data_Solar)/1000
  Data_Solar <- NULL
  
  #Total Production
  Total_Prod <- Wind_Production + Solar_Production
  
  #___________________________________________________________________#
  #-----------------------------Simulations---------------------------#
  nsim <- length(Simulations)
  
  #Start parallel processing
  cores=detectCores()
  registerDoParallel(cores)
  
  #Run the Simulator
  sims <- list()
  start.time <- Sys.time()
  Sims_Production <- foreach(ns = 1:nsim, .verbose = TRUE) %dopar% {
    
    #Load the Functions
    source("functions/Get_Wind_Capacity_Factor.R")
    
    ###Wind
    WT <-  Simulations[[ns]]$WPnew
    for(i in 1:ncol(WT)){
      tx <- as.data.frame(WT[,i])
      WT[,i] <- apply(tx, 1, get_wind_CF)
    }
    WT <- t(Wind_Weights*t(WT))
    Wind_Production <- rowSums(WT)
    
    ###Solar 
    ST <- t(Solar_Weights*t(Simulations[[ns]]$SSnew))
    Solar_Production <- rowSums(ST)/1000
    
    #Total Production
    Total_Prod <- Wind_Production + Solar_Production
    
    sims[[ns]] <- list(Wind_Production, Solar_Production, Total_Prod)
    
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  stopImplicitCluster()
  
  
  #______________________________________________________________________________#
  #Wind
  tx <- Wind_Production
  og_pdf <- density(tx, from = 0, to = (max(tx) + 1.5*sd(tx)))
  
  sim_pdf <- matrix(NA, ncol = nsim, nrow = length(og_pdf$x)) #Storing the Simulated CDF's
  for(j in 1:nsim){
    #Computing each CDF
    sim <- Sims_Production[[j]][[1]]
    pdf_sim <- density(sim, from = 0, to = (max(tx) + 1.5*sd(tx)))
    sim_pdf[,j] <- pdf_sim$y
  }
  
  #Getting the percentiles
  lower_percentile <- apply(sim_pdf, 1, function(x) quantile(x, probs=.05))
  upper_percentile <- apply(sim_pdf, 1, function(x) quantile(x, probs=.95))
  median_percentile <- apply(sim_pdf, 1, median)
  par(mfrow=c(1,1), mar = c(6,6,4,1))
  plot(og_pdf$x, og_pdf$y, type='l',col='red',
       lwd = 2, main = paste0("Total Hourly Production - Wind \n ", plt_title), 
       xlab = "MWhr", ylab = "Density - f(x) ",
       ylim = c(0, 1.05*max(og_pdf$y)),
       cex.lab = 1.5)
  polygon(c(og_pdf$x,rev(og_pdf$x)),c(lower_percentile,rev(upper_percentile)),
          col="gray")
  lines(og_pdf$x, median_percentile, lwd = 2)
  lines(og_pdf$x, og_pdf$y, col='red', lwd = 2)
  legend('topright', legend = c("Median Simulation Value", "Reanalysis Data", 
                                "5 - 95 Percentile Range"),
         lty = 1, col = c('black','red','grey'), lwd = 3, cex = 1.25)
  
  
  #______________________________________________________________________________#
  #Solar
  tx <- Solar_Production
  og_pdf <- density(tx, from = 0, to = (max(tx) + 1.5*sd(tx)))
  
  sim_pdf <- matrix(NA, ncol = nsim, nrow = length(og_pdf$x)) #Storing the Simulated CDF's
  for(j in 1:nsim){
    #Computing each CDF
    sim <- Sims_Production[[j]][[2]]
    pdf_sim <- density(sim, from = 0, to = (max(tx) + 1.5*sd(tx)))
    sim_pdf[,j] <- pdf_sim$y
  }
  
  #Getting the percentiles
  lower_percentile <- apply(sim_pdf, 1, function(x) quantile(x, probs=.05))
  upper_percentile <- apply(sim_pdf, 1, function(x) quantile(x, probs=.95))
  median_percentile <- apply(sim_pdf, 1, median)
  par(mfrow=c(1,1), mar = c(6,6,4,1))
  plot(og_pdf$x, og_pdf$y, type='l',col='red',
       lwd = 2, main = paste0("Total Hourly Production - Solar \n ", plt_title), 
       xlab = "MWhr", ylab = "Density - f(x) ",
       ylim = c(0, 1.05*max(og_pdf$y)),
       cex.lab = 1.5)
  polygon(c(og_pdf$x,rev(og_pdf$x)),c(lower_percentile,rev(upper_percentile)),
          col="gray")
  lines(og_pdf$x, median_percentile, lwd = 2)
  lines(og_pdf$x, og_pdf$y, col='red', lwd = 2)
  legend('topright', legend = c("Median Simulation Value", "Reanalysis Data", 
                                "5 - 95 Percentile Range"),
         lty = 1, col = c('black','red','grey'), lwd = 3, cex = 1.25)
  
  
  
  #______________________________________________________________________________#
  #Combined
  tx <- Total_Prod
  og_pdf <- density(tx, from = 0, to = (max(tx) + 1.5*sd(tx)))
  
  sim_pdf <- matrix(NA, ncol = nsim, nrow = length(og_pdf$x)) #Storing the Simulated CDF's
  for(j in 1:nsim){
    #Computing each CDF
    sim <- Sims_Production[[j]][[3]]
    pdf_sim <- density(sim, from = 0, to = (max(tx) + 1.5*sd(tx)))
    sim_pdf[,j] <- pdf_sim$y
  }
  
  #Getting the percentiles
  lower_percentile <- apply(sim_pdf, 1, function(x) quantile(x, probs=.05))
  upper_percentile <- apply(sim_pdf, 1, function(x) quantile(x, probs=.95))
  median_percentile <- apply(sim_pdf, 1, median)
  par(mfrow=c(1,1), mar = c(6,6,4,1))
  plot(og_pdf$x, og_pdf$y, type='l',col='red',
       lwd = 2, main = paste0("Total Hourly Production - Combined \n ", plt_title), 
       xlab = "MWhr", ylab = "Density - f(x) ",
       ylim = c(0, 1.05*max(og_pdf$y)),
       cex.lab = 1.5)
  polygon(c(og_pdf$x,rev(og_pdf$x)),c(lower_percentile,rev(upper_percentile)),
          col="gray")
  lines(og_pdf$x, median_percentile, lwd = 2)
  lines(og_pdf$x, og_pdf$y, col='red', lwd = 2)
  legend('topright', legend = c("Median Simulation Value", "Reanalysis Data", 
                                "5 - 95 Percentile Range"),
         lty = 1, col = c('black','red','grey'), lwd = 3, cex = 1.25)
  
  
  
}

