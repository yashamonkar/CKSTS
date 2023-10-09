#______________________________________________________________________________#
#CODE TO GENERATE PLOTS FOR TOTAL PRODUCTION FOR ONE FIELD AT A TIME

#Sub-divided into wind generation, solar generation and plotting for memory purposes. 


#Variables
#1. Data (Wind or Solar)
#2. Simulations #Consolidated 
#3. Plot Title (Sub_Title Indicating the capacity type)
#4. Weights (Installed Wind and Solar Capacity Weights at each grid point)


#Output
#1. Aggregate Production for Data and Sims


#______________________________________________________________________________#
##-------------------------WIND----------------------------------------------###

get_total_production_wind <- function(Data_Wind, 
                                 Simulations,
                                 Wind_Weights){
  
  
  #___________________________________________________________________#
  #-----------------------------Data----------------------------------#
  #Wind
  Data_Wind <- data.frame(lapply(Data_Wind, get_wind_CF))
  Data_Wind <- t(Wind_Weights*t(Data_Wind))
  Wind_Production <- rowSums(Data_Wind)
  Data_Wind <- NULL
  
  #___________________________________________________________________#
  #-----------------------------Simulations---------------------------#
  nsim <- length(Simulations)
  Wind_Production_Sims <- list()
  
  for(ns in 1:nsim){
    
    print(ns)
    
    ###Wind
    WT <- data.frame(lapply(data.frame(Simulations[[ns]]), get_wind_CF))
    WT <- t(Wind_Weights*t(WT))
    Wind_Production_Sims[[ns]] <- rowSums(WT)
    
  }
  
  #Return the Data and Simulations Results
  out = list(Data=Wind_Production,Sims=Wind_Production_Sims)
  
}


#______________________________________________________________________________#
###--------------------------Solar---------------------------------------------#

get_total_production_solar <- function(Data_Solar, 
                                      Simulations,
                                      Solar_Weights){
  
  
  #___________________________________________________________________#
  #-----------------------------Data----------------------------------#
  ###Solar 
  Data_Solar <- t(Solar_Weights*t(Data_Solar))
  Solar_Production <- rowSums(Data_Solar)/1000
  Data_Solar <- NULL
  
  #___________________________________________________________________#
  #-----------------------------Simulations---------------------------#
  nsim <- length(Simulations)
  Solar_Production_Sims <- list()
  
  for(ns in 1:nsim){
    
    print(ns)
    
    ###Solar 
    ST <- t(Solar_Weights*t(Simulations[[ns]]))
    Solar_Production_Sims[[ns]] <- rowSums(ST)/1000
    
  }
  
  #Return the Data and Simulations Results
  out = list(Data=Solar_Production,Sims=Solar_Production_Sims)
  
}  
  

#______________________________________________________________________________#
###--------------------------Plots---------------------------------------------#
get_total_production_plots <- function(Wind_Production, Solar_Production, 
                                       Wind_Production_Sims, Solar_Production_Sims,
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
  
  #______________________________________________________________________________#
  #Wind
  tx <- Wind_Production
  og_pdf <- density(tx, from = 0, to = (max(tx) + 1.5*sd(tx)))
  
  sim_pdf <- matrix(NA, ncol = nsim, nrow = length(og_pdf$x)) #Storing the Simulated CDF's
  for(j in 1:nsim){
    #Computing each CDF
    sim <- Wind_Production_Sims[[j]]
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
    sim <- Solar_Production_Sims[[j]]
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
    sim <- Total_Prod_Sims[[j]]
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
  

