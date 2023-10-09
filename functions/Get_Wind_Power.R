#______________________________________________________________________________#
#CODE TO GENERATE PLOTS FOR CONTRACT INITIATION and CONTRACT HORIZON


#Input
#1. Data (Wind and Solar separate)
#2. Wind Simulations
#3. Data Fields


#Output
#Plot

#Data_Field <- WP
#sims <- wind_sims


get_wind_power <- function(Data_Field, sims){
  
  #Load the Functions
  source("functions/Get_Wind_Capacity_Factor.R")
  
  #___________________________________________________________________#
  #-----------------------------Data----------------------------------#
  #Wind
  for(i in 1:ncol(Data_Field)){
    tx <- as.data.frame(Data_Field[,i])
    Data_Field[,i] <- apply(tx, 1, get_wind_CF)
  }
  Wind_Production <- rowSums(Data_Field)
  
  #___________________________________________________________________#
  #-----------------------------Sims----------------------------------#
  nsim <- length(sims)
  
  #Start parallel processing
  cores=detectCores()
  registerDoParallel(cores)
  
  #Run the Simulator
  wind_power <- list()
  start.time <- Sys.time()
  Sims_Production <- foreach(ns = 1:nsim, .verbose = TRUE) %dopar% {
    
    #Load the Functions
    source("functions/Get_Wind_Capacity_Factor.R")
    
    ###Wind
    WT <-  sims[[ns]]
    for(i in 1:ncol(WT)){
      tx <- as.data.frame(WT[,i])
      WT[,i] <- apply(tx, 1, get_wind_CF)
    }
    Production <- rowSums(WT)
    
    wind_power[[ns]] <- Production
    
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  stopImplicitCluster()
  
  #____________________________________________________________________________#
  #Get the Simulation Length
  n_sim <- length(sims)
  
  
  #PDF for th entire field.
  og_pdf <- density(Wind_Production, 
                    from = 0, 
                    to = (max(Wind_Production) + 0.5*sd(Wind_Production)))
  
  #PDF for the suimulations
  sim_pdf <- matrix(NA, ncol = n_sim, nrow = length(og_pdf$x)) #Storing the Simulated CDF's
  for(j in 1:n_sim){
    #Computing each CDF
    sim <- Sims_Production[[j]]
    pdf_sim <- density(sim, 
                       from = 0, 
                       to = (max(Wind_Production) + 0.5*sd(Wind_Production)))
    sim_pdf[,j] <- pdf_sim$y
  }
  
  
  #Getting the percentiles
  lower_percentile <- apply(sim_pdf, 1, function(x) quantile(x, probs=.05))
  upper_percentile <- apply(sim_pdf, 1, function(x) quantile(x, probs=.95))
  median_percentile <- apply(sim_pdf, 1, median)
  par(mfrow=c(1,1), mar = c(6,6,4,1))
  plot(og_pdf$x, og_pdf$y, type='l',col='red',
       lwd = 2, main = paste0("Simulated Wind Power for the entire field "), 
       xlab = "MWhr", ylab = "Density - f(x) ",
       ylim = c(0, 1.15*max(og_pdf$y)),
       cex.lab = 1.5)
  polygon(c(og_pdf$x,rev(og_pdf$x)),c(lower_percentile,rev(upper_percentile)),col="gray")
  lines(og_pdf$x, median_percentile, lwd = 2)
  lines(og_pdf$x, og_pdf$y, col='red', lwd = 2)
  legend('topright', 
         legend = c("Median Simulation Value", "Reanalysis Data",
                    "5 - 95 Percentile Range"),
         lty = 1, lwd = 3, cex = 1.15,
         col = c('black','red','grey'))
  
}