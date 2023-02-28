#FUNCTION TO PLOT THE PDF FOR THE SIMULATIONS, DATA and 40-yrs data. 

get__wind_pdf <- function(True_Data,
                          Sims){
  
  #____________________________________________________________________________#
  #Get the Simulation Length
  n_sim <- length(Sims)
  
  
  #PDF for th entire field.
  tx <- rowSums(True_Data)
  og_pdf <- density(tx, from = 0, to = (max(tx) + 0.5*sd(tx)))
  
  #PDF for the suimulations
  sim_pdf <- matrix(NA, ncol = n_sim, nrow = length(og_pdf$x)) #Storing the Simulated CDF's
  for(j in 1:n_sim){
    #Computing each CDF
    sim <- as.data.frame(Sims[[j]])
    sim <- rowSums(sim)
    pdf_sim <- density(sim, from = 0, to = (max(tx) + 0.5*sd(tx)))
    sim_pdf[,j] <- pdf_sim$y
  }
  
  #Getting the percentiles
  lower_percentile <- apply(sim_pdf, 1, function(x) quantile(x, probs=.05))
  upper_percentile <- apply(sim_pdf, 1, function(x) quantile(x, probs=.95))
  median_percentile <- apply(sim_pdf, 1, median)
  par(mfrow=c(1,1), mar = c(6,6,4,1))
  plot(og_pdf$x, og_pdf$y, type='l',col='red',
       lwd = 2, main = paste0("Simulated PDF for Entire Field - Wind "), 
       xlab = "Aggregated Variable (X)", ylab = "Density - f(x) ",
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