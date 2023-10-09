#______________________________________________________________________________#
#Hybrid KNN Field Simulator
#Manu's Algorithm - Version for HARD CLUSTERING at each time step.

#Algorithm
# For each time step run KNN for each site. 
# Save the indices of the n nearest neighbors. 
# The inherent assumtion is the field has high correlation. 
# Compute the Resampling Algorithm based on all sites. 
# For each unique index, prob is summ(1/j), where j is the nneib number. 
# Cluster on the entire nearest neighbors. 
# KSTS measure on each cluster.



#Set-up Directory Path
setwd("~/GitHub/CKSTS") #Code for personal device


#______________________________________________________________________________#
#.libPaths("/rigel/cwc/users/yva2000/rpackages/") #Columbia Habanero Cluster.

###Load Packages
library(maps)       
library(dplyr)
library(ggplot2)
library(foreach)    #Parallel Execution
library(doParallel) #Backend to foreach
library(lubridate)

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



#______________________________________________________________________________#
#Subset (Optional)
#n <- 500 #Subset
#ssrd <- ssrd[1:n,]
#WP <- WP[1:n,]
ln <- c("Five_Year") #Current run name

#Select the field.
Fld <- as.matrix(cbind(WP,ssrd))
colnames(Fld) <- NULL


###Hyper-parameters
ngrids <- ncol(Fld)         #Number of grid points. Fields times Sites.
N_valid <- nrow(Fld)        #Number of Time Steps
n_site <- ncol(ssrd)        #Number of Sites in the Field. 

###Embeddings
max_embd <- 1  #Max Value of Lagged Embedding
sel_lags <- c(1) #Individual lags
n_lags <- length(sel_lags)
w <- c(1) #Weights for KNN


#______________________________________________________________________________#
#Functions

#Moving Window Indices - Returns the day/hour index of interest.
#Option for adding the lag for day and hour
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



#KNN - non parametric nearest neighbors resampling based on training and test data.
#Returns Index of Nearest neighbors
knn.sim.index <- function(x,xtest,nneib,weights){
  
  d <- matrix(NA, ncol=ncol(xtest), nrow=nrow(x))
  #Compute Distances
  for(i in 1:ncol(x))
    d[,i] <- weights[i]*(x[,i]-xtest[,i])^2
  d <- rowSums(d)
  sorted.data = sort.int(d,method="quick",index.return=TRUE)
  
  sorted.neib = matrix(sorted.data$ix[1:nneib])
  
  out = list(yknn=sorted.neib
  )
}


#Function to convert all unique neighbor indices to matrix format. 
#Input:-  Pre-Computed Neighbor Indices. 
#         Time Index          
#         
#Output:- Unique x Site Matrix

get_clust_matrix <- function(site_neigh){
  
  #Hyper-Parameters
  sites <- length(site_neigh)
  nneib <- length(site_neigh[[1]])
  
  #Kernel
  sj <- 1/(1:nneib)
  pj <- sj/sum(sj)
  
  #Unique Elements
  unq <- unique(unlist(site_neigh)) 
  clust_mat <- matrix(0, ncol = length(unq),
                      nrow = sites)
  
  for(st in 1:sites){
    for(j in 1:nneib){
      col <- which(unq == site_neigh[[st]][j])
      clust_mat[st,col] <- pj[j]
    }
  }
  
  return(clust_mat)
}


#Function to find optimi
#Input:-  Xt Clustering Matrix at time t. Contains all unique time indices. 
#         max_nc Max number of Clusters  
#         Method --- look into the NbClust documentation. 
#         Options - "ward.D", "ward.D2", "single", "complete", "average"
#Output:- Number of Clusters.
get_Clusters <- function(Xt, max_nc, method){
  
  library(NbClust)
  
  res<-NbClust(data = Xt, distance = "euclidean", 
               min.nc=2, max.nc=max_nc, 
               method = method, index = "ch")
  
  return(res$Best.partition)
}


##Function to take in Cluster Membership and output the best option for each cluster. 
##Input - 1. Clust_Mem - Cluster-Membership. Length - 432
#         2. N_Index - Nearest Neighbors. Length - 432
#         3. Current Cluster Number. Goes from 1 to max(cls)
##Output - Nearest Neighbor for the cluster.

get_clust_nn <- function(N_Index, Clust_Mem, Clust_ID){
  
  #Hyper-Parameters
  nneib <- length(N_Index[[1]])
  
  #Get_Members
  members <- which(Clust_Mem == Clust_ID)
  
  #Subset to cluster members
  n_ind <- N_Index[members]
  
  #Computing the Resampling Probability
  un_index <- unique(unlist(n_ind))
  un_prob <- rep(NA, length(un_index))
  
  n_ind  <-  matrix(unlist(n_ind), nrow=nneib)
  
  for(k in 1:length(un_index)){
    temp <- which(n_ind == un_index[k]) %% nneib
    temp[temp == 0] <- nneib
    un_prob[k] <- sum(1/temp)
  }
  
  
  #Computing the Resampling Probablites 
  pj <- data.frame(cbind(un_prob,un_index)) 
  thresh <- apply(pj, 2, FUN = function(x) tail(sort(x), nneib+1)[1])[1]
  pj <- pj %>% filter(un_prob > thresh)  
  pj$un_prob <- pj$un_prob/sum(pj$un_prob)
  ns <- sample(pj$un_index,1,prob = pj$un_prob)
  
  out = list(ns=ns,members=members)
}

#################################################################################################
#Knn with embeddings without climate 
knngrids.noclim <- function(Fld,ngrids, N_valid, #Data Parameters
                            nneib,weights, #KNN Parameter
                            start_date, #Date Seasonality Parameters
                            day_mv,hour_mv,
                            max_cls, cls_mtd,  #Clustering Parameters
                            max_embd, sel_lags, n_lags) #Embedding Parameters
{ library(dplyr)
  library(lubridate)
  
  #Day Indices
  st_date <- as.POSIXct(start_date, format = "%m-%d-%Y %H:%M")
  time_stamps <- seq(st_date, by = "hour", length.out = N_valid)
  day_index <- yday(time_stamps)
  hour_index <- as.numeric(format(time_stamps,'%H')) + 1  #Hours from 1-24. 
  
  #Setting up Storage for Simulations
  Xnew = matrix(NA,nr=N_valid,nc=ngrids)
  for(i in 1:max_embd){
    Xnew[i,] = jitter(Fld[i,]) 
  }
  
  #Creating the feature Vector
  X = array(NA, c(N_valid-max_embd,n_lags,ngrids))
  Y = array(NA, c(N_valid-max_embd,1,ngrids))
  for (j in 1:ngrids) 
  {
    #Get Lagged Structure Upto Max Embedding
    x_fld <- embed(Fld[,j],max_embd+1) 
    
    X[,,j] <- x_fld[,sel_lags+1]
    Y[,,j] <- x_fld[,1]
  }
  
  
  sel_clust <- max_clust <- min_clust <- hrs <- list() #Store the selected number of clusters
  
  pb = txtProgressBar(min = (max_embd+1), max = N_valid, initial = 1) 
  for (i in (max_embd+1):N_valid){
    
    setTxtProgressBar(pb,i)
    
    nn_index <- list() #Store all the nearest neighbours.
    day <- day_index[i]
    hour <- hour_index[i]
    sel_days <- close_ind(day,max = 366, window_lag =  day_mv[1], window_front =  day_mv[2])
    sel_hour <- close_ind(hour,max = 24,  window_lag =  hour_mv[1], window_front =  hour_mv[2])
    
    #Subset to the moving window
    indx <- day_index
    #indx[i] <- 999  #Remove the current value
    days <- tail(indx, -max_embd)
    hours <- tail(hour_index, -max_embd)
    
    sel <- days %in% sel_days & hours %in% sel_hour
    X_t <- X[sel,,]
    Y_t <- Y[sel,,]
    
    #Setting the Test Parameters
    sel_pars <- i-sel_lags
    
    for (j in 1:ngrids){
      
      #Setting the Test Parameters
      xtest <- matrix(Xnew[sel_pars,j], nrow =1)
      
      #Running the Simulator
      nn_index[[j]] <- knn.sim.index(as.matrix(X_t[,j]),xtest,nneib,weights)$yknn
    }
    
    #Converting to a usable Matrix form. 
    clust_mat <- get_clust_matrix(nn_index)
    
    #Find the best cluster partition
    cls <- get_Clusters(Xt = clust_mat, max_nc = max_cls,
                        method = cls_mtd)
    if(length(cls)==0){
      cls <- rep(1, ngrids)
      
    }
    
    #Find the Cluster Members
    for(jk in 1:max(cls)){
      
      cls_nn <- get_clust_nn(N_Index = nn_index,
                             Clust_Mem = cls,
                             Clust_ID = jk)
      Xnew[i,cls_nn$members] <- Y_t[cls_nn$ns,cls_nn$members]
    }
    
    #Save the selected clusters
    sel_clust[[i]] <- max(cls)
    max_clust[[i]] <- max(table(cls))
    min_clust[[i]] <- min(table(cls))
    hrs[[i]] <- hour
    
  }
  
  WPnew = Xnew[,1:n_site]
  SSnew = Xnew[,(n_site+1):ncol(Xnew)]
  Clust <- data.frame(Num = unlist(sel_clust),
                      Max_clust = unlist(max_clust),
                      Min_clust = unlist(min_clust),
                      Hour = unlist(hrs))
  
  out = list(WPnew=WPnew,SSnew=SSnew, Clust = Clust)
}


#______________________________________________________________________________#
#Running the Simulationnsample <- nsamples[ns]
source("functions/Get_Simulation_Skill.R")
source("functions/Get_PCWavelet_Sim_Skill.R")
source("functions/Get_Annual_Cycle.R")
source("functions/Get_Site_Correlation.R")
source("functions/Get_PCA_ggplot.R")
source("functions/Get_Seasonal_Correlation.R")
source("functions/Get_Wind_PDF.R")
source("functions/Get_Wind_Power.R")

#Set the Hyper-Parameters
nneib <- 7
nsim <- 48


#Compute and Store the Nearest Neighbors  
cores=detectCores()
registerDoParallel(cores)

#Run the Simulator
ysims <- list()
start.time <- Sys.time()
ynew_results <- foreach(m = 1:nsim, .verbose = TRUE) %dopar% {
  ysims[[m]] <- knngrids.noclim(Fld,ngrids, N_valid,
                                nneib,weights = w, 
                                start_date = "01-01-2018 00:00",
                                day_mv = c(15,15),hour_mv = c(0,0),
                                max_cls = 10, cls_mtd = "ward.D2",
                                max_embd, sel_lags, n_lags)
  }
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
stopImplicitCluster()

#Saving Raw Simulations
save(ynew_results, file = paste0("sims/Clust_Raw_Simulations_5_yrs.RData"))

#Saving Simulation Skill
pdf(paste0("figures/Clust_Joint_",ln,".pdf"))


###Seperating the Simulations
wind_sims <- solar_sims <- clust_num <- list()
for(i in 1:nsim){
  wind_sims[[i]] <- ynew_results[[i]]$WPnew
  solar_sims[[i]] <- ynew_results[[i]]$SSnew
  clust_num[[i]] <- ynew_results[[i]]$Clust
  
}

ynew_results<- NA


#------------------------------------------------------------------------------#
#####Cluster Visualization####


###Single Cluster Run###
sel <- sample(length(clust_num), 1, replace = FALSE)

par(mfrow = c(2,2), mar = c(4,4,4,2))
plot(clust_num[[sel]][,1], type = 'l',
     main = "Selected Number of Clusters \n each time step",
     ylab = "Number of Clusters", xlab = "Hour Index")
mtext("Single Simulation Run")

plot(clust_num[[sel]][,2], type = 'l', ylim = c(0,432),
     main = "Maximum Number of Members \n in a Cluster",
     ylab = "Number of Sites", xlab = "Hour Index")
abline(h = 216, col = 'red', lwd = 0.5, lty = 2)
mtext("Single Simulation Run")

plot(clust_num[[sel]][,3], type = 'l', ylim = c(0,216),
     main = "Minimum Number of Members in a Cluster",
     ylab = "Number of Sites", xlab = "Hour Index")
mtext("Single Simulation Run")
abline(h = 216, col = 'red', lwd = 0.5, lty = 2)



###Boxplots for Hourly Distribution of Cluster Numbers, Min and Max
clust_num <- bind_rows(clust_num)
clust_num$Hour <- as.factor(clust_num$Hour)

##Plotting Number of Clusters per Hour
ggplot(clust_num, aes(x = factor(Hour), fill = factor(Num))) +
  geom_bar(position = position_dodge(width = 0.8)) +
  ylab("Count Occurrence") +
  xlab("Hour of the Day") + 
  ggtitle("Distribution of the Number of Clusters") 

#Plots by the Number of Clusters
ggplot(clust_num, aes(Hour, Max_clust)) +
  geom_hline(yintercept = 215) +
  geom_boxplot(outlier.shape = NA) +
  ylab("Maximum Number of Grids within Clusters") +
  xlab("Hour of the Day") + 
  ggtitle("Distribution of the Maximum Number of Cluster Members") 


ggplot(clust_num, aes(Hour, Min_clust)) +
  geom_hline(yintercept = 217) +
  geom_boxplot(outlier.shape = NA) +
  ylab("Minimum Number of Grids within Clusters") +
  xlab("Hour of the Day") + 
  ggtitle("Distribution of the Minimum Number of Cluster Members") 



#Plots by the Number of Clusters
ggplot(clust_num, aes(Hour, Max_clust)) +
  geom_hline(yintercept = 215) +
  geom_boxplot(aes(color = factor(Num)), outlier.shape = NA) +
  ylab("Maximum Number of Grids within Clusters") +
  xlab("Hour of the Day") + 
  ggtitle("Distribution of the Maximum Number of Cluster Members") 


ggplot(clust_num, aes(Hour, Min_clust)) +
  geom_hline(yintercept = 217) +
  geom_boxplot(aes(color = factor(Num)), outlier.shape = NA) +
  ylab("Minimum Number of Grids within Clusters") +
  xlab("Hour of the Day") + 
    ggtitle("Distribution of the Minimum Number of Cluster Members") 

#------------------------------------------------------------------------------# 
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
               Resolution = "hourly",
               Start_Date = "01-01-1979 00:00")

get_pca_plot(X = fld, 
             Grid = grid_locs, 
             Field = Field,
             Sims = wind_sims)


get_wind_power(Data_Field = fld,
                sims = wind_sims)


#------------------------------------------------------------------------------#
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
                 Resolution = "hourly",
                 Start_Date = "01-01-1979 00:00")

get_pca_plot(X = fld, 
             Grid = grid_locs, 
             Field = Field,
             Sims = solar_sims)

#------------------------------------------------------------------------------#
###Cross - Correlation across Sites. 
Get_Site_Correlation(Fld1 = Fld[,1:n_site], #Wind
                     Fld2 = Fld[,(n_site+1):ncol(Fld)], #Solar
                     Fld1_Sims = wind_sims,
                     Fld2_Sims = solar_sims, 
                     Grid = grid_locs)


Get_Seasonal_Correlation(Fld1 = Fld[,1:n_site], #Wind
                         Fld2 = Fld[,(n_site+1):ncol(Fld)], #Solar
                         Fld1_Sims = wind_sims,
                         Fld2_Sims = solar_sims, 
                         Grid = grid_locs,
                         start_date = "01-01-1979 00:00",
                         col_hx = "#af8dc3")



dev.off()
