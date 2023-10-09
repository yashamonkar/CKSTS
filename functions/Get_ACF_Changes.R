#______________________________________________________________________________#
#Function to view ACF at different times of the day and seasons.

#Input
#1. Single Data Field
#2. Start-Date
#3. Site Number
#4. Field - Name of the Field Wind or Solar


#Output
#1. Single 3x3 plot with acf's via violin plot. 

Get_ACF_Changes <- function(X,
                            start_date, 
                            Site_Number,
                            Field)
{
  #Load Dependencies
  library(ggplot2)
  library(lubridate)
  library(dplyr)
  library(gridExtra)
  
  #Convert to Matrix Format
  X <- data.frame(Site = X)
  
  #Get the Time Index
  st_date <- as.POSIXct(start_date, format = "%m-%d-%Y %H:%M")
  time_stamps <- seq(st_date, by = "hour", length.out = nrow(X))
  X$Hour <- as.numeric(format(time_stamps,'%H'))
  X$Month <- as.numeric(format(time_stamps,'%m'))
  
  
  #______________________________________________________________________________#
  ####Dec-Jan-Feb and 00:00
  
  #Select the Hour
  t <- 0

  x1 <- which(X$Hour == t & X$Month == 12)
  x1 <- head(x1,-1)
  x2 <- which(X$Hour == t & X$Month == 1)
  x3 <- which(X$Hour ==t & X$Month == 2)
  x <- c(x1,x2,x3)
  
  #Making the Ggplot
  consolid_sims <- list()
  consolid_points <- list()
  
  for(j in 1:length(x)) {
    temp <- X$Site[x[j]:(x[j]+8)]
    temp <- temp + rnorm(length(temp),0,0.0001)
    t_acf <- acf(temp, plot=FALSE)
    consolid_sims[[j]] <- t_acf$acf
    consolid_points[[j]] <- t_acf$lag
  }
  consolid_points <- unlist(consolid_points)
  consolid_sims <- unlist(consolid_sims)
  consolid_df <- data.frame(x=consolid_points,y=consolid_sims)
  consolid_df$x <- cut(consolid_df$x, breaks = seq(-1,10,1))
  mid_points <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", consolid_df$x) ),
                      upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", consolid_df$x) ))
  consolid_df$x <- rowMeans(mid_points)
  
  og_acf <- acf(temp, plot = FALSE)
  og_df <- data.frame(x1=og_acf$lag, y1= og_acf$acf)
  
  p1 <- ggplot(og_df, aes(x=x1,y=y1))+
    geom_point(size=0)+
    scale_x_continuous(limits=c(-1,10)) +
    geom_boxplot(consolid_df, mapping = aes(y = y, x = x+0.5,group = x), outlier.shape = NA,outlier.colour = NA)+ 
    ggtitle(paste0(Field, " Site - ", Site_Number, "  DJF 00:00" )) +
    ylab("Correlation") +
    xlab(paste0("Lags")) +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  #------------------------------------------------------------------------------#
  #Select the Hour
  t <- 8
  
  x1 <- which(X$Hour == t & X$Month == 12)
  x1 <- head(x1,-1)
  x2 <- which(X$Hour == t & X$Month == 1)
  x3 <- which(X$Hour ==t & X$Month == 2)
  x <- c(x1,x2,x3)
  
  #Making the Ggplot
  consolid_sims <- list()
  consolid_points <- list()
  
  for(j in 1:length(x)) {
    temp <- X$Site[x[j]:(x[j]+8)]
    temp <- temp + rnorm(length(temp),0,0.0001)
    t_acf <- acf(temp, plot=FALSE)
    consolid_sims[[j]] <- t_acf$acf
    consolid_points[[j]] <- t_acf$lag
  }
  consolid_points <- unlist(consolid_points)
  consolid_sims <- unlist(consolid_sims)
  consolid_df <- data.frame(x=consolid_points,y=consolid_sims)
  consolid_df$x <- cut(consolid_df$x, breaks = seq(-1,10,1))
  mid_points <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", consolid_df$x) ),
                      upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", consolid_df$x) ))
  consolid_df$x <- rowMeans(mid_points)
  
  og_acf <- acf(temp, plot = FALSE)
  og_df <- data.frame(x1=og_acf$lag, y1= og_acf$acf)
  
  p2 <- ggplot(og_df, aes(x=x1,y=y1))+
    geom_point(size=0)+
    scale_x_continuous(limits=c(-1,10)) +
    geom_boxplot(consolid_df, mapping = aes(y = y, x = x+0.5,group = x), outlier.shape = NA,outlier.colour = NA)+ 
    ggtitle(paste0(Field, " Site - ", Site_Number, "  DJF 08:00" )) +
    ylab("Correlation") +
    xlab(paste0("Lags")) +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  #------------------------------------------------------------------------------#
  #Select the Hour
  t <- 16
  
  x1 <- which(X$Hour == t & X$Month == 12)
  x1 <- head(x1,-1)
  x2 <- which(X$Hour == t & X$Month == 1)
  x3 <- which(X$Hour ==t & X$Month == 2)
  x <- c(x1,x2,x3)
  
  #Making the Ggplot
  consolid_sims <- list()
  consolid_points <- list()
  
  for(j in 1:length(x)) {
    temp <- X$Site[x[j]:(x[j]+8)]
    temp <- temp + rnorm(length(temp),0,0.0001)
    t_acf <- acf(temp, plot=FALSE)
    consolid_sims[[j]] <- t_acf$acf
    consolid_points[[j]] <- t_acf$lag
  }
  consolid_points <- unlist(consolid_points)
  consolid_sims <- unlist(consolid_sims)
  consolid_df <- data.frame(x=consolid_points,y=consolid_sims)
  consolid_df$x <- cut(consolid_df$x, breaks = seq(-1,10,1))
  mid_points <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", consolid_df$x) ),
                      upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", consolid_df$x) ))
  consolid_df$x <- rowMeans(mid_points)
  
  og_acf <- acf(temp, plot = FALSE)
  og_df <- data.frame(x1=og_acf$lag, y1= og_acf$acf)
  
  p3 <- ggplot(og_df, aes(x=x1,y=y1))+
    geom_point(size=0)+
    scale_x_continuous(limits=c(-1,10)) +
    geom_boxplot(consolid_df, mapping = aes(y = y, x = x+0.5,group = x), outlier.shape = NA,outlier.colour = NA)+ 
    ggtitle(paste0(Field, " Site - ", Site_Number, "  DJF 16:00" )) +
    ylab("Correlation") +
    xlab(paste0("Lags")) +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  
  
  #______________________________________________________________________________#
  ####March-April-May and 00:00
  
  #Select the Hour
  t <- 0
  
  x1 <- which(X$Hour == t & X$Month == 3)
  x2 <- which(X$Hour == t & X$Month == 4)
  x3 <- which(X$Hour ==t & X$Month == 5)
  x <- c(x1,x2,x3)
  
  #Making the Ggplot
  consolid_sims <- list()
  consolid_points <- list()
  
  for(j in 1:length(x)) {
    temp <- X$Site[x[j]:(x[j]+8)]
    temp <- temp + rnorm(length(temp),0,0.0001)
    t_acf <- acf(temp, plot=FALSE)
    consolid_sims[[j]] <- t_acf$acf
    consolid_points[[j]] <- t_acf$lag
  }
  consolid_points <- unlist(consolid_points)
  consolid_sims <- unlist(consolid_sims)
  consolid_df <- data.frame(x=consolid_points,y=consolid_sims)
  consolid_df$x <- cut(consolid_df$x, breaks = seq(-1,10,1))
  mid_points <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", consolid_df$x) ),
                      upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", consolid_df$x) ))
  consolid_df$x <- rowMeans(mid_points)
  
  og_acf <- acf(temp, plot = FALSE)
  og_df <- data.frame(x1=og_acf$lag, y1= og_acf$acf)
  
  p4 <- ggplot(og_df, aes(x=x1,y=y1))+
    geom_point(size=0)+
    scale_x_continuous(limits=c(-1,10)) +
    geom_boxplot(consolid_df, mapping = aes(y = y, x = x+0.5,group = x), outlier.shape = NA,outlier.colour = NA)+ 
    ggtitle(paste0(Field, " Site - ", Site_Number, "  MAM 00:00" )) +
    ylab("Correlation") +
    xlab(paste0("Lags")) +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  #------------------------------------------------------------------------------#
  #Select the Hour
  t <- 8
  
  x1 <- which(X$Hour == t & X$Month == 3)
  x2 <- which(X$Hour == t & X$Month == 4)
  x3 <- which(X$Hour ==t & X$Month == 5)
  x <- c(x1,x2,x3)
  
  #Making the Ggplot
  consolid_sims <- list()
  consolid_points <- list()
  
  for(j in 1:length(x)) {
    temp <- X$Site[x[j]:(x[j]+8)]
    temp <- temp + rnorm(length(temp),0,0.0001)
    t_acf <- acf(temp, plot=FALSE)
    consolid_sims[[j]] <- t_acf$acf
    consolid_points[[j]] <- t_acf$lag
  }
  consolid_points <- unlist(consolid_points)
  consolid_sims <- unlist(consolid_sims)
  consolid_df <- data.frame(x=consolid_points,y=consolid_sims)
  consolid_df$x <- cut(consolid_df$x, breaks = seq(-1,10,1))
  mid_points <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", consolid_df$x) ),
                      upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", consolid_df$x) ))
  consolid_df$x <- rowMeans(mid_points)
  
  og_acf <- acf(temp, plot = FALSE)
  og_df <- data.frame(x1=og_acf$lag, y1= og_acf$acf)
  
  p5 <- ggplot(og_df, aes(x=x1,y=y1))+
    geom_point(size=0)+
    scale_x_continuous(limits=c(-1,10)) +
    geom_boxplot(consolid_df, mapping = aes(y = y, x = x+0.5,group = x), outlier.shape = NA,outlier.colour = NA)+ 
    ggtitle(paste0(Field, " Site - ", Site_Number, "  MAM 08:00" )) +
    ylab("Correlation") +
    xlab(paste0("Lags")) +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  #------------------------------------------------------------------------------#
  #Select the Hour
  t <- 16
  
  x1 <- which(X$Hour == t & X$Month == 3)
  x2 <- which(X$Hour == t & X$Month == 4)
  x3 <- which(X$Hour ==t & X$Month == 5)
  x <- c(x1,x2,x3)
  
  #Making the Ggplot
  consolid_sims <- list()
  consolid_points <- list()
  
  for(j in 1:length(x)) {
    temp <- X$Site[x[j]:(x[j]+8)]
    temp <- temp + rnorm(length(temp),0,0.0001)
    t_acf <- acf(temp, plot=FALSE)
    consolid_sims[[j]] <- t_acf$acf
    consolid_points[[j]] <- t_acf$lag
  }
  consolid_points <- unlist(consolid_points)
  consolid_sims <- unlist(consolid_sims)
  consolid_df <- data.frame(x=consolid_points,y=consolid_sims)
  consolid_df$x <- cut(consolid_df$x, breaks = seq(-1,10,1))
  mid_points <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", consolid_df$x) ),
                      upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", consolid_df$x) ))
  consolid_df$x <- rowMeans(mid_points)
  
  og_acf <- acf(temp, plot = FALSE)
  og_df <- data.frame(x1=og_acf$lag, y1= og_acf$acf)
  
  p6 <- ggplot(og_df, aes(x=x1,y=y1))+
    geom_point(size=0)+
    scale_x_continuous(limits=c(-1,10)) +
    geom_boxplot(consolid_df, mapping = aes(y = y, x = x+0.5,group = x), outlier.shape = NA,outlier.colour = NA)+ 
    ggtitle(paste0(Field, " Site - ", Site_Number, "  MAM 16:00" )) +
    ylab("Correlation") +
    xlab(paste0("Lags")) +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  #______________________________________________________________________________#
  ####June-July-Aug and 00:00
  
  #Select the Hour
  t <- 0
  
  x1 <- which(X$Hour == t & X$Month == 6)
  x2 <- which(X$Hour == t & X$Month == 7)
  x3 <- which(X$Hour ==t & X$Month == 8)
  x <- c(x1,x2,x3)
  
  #Making the Ggplot
  consolid_sims <- list()
  consolid_points <- list()
  
  for(j in 1:length(x)) {
    temp <- X$Site[x[j]:(x[j]+8)]
    temp <- temp + rnorm(length(temp),0,0.0001)
    t_acf <- acf(temp, plot=FALSE)
    consolid_sims[[j]] <- t_acf$acf
    consolid_points[[j]] <- t_acf$lag
  }
  consolid_points <- unlist(consolid_points)
  consolid_sims <- unlist(consolid_sims)
  consolid_df <- data.frame(x=consolid_points,y=consolid_sims)
  consolid_df$x <- cut(consolid_df$x, breaks = seq(-1,10,1))
  mid_points <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", consolid_df$x) ),
                      upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", consolid_df$x) ))
  consolid_df$x <- rowMeans(mid_points)
  
  og_acf <- acf(temp, plot = FALSE)
  og_df <- data.frame(x1=og_acf$lag, y1= og_acf$acf)
  
  p7 <- ggplot(og_df, aes(x=x1,y=y1))+
    geom_point(size=0)+
    scale_x_continuous(limits=c(-1,10)) +
    geom_boxplot(consolid_df, mapping = aes(y = y, x = x+0.5,group = x), outlier.shape = NA,outlier.colour = NA)+ 
    ggtitle(paste0(Field, " Site - ", Site_Number, "  JJA 00:00" )) +
    ylab("Correlation") +
    xlab(paste0("Lags")) +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  #------------------------------------------------------------------------------#
  #Select the Hour
  t <- 8
  
  x1 <- which(X$Hour == t & X$Month == 6)
  x2 <- which(X$Hour == t & X$Month == 7)
  x3 <- which(X$Hour ==t & X$Month == 8)
  x <- c(x1,x2,x3)
  
  #Making the Ggplot
  consolid_sims <- list()
  consolid_points <- list()
  
  for(j in 1:length(x)) {
    temp <- X$Site[x[j]:(x[j]+8)]
    temp <- temp + rnorm(length(temp),0,0.0001)
    t_acf <- acf(temp, plot=FALSE)
    consolid_sims[[j]] <- t_acf$acf
    consolid_points[[j]] <- t_acf$lag
  }
  consolid_points <- unlist(consolid_points)
  consolid_sims <- unlist(consolid_sims)
  consolid_df <- data.frame(x=consolid_points,y=consolid_sims)
  consolid_df$x <- cut(consolid_df$x, breaks = seq(-1,10,1))
  mid_points <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", consolid_df$x) ),
                      upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", consolid_df$x) ))
  consolid_df$x <- rowMeans(mid_points)
  
  og_acf <- acf(temp, plot = FALSE)
  og_df <- data.frame(x1=og_acf$lag, y1= og_acf$acf)
  
  p8 <- ggplot(og_df, aes(x=x1,y=y1))+
    geom_point(size=0)+
    scale_x_continuous(limits=c(-1,10)) +
    geom_boxplot(consolid_df, mapping = aes(y = y, x = x+0.5,group = x), outlier.shape = NA,outlier.colour = NA)+ 
    ggtitle(paste0(Field, " Site - ", Site_Number, "  JJA 08:00" )) +
    ylab("Correlation") +
    xlab(paste0("Lags")) +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  #------------------------------------------------------------------------------#
  #Select the Hour
  t <- 16
  
  x1 <- which(X$Hour == t & X$Month == 6)
  x2 <- which(X$Hour == t & X$Month == 7)
  x3 <- which(X$Hour ==t & X$Month == 8)
  x <- c(x1,x2,x3)
  
  #Making the Ggplot
  consolid_sims <- list()
  consolid_points <- list()
  
  for(j in 1:length(x)) {
    temp <- X$Site[x[j]:(x[j]+8)]
    temp <- temp + rnorm(length(temp),0,0.0001)
    t_acf <- acf(temp, plot=FALSE)
    consolid_sims[[j]] <- t_acf$acf
    consolid_points[[j]] <- t_acf$lag
  }
  consolid_points <- unlist(consolid_points)
  consolid_sims <- unlist(consolid_sims)
  consolid_df <- data.frame(x=consolid_points,y=consolid_sims)
  consolid_df$x <- cut(consolid_df$x, breaks = seq(-1,10,1))
  mid_points <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", consolid_df$x) ),
                      upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", consolid_df$x) ))
  consolid_df$x <- rowMeans(mid_points)
  
  og_acf <- acf(temp, plot = FALSE)
  og_df <- data.frame(x1=og_acf$lag, y1= og_acf$acf)
  
  p9 <- ggplot(og_df, aes(x=x1,y=y1))+
    geom_point(size=0)+
    scale_x_continuous(limits=c(-1,10)) +
    geom_boxplot(consolid_df, mapping = aes(y = y, x = x+0.5,group = x), outlier.shape = NA,outlier.colour = NA)+ 
    ggtitle(paste0(Field, " Site - ", Site_Number, "  JJA 16:00" )) +
    ylab("Correlation") +
    xlab(paste0("Lags")) +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  
  #______________________________________________________________________________#
  ####Sept-Oct-Nov and 00:00
  
  #Select the Hour
  t <- 0
  
  x1 <- which(X$Hour == t & X$Month == 9)
  x2 <- which(X$Hour == t & X$Month == 10)
  x3 <- which(X$Hour ==t & X$Month == 11)
  x <- c(x1,x2,x3)
  
  #Making the Ggplot
  consolid_sims <- list()
  consolid_points <- list()
  
  for(j in 1:length(x)) {
    temp <- X$Site[x[j]:(x[j]+8)]
    temp <- temp + rnorm(length(temp),0,0.0001)
    t_acf <- acf(temp, plot=FALSE)
    consolid_sims[[j]] <- t_acf$acf
    consolid_points[[j]] <- t_acf$lag
  }
  consolid_points <- unlist(consolid_points)
  consolid_sims <- unlist(consolid_sims)
  consolid_df <- data.frame(x=consolid_points,y=consolid_sims)
  consolid_df$x <- cut(consolid_df$x, breaks = seq(-1,10,1))
  mid_points <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", consolid_df$x) ),
                      upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", consolid_df$x) ))
  consolid_df$x <- rowMeans(mid_points)
  
  og_acf <- acf(temp, plot = FALSE)
  og_df <- data.frame(x1=og_acf$lag, y1= og_acf$acf)
  
  p10 <- ggplot(og_df, aes(x=x1,y=y1))+
    geom_point(size=0)+
    scale_x_continuous(limits=c(-1,10)) +
    geom_boxplot(consolid_df, mapping = aes(y = y, x = x+0.5,group = x), outlier.shape = NA,outlier.colour = NA)+ 
    ggtitle(paste0(Field, " Site - ", Site_Number, "  SON 00:00" )) +
    ylab("Correlation") +
    xlab(paste0("Lags")) +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  #------------------------------------------------------------------------------#
  #Select the Hour
  t <- 8
  
  x1 <- which(X$Hour == t & X$Month == 9)
  x2 <- which(X$Hour == t & X$Month == 10)
  x3 <- which(X$Hour ==t & X$Month == 11)
  x <- c(x1,x2,x3)
  
  #Making the Ggplot
  consolid_sims <- list()
  consolid_points <- list()
  
  for(j in 1:length(x)) {
    temp <- X$Site[x[j]:(x[j]+8)]
    temp <- temp + rnorm(length(temp),0,0.0001)
    t_acf <- acf(temp, plot=FALSE)
    consolid_sims[[j]] <- t_acf$acf
    consolid_points[[j]] <- t_acf$lag
  }
  consolid_points <- unlist(consolid_points)
  consolid_sims <- unlist(consolid_sims)
  consolid_df <- data.frame(x=consolid_points,y=consolid_sims)
  consolid_df$x <- cut(consolid_df$x, breaks = seq(-1,10,1))
  mid_points <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", consolid_df$x) ),
                      upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", consolid_df$x) ))
  consolid_df$x <- rowMeans(mid_points)
  
  og_acf <- acf(temp, plot = FALSE)
  og_df <- data.frame(x1=og_acf$lag, y1= og_acf$acf)
  
  p11 <- ggplot(og_df, aes(x=x1,y=y1))+
    geom_point(size=0)+
    scale_x_continuous(limits=c(-1,10)) +
    geom_boxplot(consolid_df, mapping = aes(y = y, x = x+0.5,group = x), outlier.shape = NA,outlier.colour = NA)+ 
    ggtitle(paste0(Field, " Site - ", Site_Number, "  SON 08:00" )) +
    ylab("Correlation") +
    xlab(paste0("Lags")) +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  #------------------------------------------------------------------------------#
  #Select the Hour
  t <- 16
  
  x1 <- which(X$Hour == t & X$Month == 9)
  x2 <- which(X$Hour == t & X$Month == 10)
  x3 <- which(X$Hour ==t & X$Month == 11)
  x <- c(x1,x2,x3)
  
  #Making the Ggplot
  consolid_sims <- list()
  consolid_points <- list()
  
  for(j in 1:length(x)) {
    temp <- X$Site[x[j]:(x[j]+8)]
    temp <- temp + rnorm(length(temp),0,0.0001)
    t_acf <- acf(temp, plot=FALSE)
    consolid_sims[[j]] <- t_acf$acf
    consolid_points[[j]] <- t_acf$lag
  }
  consolid_points <- unlist(consolid_points)
  consolid_sims <- unlist(consolid_sims)
  consolid_df <- data.frame(x=consolid_points,y=consolid_sims)
  consolid_df$x <- cut(consolid_df$x, breaks = seq(-1,10,1))
  mid_points <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", consolid_df$x) ),
                      upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", consolid_df$x) ))
  consolid_df$x <- rowMeans(mid_points)
  
  og_acf <- acf(temp, plot = FALSE)
  og_df <- data.frame(x1=og_acf$lag, y1= og_acf$acf)
  
  p12 <- ggplot(og_df, aes(x=x1,y=y1))+
    geom_point(size=0)+
    scale_x_continuous(limits=c(-1,10)) +
    geom_boxplot(consolid_df, mapping = aes(y = y, x = x+0.5,group = x), outlier.shape = NA,outlier.colour = NA)+ 
    ggtitle(paste0(Field, " Site - ", Site_Number, "  JJA 16:00" )) +
    ylab("Correlation") +
    xlab(paste0("Lags")) +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  
  
  grid.arrange(p1, p2, p3,
               p4, p5, p6,
               p7, p8, p9,
               p10, p11, p12,
               nrow = 4)
  
}