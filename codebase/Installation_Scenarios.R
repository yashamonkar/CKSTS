#______________________________________________________________________________#
###-------Plot of Uniform Capacity and Installed Capacity Scenario Texas-----###


#Set-up Directory Path
setwd("~/GitHub/CKSTS") #Code for personal device


#______________________________________________________________________________#
###Load Packages
library(maps)       
library(dplyr)
library(ggplot2)
library(foreach)    #Parallel Execution
library(doParallel) #Backend to foreach
library(lubridate)
library(gridExtra)
library(ggpattern)
library(cowplot)



#______________________________________________________________________________#
###Reading the Data
grid_locs <- read.csv("data/ERCOT_lat_lon_index_key.csv", 
                      header = TRUE, sep=",")


ercot_generators <- read.csv("data/ERCOT_Generators.csv",
                             header = TRUE, sep=",")


pdf("figures/Installation_Scenarios.pdf")

#______________________________________________________________________________#
#----------------------------Get Weights---------------------------------------#

#Wind
wind_gen <- ercot_generators %>% filter(PrimSource == "wind")
tx_wind_weights <- rep(0,216)
for(i in 1:nrow(wind_gen)){
  tx_wind_weights[wind_gen$lat_lon_index[i]] <- tx_wind_weights[wind_gen$lat_lon_index[i]] + wind_gen$Install_MW[i]
}


#Solar
solar_gen <- ercot_generators %>% filter(PrimSource == "solar")
tx_solar_weights <- rep(0,216)
for(i in 1:nrow(solar_gen)){
  tx_solar_weights[solar_gen$lat_lon_index[i]] <- tx_solar_weights[solar_gen$lat_lon_index[i]] + solar_gen$Install_MW[i]
}


Total_Cap <- sum(tx_solar_weights)+sum(tx_wind_weights)

###Load the general maps
world <- map_data("world")
us <- map_data("state")

#______________________________________________________________________________#
#----------------------------Uniform Capacity-------------------------------------#
Wind_Weights <- rep(sum(tx_wind_weights)/216, 216) #rep(Total_Cap/432, 216) #rep(1, 216)
Solar_Weights <- rep(sum(tx_solar_weights)/216, 216) #rep(Total_Cap/432, 216) #rep(1, 216)
Sce_title <- "Uniform Capacity"

#Wind
p1 <- ggplot() +
  geom_map(data=world, map=world,
           aes(x=long, y=lat, map_id=region),
           fill="#D3D3D3", color="#000000", size=0.15) +
  geom_map(data=us, map=us,
           aes(x=long, y=lat, map_id=region),
           fill="#D3D3D3", color="#000000", size=0.15) +
  geom_tile(data = grid_locs, aes(x= lon-360, y = lat, fill = Wind_Weights)) + 
  scale_fill_gradient2(limits = c(0, max(tx_wind_weights)),
                       midpoint=500,
                       low="blue", mid="yellow", high="red",
                       name = "Capacity (MW)") +
  labs(subtitle = paste0(Sce_title), y = " ", x = " ",
       title = paste0("  Wind - Total ", round(sum(Wind_Weights),0), " MW")) +
  scale_x_continuous(name = "lon", limits = c(-107, -92)) +
  scale_y_continuous(name = "lat", limits = c(25.5, 36.5)) +
  theme_bw() +
  theme(axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        legend.position = "bottom")


#Solar
p2 <- ggplot() +
  geom_map(data=world, map=world,
           aes(x=long, y=lat, map_id=region),
           fill="#D3D3D3", color="#000000", size=0.15) +
  geom_map(data=us, map=us,
           aes(x=long, y=lat, map_id=region),
           fill="#D3D3D3", color="#000000", size=0.15) +
  geom_tile(data = grid_locs, aes(x= lon-360, y = lat, fill = Solar_Weights)) + 
  scale_fill_gradient2(limits = c(0, max(tx_wind_weights)),
                       midpoint=500,
                       low="blue", mid="yellow", high="red",
                       name = "Capacity (MW)") +
  labs(subtitle = paste0(Sce_title), y = " ", x = " ",
       title = paste0("  Solar - Total ", round(sum(Solar_Weights),0)," MW")) +
  scale_x_continuous(name = "lon", limits = c(-107, -92)) +
  scale_y_continuous(name = "lat", limits = c(25.5, 36.5)) +
  theme_bw() +
  theme(axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        legend.position = "bottom")

#Combine the plots
p_total1 <- plot_grid(p1 + theme(legend.position="none"),
                     p2 + theme(legend.position="none"),
                     nrow =1,
                     labels = c('A', 'B'))

#legend_b <- get_legend(
#  p1 + 
#    guides(color = guide_legend(nrow = 1, override.aes = list(size=2))) +
#    theme(legend.position = "bottom")
#)

#pt1 <- plot_grid(p_total, legend_b, ncol = 1, rel_heights = c(1, .2))

#----------------------------Installed Capacity-------------------------------------#
Wind_Weights <- tx_wind_weights
Solar_Weights <- tx_solar_weights
Sce_title <- "Installed Capacity"



#Wind

#Subset values which are zero.
grids <- grid_locs
grids$Capacity <- Wind_Weights
grids <- grids[grids$Capacity>0,]


p3 <- ggplot() +
  geom_map(data=world, map=world,
           aes(x=long, y=lat, map_id=region),
           fill="#D3D3D3", color="#000000", size=0.15) +
  geom_map(data=us, map=us,
           aes(x=long, y=lat, map_id=region),
           fill="#D3D3D3", color="#000000", size=0.15) +
  geom_point(data = grid_locs, aes(x= lon-360, y = lat), size = 0.15) +
  geom_tile(data = grids, aes(x= lon-360, y = lat, fill = Capacity)) + 
  scale_fill_gradient2(limits = c(0, max(tx_wind_weights)),
                       midpoint=500,
                       low="blue", mid="yellow", high="red",
                       name = "Capacity (MW)") +
  labs(subtitle = paste0(Sce_title), y = " ", x = " ",
       title = paste0("  Wind - Total ", round(sum(Wind_Weights),0)," MW")) +
  scale_x_continuous(name = "lon", limits = c(-107, -92)) +
  scale_y_continuous(name = "lat", limits = c(25.5, 36.5)) +
  theme_bw() +
  theme(axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        legend.position = "bottom")


#Solar

#Subset values which are zero.
grids <- grid_locs
grids$Capacity <- Solar_Weights
grids <- grids[grids$Capacity>0,]

p4 <- ggplot() +
  geom_map(data=world, map=world,
           aes(x=long, y=lat, map_id=region),
           fill="#D3D3D3", color="#000000", size=0.15) +
  geom_map(data=us, map=us,
           aes(x=long, y=lat, map_id=region),
           fill="#D3D3D3", color="#000000", size=0.15) +
  geom_point(data = grid_locs, aes(x= lon-360, y = lat), size = 0.15) +
  geom_tile(data = grids, aes(x= lon-360, y = lat, fill = Capacity)) + 
  scale_fill_gradient2(limits = c(0, max(tx_wind_weights)),
                       midpoint=500,
                       low="blue", mid="yellow", high="red",
                       name = "Capacity (MW)") +
  labs(subtitle = paste0(Sce_title), y = " ", x = " ",
       title = paste0("  Solar - Total ", round(sum(Solar_Weights),0)," MW")) +
  scale_x_continuous(name = "lon", limits = c(-107, -92)) +
  scale_y_continuous(name = "lat", limits = c(25.5, 36.5)) +
  theme_bw() +
  theme(axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        legend.position = "bottom")




#Combine the plots
p_total2 <- plot_grid(p3 + theme(legend.position="none"),
                     p4 + theme(legend.position="none"),
                     nrow =1,
                     labels = c('C', 'D'))

legend_b <- get_legend(
  p3 + 
    guides(color = guide_legend(nrow = 1, override.aes = list(size=2))) +
    theme(legend.position = "bottom")
)

pt2 <- plot_grid(p_total1, p_total2, legend_b, ncol = 1, rel_heights = c(1,1, .2))

plot(pt2)


dev.off()
