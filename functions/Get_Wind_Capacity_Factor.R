#______________________________________________________________________________#
######---------------Convert to Wind Capacity Factor--------------------###

#Convert the wind speed to wind capacity factor using 
#Wind Power Curve for a V90-2.0MW Vestas turbine


#Input
#1. Wind Speed @ 100 m in (m/s)


#Output
#1. Wind Capacity Factor (OR)



get_wind_CF <- function(WS){

  if(WS <= 3) {
    W_CF = 0
  } else if(WS < 12.5 & WS > 3) {
    W_CF = 634.228 - 1248.5*WS + 999.57*WS^2 - 426.224*WS^3 + 105.617*WS^4 - 15.4587*WS^5 + 1.3223*WS^6 - 0.0609186*WS^7 + 0.00116265*WS^8
  } else if(WS < 25 & WS >= 12.5) {
    W_CF <- 2000
  } else {
    W_CF <- 0
  }
  
  W_CF <- W_CF/2000
  return(W_CF)
  
  
}




