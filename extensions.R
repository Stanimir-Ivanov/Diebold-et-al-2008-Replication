load("./Data/macroeconomic_data_filtered.RData")

ext <- lapply(loc_f, function(x) {
  level <- arima(x$level, 
                 order = c(0, 0, 1), 
                 xreg = cbind(glob_f$level, 
                             lag(x[,c("capacity.utilization", 
                                      "inflation", 
                                      "interest.rate", 
                                      "unemployment")])
                             )
                 )
  slope <- arima(x$slope, 
                 order = c(0, 0, 1), 
                 xreg = cbind(glob_f$slope,
                              lag(x[,c("capacity.utilization", 
                                       "inflation", 
                                       "interest.rate", 
                                       "unemployment")])
                              )
                 )
  curvature <- arima(x$curvature, 
                     order = c(0, 0, 1) , 
                     xreg = cbind(glob_f$curvature,
                                  lag(x[,c("capacity.utilization", 
                                           "inflation", 
                                           "interest.rate", 
                                           "unemployment")])
                                  )
                     )
  return(list("level" = level, "slope" = slope, "curvature" = curvature))
})