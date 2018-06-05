# detach("package:dplyr", unload = TRUE)
library(xts)
library(stats) # stats for ARIMA
library(MTS) # multivariate time series for VAR

load("./Data/latent_factor_data.RData")

eq5 <- VAR(glob_f, p = 1, fixed = diag(3), include.mean = FALSE, output = FALSE)

eq67 <- lapply(loc_f, function(x) {
  level <- arima(x$level, order = c(0, 0, 1), xreg = glob_f$level)
  slope <- arima(x$slope, order = c(0, 0, 1), xreg = glob_f$slope)
  curvature <- arima(x$curvature, order = c(0, 0, 1) , xreg = glob_f$curvature)
  return(list("level" = level, "slope" = slope, "curvature" = curvature))
})

eq67alt <- lapply(loc_f, function(x) {
  level <- lm(x$level ~ 1 + glob_f$level)
  level_ar1 <- lm(level$residuals ~ 0 + lag(level$residuals))
  
  slope <- lm(x$slope ~ 1 + glob_f$slope)
  slope_ar1 <- lm(slope$residuals ~ 0 + lag(slope$residuals))
  
  curvature <- lm(x$curvature ~ 1 + glob_f$curvature)
  curvature_ar1 <- lm(curvature$residuals ~ 0 + lag(curvature$residuals))
  
  return(list("level" = level, "slope" = slope, "curvature" = curvature, 
              "level_ar1" = level_ar1, "slope_ar1" = slope_ar1, "curvature_ar1" = curvature_ar1))
})

remove(glob_f)
remove(loc_f)
remove(loc_level)
remove(loc_slope)
remove(loc_curvature)

save.image("./Data/replication_results.RData")