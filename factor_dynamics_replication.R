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

remove(glob_f)
remove(loc_f)
remove(loc_level)
remove(loc_slope)
remove(loc_curvature)

save.image("./Data/factor_dynamics_data.RData")