## I  ################################################################################################
library(forecast)
library(MTS)
library(zoo)
library(stats)
load("./Data/grouped_yield_curves.RData")
load("./Data/grouped_macro.RData")
source("./Utils/generate_latent_factors.R")
source("range.R")
source("./forecasting_utils.R")
size <- 36
macro <- lapply(loc_f, stats::lag)
tau <- as.numeric(colnames(yield_curves$US))
lambdaMatrix <- LambdaMat(tau, lambda)
macro_switch <- TRUE
ma_switch <- TRUE
y_hat <- lapply(yield_curves, function(x){
  tmp <- as.xts(t(rep(NA, length(tau))), order.by = time(x[size]))
  colnames(tmp) <- tau
  return(tmp)
})

yield_curves <- lapply(yield_curves, function(x){
  return(rbind(x, as.xts(t(rep(NA, length(tau))), order.by = as.Date("1 Jan 2018", format = "%d %b %Y"))))
})

# forecaster <- function(yield_curves, macro, size, ma_switch, macro_switch){
#   
# }



for (i in size:length(time(yield_curves$US))) {
  yield_curves_window <- lapply(yield_curves, function(x){
    return(x[(i - size):i,])
  })
  
  loc_f_window <- lapply(yield_curves_window, function(x){
    res <- get_lf(yield_curve=x, lambda=lambda)
    return(res)
  })
  
  macro_window <- lapply(macro, function(x){
    return(x[(i - size):i,])
  })
  
  glob_f <- get_glob_f(loc_f_window)
  
  eq5 <- eq5f(glob_f)
  glob_f_hat <- as.xts(t(eq5$Phi %*% t(glob_f[length(time(glob_f)),])), order.by = time(yield_curves$US)[i + 1])
  colnames(glob_f_hat) <- c("level", "slope", "curvature")
  
  if(macro_switch){
    eq67 <- ext1alt(loc_f_window, macro_window, glob_f)
    x_reg <- lapply(macro, function(x){
      return(lapply(glob_f_hat, function(y){
        
        return(cbind(y, x$capacity.utilization[i + 1], x$inflation[i + 1], 
                     x$interest.rate[i + 1], x$unemployment[i + 1]))
        })
      )
    })
  } else {
    eq67 <- eq67f(loc_f_window, glob_f)
    x_reg <- lapply(yield_curves_window, function(x){
      return(glob_f_hat)
    })
  }
  loc_f_hat <- mapply(function(x, y){
    level_reg <- y$level
    level_hat <- predict(x$level, n.ahead = 1, newxreg = level_reg)$pred
    slope_reg <- y$slope
    slope_hat <- predict(x$slope, n.ahead = 1, newxreg = slope_reg)$pred
    curvature_reg <- y$curvature
    curvature_hat <- predict(x$curvature, n.ahead = 1, newxreg = curvature_reg)$pred
    return(xts(cbind(level_hat, slope_hat, curvature_hat), order.by = time(y$level)))
  }, x = eq67, y = x_reg, SIMPLIFY = FALSE)
  
  y_bar <- lapply(loc_f_hat, function(x){
    tmp <- as.xts(x %*% lambdaMatrix, order.by = time(x))
    colnames(tmp) <- tau
    return(tmp)
  })

  if(ma_switch){
    ma <- get_ma_eq(yield_curves_window, lambda)
    y_bar <- mapply(function(x, y, z){
      alpha_tau <- x$coefficients[grepl("factor", names(x$coefficients))]
      res_effect <- x$coefficients["res"] * y[size - 1]
      y_bar_effect <- x$coefficients["fit"] * z
      time(res_effect) <- time(z)
      tmp <- alpha_tau + y_bar_effect + res_effect
      colnames(tmp) <- tau
      return(tmp)
    }, x = ma$Ext2, y = ma$Res, z = y_bar, SIMPLIFY = FALSE)
  }
  
  
  print(i)
  y_hat <- mapply(function(x, y){
    return(rbind(x, y))
  }, x = y_hat, y = y_bar, SIMPLIFY = FALSE)
  
}
