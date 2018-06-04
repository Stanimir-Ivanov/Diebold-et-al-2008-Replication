load("./Data/macroeconomic_data_filtered.RData")

ext1 <- lapply(loc_f, function(x) {

  level <- lm(x$level ~ glob_f$level + lag(x$capacity.utilization) + 
                lag(x$inflation) + lag(x$interest.rate) + stats::lag(x$unemployment))
  level_ar1 <- lm(level$residuals ~ 0 + lag(level$residuals))
  
  slope <- lm(x$slope ~ glob_f$slope + lag(x$capacity.utilization) + 
                lag(x$inflation) + lag(x$interest.rate) + lag(x$unemployment))
  slope_ar1 <- lm(slope$residuals ~ 0 + lag(slope$residuals))
  
  curvature <- lm(x$curvature ~ glob_f$curvature + lag(x$capacity.utilization) + 
                    lag(x$inflation) + lag(x$interest.rate) + lag(x$unemployment))
  curvature_ar1 <- lm(curvature$residuals ~ 0 + lag(curvature$residuals))
                     
  return(list("level" = level, "slope" = slope, "curvature" = curvature,
              "level_ar1" = level_ar1, "slope_ar1" = slope_ar1, "curvature_ar1" = curvature_ar1))
})

remove(glob_f)
remove(loc_f)
save.image("./Data/arimax_results.RData")

# source("./LF Utils/generate_latent_factors.R")
