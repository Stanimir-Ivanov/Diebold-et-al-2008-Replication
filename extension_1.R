load("./Data/grouped_macro.RData")

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
save.image("./Data/extension_results.RData")
remove(ext1)

load("./Data/grouped_yield_curves.RData")
source("./Utils/generate_latent_factors.R")
source("range.R")

res <- lapply(yield_curves, function(x){
  return(get_res(yield_curve = x, lambda = lambda) %>% stats::lag())
})

# omit first observation
res <- lapply(res, function(x){
  return(x[2:length(time(x)),])
})

yield_curves <- lapply(yield_curves, function(x){
  return(x[2:length(time(x)),])
})

ext2 <- mapply(FUN = function(x, y){
  tau <- colnames(x) %>% as.numeric()
  colnames(x) = paste(colnames(x), "Yield Curve")
  yc_res_data <- cbind(x, y)
  return(get_lf_ma1(yc_res_data, tau, lambda))
}, x = yield_curves, y = res, SIMPLIFY = FALSE)
