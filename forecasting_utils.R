source("Utils/generate_latent_factors.R")

get_glob_f <- function(loc_f){
  loc_f <- group_latent_factors(loc_f)
  # perform PCA
  pr_level <- prcomp(loc_f$level, center = TRUE, scale. = TRUE)
  pr_slope <- prcomp(loc_f$slope, center = TRUE, scale. = TRUE)
  pr_curvature <- prcomp(loc_f$curvature, center = TRUE, scale. = TRUE)
  
  #extract global factors
  level <- as.xts(-pr_level$x[,"PC1"], order.by = time(loc_f$level))
  slope <- as.xts(-pr_slope$x[,"PC1"], order.by = time(loc_f$slope))
  curvature <- as.xts(-pr_curvature$x[,"PC1"], order.by = time(loc_f$curvature))  
  res <- cbind(level, slope, curvature)
  colnames(res) <- c("level", "slope", "curvature")
  return(res)
}

LambdaMat <- function(tau, lambda){
  return(rbind(1, factorBeta1(tau, lambda), factorBeta2(tau, lambda)))
}

eq5f <- function(glob_f) {
  return(VAR(glob_f, p = 1, fixed = diag(3), include.mean = FALSE, output = FALSE))
}

eq67f <- function(loc_f_window, glob_f) {
  return(lapply(loc_f_window, function(x) {
  level <- arima(x$level, order = c(0, 0, 1), xreg = glob_f$level)
  slope <- arima(x$slope, order = c(0, 0, 1), xreg = glob_f$slope)
  curvature <- arima(x$curvature, order = c(0, 0, 1) , xreg = glob_f$curvature)
  return(list("level" = level, "slope" = slope, "curvature" = curvature))
}))
}

ext1alt <- function(loc_f_window, macro, glob_f){
  return(mapply(function(x, y) {
    macro_reg <- cbind(y$capacity.utilization, y$inflation, y$interest.rate, y$unemployment)
    level_reg <- cbind(glob_f$level, macro_reg)
    level <- arima(x$level, order = c(0, 0, 1), xreg = level_reg)
    slope_reg <- cbind(glob_f$slope, macro_reg)
    slope <- arima(x$slope, order = c(0, 0, 1), xreg = slope_reg)
    curvature_reg <- cbind(glob_f$curvature, macro_reg)
    curvature <- arima(x$curvature, order = c(0, 0, 1) , xreg = curvature_reg)
    return(list("level" = level, "slope" = slope, "curvature" = curvature))
  }, x = loc_f_window, y = macro, SIMPLIFY = FALSE))
}

group_latent_factors <- function(loc_f){
  # group latent factors together
  loc_level <- cbind(loc_f$US$level, loc_f$CA$level, loc_f$JP$level, loc_f$DE$level, loc_f$UK$level)
  colnames(loc_level) <- c("US", "CA", "JP", "DE", "UK")
  loc_slope <- cbind(loc_f$US$slope, loc_f$CA$slope, loc_f$JP$slope, loc_f$DE$slope, loc_f$UK$slope)
  colnames(loc_slope) <- c("US", "CA", "JP", "DE", "UK")
  loc_curvature <- cbind(loc_f$US$curvature, loc_f$CA$curvature, loc_f$JP$curvature, loc_f$DE$curvature, loc_f$UK$curvature)
  colnames(loc_curvature) <- c("US", "CA", "JP", "DE", "UK")
  return(list("level" = loc_level, "slope" = loc_slope, "curvature" = loc_curvature))
}

get_ma_eq <- function(yield_curves_window, lambda){
  
  fit <- lapply(yield_curves_window, function(x){
    return(get_fit(x, lambda))
  })
  
  res <- lapply(yield_curves_window, function(x){x
    return(get_res(x, lambda) %>% stats::lag())
  })
  
  # omit first observation
  res <- lapply(res, function(x){
    return(x[2:length(time(x)),])
  })
  
  yield_curves_window <- lapply(yield_curves_window, function(x){
    return(x[2:length(time(x)),])
  })
  
  melted_yc <- melt_list(yield_curves_window)
  melted_fit <- melt_list(fit)
  melted_res <- melt_list(res)
  
  panel_data <- mapply(FUN = function(x, y, z){
    tmp <- merge(x, y, by = c("time.x.", "variable"))
    tmp <- merge(tmp, z, by = c("time.x.", "variable"))
    colnames(tmp) <- c("time", "maturity", "yield", "fit", "res")
    return(tmp)
  }, x = melted_yc, y = melted_fit, z = melted_res, SIMPLIFY = FALSE)
  
  
  ext2_pd <- lapply(panel_data, function(x){
    beta <- lm(yield ~ 0 + fit + res + factor(maturity), data = x)
    return(beta)
  })
  
  return(list(Ext2 = ext2_pd, Res = res))
}