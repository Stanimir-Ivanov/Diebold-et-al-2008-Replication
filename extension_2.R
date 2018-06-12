load("./Data/grouped_yield_curves.RData")
source("./Utils/generate_latent_factors.R")
source("range.R")

fit <- lapply(yield_curves, function(x){
  return(get_fit(x, lambda))
})

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

melted_yc <- melt_list(yield_curves)
melted_fit <- melt_list(fit)
melted_res <- melt_list(res)

panel_data <- mapply(FUN = function(x, y, z){
  tmp <- merge(x, y, by = c("time.x.", "variable"))
  tmp <- merge(tmp, z, by = c("time.x.", "variable"))
  colnames(tmp) <- c("time", "maturity", "yield", "fit", "res")
  return(tmp)
}, x = melted_yc, y = melted_fit, z = melted_res, SIMPLIFY = FALSE)

# ext2 <- mapply(FUN = function(x, y){
#   tau <- colnames(x) %>% as.numeric()
#   colnames(x) = paste(colnames(x), "Yield Curve")
#   yc_res_data <- cbind(x, y)
#   return(get_lf_ma1(yc_res_data, tau, lambda))
# }, x = yield_curves, y = res, SIMPLIFY = FALSE)

ext2_pd <- lapply(panel_data, function(x){
  beta <- lm(yield ~ 0 + fit + res + factor(maturity), data = x)
  return(beta)
})

remove(yield_curves, res, fit)
remove(melted_yc, melted_res, melted_fit, panel_data)

save.image("./Data/extention2_results.RData")