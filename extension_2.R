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

remove(yield_curves)
remove(res)

save.image("./Data/extention2_results.RData")