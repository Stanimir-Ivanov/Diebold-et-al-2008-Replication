included_mat <- c("6", "12", "60", "120")

autocorrelations <- lapply(yield_curves, function(x) {
  return(table.Autocorrelation(x[, included_mat], digits = 2, max.lag = 30))
})

descriptives <- lapply(yield_curves, function(x){
  return(psych::describe(x[, included_mat], skew=FALSE))
})

lf_autocorr <- lapply(loc_f, function(x) {
  return(table.Autocorrelation(x, digits = 2, max.lag = 30))
})


lf_descript <- lapply(loc_f, function(x){
  return(psych::describe(x, skew=FALSE))
})


eq67data <- lapply(eq67, function(x){
  return(lapply(x, function(y){
    res <- rbind(c(y$coef, sqrt(y$sigma2)), c(diag(sqrt(y$var.coef)), NaN))
    colnames(res) <- c(names(y$coef), "sigma")
    return(res)
  }))
})