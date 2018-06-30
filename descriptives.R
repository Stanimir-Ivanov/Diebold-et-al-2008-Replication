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

ext1altdata <- lapply(ext1alt, function(x){
  return(lapply(x, function(y){
    res <- rbind(c(y$coef, sqrt(y$sigma2)), c(diag(sqrt(y$var.coef)), NaN))
    colnames(res) <- c(names(y$coef), "sigma")
    return(res)
  }))
})

ext2data <- lapply(ext2_pd, function(x){
  return(summary(x))
})

alphas <- lapply(ext2_pd, function(x) {
  return(x$coefficients[grepl("factor", names(x$coefficients))])
})

library(latex2exp)

df <- data.frame(alphas$US, alphas$CA, alphas$JP, alphas$DE, alphas$UK)
colnames(df) <- c("US", "CA", "JP", "DE", "UK")
tau <- c(6, 9 ,12, 15, 18, 21, 24, 30, 36, 48, 60, 72, 84, 96, 108, 120)
rownames(df) <- tau
dfz <- as.zoo(df)

plot.zoo(dfz, plot.type = "single", axes = FALSE, col = 1:5, type = 'l', ylab = TeX("$\\alpha$"),
         xlab = TeX("$\\tau$"))
axis(side = 1, at = seq(1, 16, by = 3), labels = tau[seq(1, 16, by = 3)])
axis(side = 2)
legend("topleft", lty = 1, col = 1:5, colnames(dfz), cex = .6)

