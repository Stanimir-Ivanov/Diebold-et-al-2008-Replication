library(latex2exp)
load("./Data/grouped_yield_curves.RData")
load("./Data/Forecast/all_forecasts.RData")

mspe <- function(realization_list, forecast_list){
  mapply(function(x, y){
    return(colMeans((x - y)^2, na.rm = TRUE))
  }, x = realization_list, y = forecast_list, SIMPLIFY = TRUE)
} 

plot_mspe <- function(fe, title){
  plot.zoo(fe, plot.type = "single", col = 1:5, type = 'o', main = title, ylab = TeX("$MSE$"),
          ylim = c(0.00, 0.30), xlab = TeX("$\\tau$"), xaxt = "n", lwd = 1.5, cex.main = 1.75, 
          cex.lab=1.5, cex.axis = 1.5)
  axis(side = 1, at = seq(1, 16, by = 3), labels = tau[seq(1, 16, by = 3)])
}

plot_dm <- function(dm_mat, title){
  plot.zoo(as.zoo(dm_mat), plot.type="single", col = 1:5, ylim = c(-3.5, 3.5), type = 'o', main = title, cex.axis = 1.5,
           xlab = TeX("$\\tau$"), xaxt = "n", cex.main = 1.75, cex.lab=1.5, ylab = TeX("$t-stat$"),)
  axis(side = 1, at = seq(1, 16, by = 3), labels = tau[seq(1, 16, by = 3)])
  axis(side = 1, at = -3:3, labels = -3:3)
  abline(h = -2)
  abline(h = 2)
  # legend("topright", legend = c("US", "CA", "DE", "JP", "US"), col = 1:5, lty = 1, cex = .6)
  # for(j in 1:ncol(dm_list$dm_stat)){
  #   make_err_bar(dm_list$dm_stat[,j], dm_list$se[,j], z = 2)
  # }
}

make_err_bar <- function(fe, se, epsilon = 0.02, z = 2){
  x = 1:length(fe)
  for(i in x){
    up = fe[i] + z*se[i]
    low = fe[i] - z*se[i]
    segments(x[i], low , x[i], up)
    segments(x[i] - epsilon, up , x[i] + epsilon, up)
    segments(x[i] - epsilon, low , x[i] + epsilon, low)
  }
}

dm <- function(realization_list, forecast_list){
  mapply(function(x, y){
    fe = x - y
    fe_mean = colMeans(fe, na.rm = TRUE)
    fe_se = apply(fe, 2, sd, na.rm = TRUE)/sqrt(nrow(fe))
    return(fe_mean/fe_se)
  }, x = realization_list, y = forecast_list, SIMPLIFY = TRUE)
} 

dm_base <- dm(yield_curves, y_hat)
dm_ma <- dm(yield_curves, y_hat_ma)
dm_macro <- dm(yield_curves, y_hat_macro)
dm_macro_ma <- dm(yield_curves, y_hat_macro_ma)

tau <- c(6, 9 ,12, 15, 18, 21, 24, 30, 36, 48, 60, 72, 84, 96, 108, 120)
par(mfrow=c(2,2))
plot_dm(dm_base,"I: Latent factors")
plot_dm(dm_macro,"II: Latent & Macro factors")
plot_dm(dm_ma,"III: MA Latent factors")
plot_dm(dm_macro_ma,"IV: MA Latent & Macro factors")


fe_base <- as.zoo(mspe(yield_curves, y_hat))
fe_ma <- as.zoo(mspe(yield_curves, y_hat_ma))
fe_macro <- as.zoo(mspe(yield_curves, y_hat_macro))
fe_macro_ma <- as.zoo(mspe(yield_curves, y_hat_macro_ma))

tau <- c(6, 9 ,12, 15, 18, 21, 24, 30, 36, 48, 60, 72, 84, 96, 108, 120)
par(mfrow=c(2,2))
plot_mspe(fe_base, "I: Latent factors")
plot_mspe(fe_macro, "II: Latent & Macro factors")
plot_mspe(fe_ma, "III: MA Latent factors")
plot_mspe(fe_macro_ma, "IV: MA Latent & Macro factors")
