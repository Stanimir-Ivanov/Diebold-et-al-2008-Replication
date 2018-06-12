library(xts)
library(zoo)
library(reshape2)
library(dplyr)

get_lf <- function(yield_curve, lambda) {
  return(
    rollapply(yield_curve, width = 1, by.column = FALSE, FUN = function(x) {
      res <- Nelson.Siegel(rate = x, tau = colnames(x) %>% as.numeric(), lambda = lambda)
      return(res$Par)
    })
  )
}

get_lf_ma1 <- function(yc_res_data, tau, lambda) {
  return(rollapply(yc_res_data, width = 1, by.column = FALSE, FUN = function(x){
      rate <- x[,grepl("Yield.Curve", colnames(x))]
      rate_residuals <- x[,grepl("Residuals", colnames(x))]
      res <- Nelson.Siegel.MA1(rate, rate_residuals, tau, lambda)
      return(res$Par)
    }))
}

get_pd <- function(pandel_data, tau, lambda) {
  return(rollapply(pandel_data, width = 1, by.column = FALSE, FUN = function(x){
    rate <- x[,grepl("Yield.Curve", colnames(x))]
    fit <- x[,grepl("Yield.Curve.Fit", colnames(x))]
    fit_res <- x[,grepl("Residuals", colnames(x))]
    res <- Panel.Data.Heterogen(rate, fit, fit_res, tau)
    return(res)
  }))
}

get_res <- function(yield_curve, lambda) {
  return(
    rollapply(yield_curve, width = 1, by.column = FALSE, FUN = function(x) {
      res <- Nelson.Siegel(rate = x, tau = colnames(x) %>% as.numeric(), lambda = lambda)
      # colnames(res$Res) <- paste(colnames(res$Res),"Residuals")
      return(res$Res)
    })
  )
}

get_fit <- function(yield_curve, lambda) {
  return(
    rollapply(yield_curve, width = 1, by.column = FALSE, FUN = function(x) {
      fit <- Nelson.Siegel.Fit(rate = x, tau = colnames(x) %>% as.numeric(), lambda = lambda)
      return(fit)
    })
  )
}

Panel.Data.Heterogen <- function(rate, fit, fit_res, tau) {
  t <- time(rate)
  res <- lm(t(rate) ~ 0 + t(fit) + f(fit_res) + factor(tau) - 1)
  betaPar <- coef(beta) %>% t() %>% as.xts(order.by = t)
  names(betaPar) <- c("fit", "residual")
  fit <- fitted.values(beta) %>% t() %>% as.xts(order.by = t)
  EstResults <- list(Par=betaPar, Fit=fit)
  return(EstResults)
}

Nelson.Siegel <- function(rate, tau, lambda) {
  t <- time(rate)
  fb1 <- factorBeta1(tau, lambda) %>% t() %>% as.xts(order.by = t)
  fb2 <- factorBeta2(tau, lambda) %>% t() %>% as.xts(order.by = t)
  
  beta <- lm(t(rate) ~ 1 + t(fb1) + t(fb2))
  betaPar <- coef(beta) %>% t() %>% as.xts(order.by = t)
  NaValues <- na.omit(betaPar)
  if(length(NaValues) < 3) betaPar <- c(0,0,0)
  names(betaPar) <- c("level", "slope", "curvature")
  res <- resid(beta) %>% t() %>% as.xts(order.by = t)
  EstResults <- list(Par=betaPar, Res=res)
  return(EstResults)
}

Nelson.Siegel.MA1 <- function(rate, rate_residuals, tau, lambda) {
  t <- time(rate)
  fb1 <- factorBeta1(tau, lambda) %>% t() %>% as.xts(order.by = t)
  fb2 <- factorBeta2(tau, lambda) %>% t() %>% as.xts(order.by = t)
  
  beta <- lm(t(rate) ~ 1 + t(fb1) + t(fb2) + t(rate_residuals))
  betaPar <- coef(beta) %>% t() %>% as.xts(order.by = t)
  names(betaPar) <- c("level", "slope", "curvature", "residual")
  res <- resid(beta) %>% t() %>% as.xts(order.by = t)
  EstResults <- list(Par=betaPar, Res=res)
  return(EstResults)
}

Nelson.Siegel.Fit <- function(rate, tau, lambda) {
  t <- time(rate)
  fb1 <- factorBeta1(tau, lambda) %>% t() %>% as.xts(order.by = t)
  fb2 <- factorBeta2(tau, lambda) %>% t() %>% as.xts(order.by = t)
  
  beta <- lm(t(rate) ~ 1 + t(fb1) + t(fb2))
  fit <- fitted.values(beta) %>% t() %>% as.xts(order.by = t)
  names(fit) <- tau
  return(fit)
}

Nelson.Siegel2 <- function(rate, tau, lambda) {
  t <- time(rate)
  fb1 <- factorBeta1(tau, lambda) %>% t() %>% as.xts(order.by = t)
  
  beta <- lm(t(rate) ~ 1 + t(fb1))
  betaPar <- coef(beta) %>% t() %>% as.xts(order.by = t)
  NaValues <- na.omit(betaPar)
  if(length(NaValues) < 2) betaPar <- c(0,0)
  names(betaPar) <- c("beta_0", "beta_1")
  res <- resid(beta) %>% t() %>% as.xts(order.by = t)
  EstResults <- list(Par=betaPar, Res=res)
  return(EstResults)
}

factorBeta1 <- function(tau, lambda) {
  return((1 - exp(-lambda * tau))/(lambda * tau))
}

factorBeta2 <- function(tau, lambda) {
  return((1 - exp(-lambda * tau))/(lambda * tau) - exp(-lambda * tau))
}

melt_list <- function(lst) {
  return(lapply(lst, function(x){
    tmp <- data.frame(time(x), x)
    return(melt(tmp, id = colnames(tmp)[grepl("time", colnames(tmp))]))
  }))
}