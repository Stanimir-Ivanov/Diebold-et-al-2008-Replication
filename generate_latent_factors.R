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

Nelson.Siegel <- function(rate, tau, lambda)
{
  t <- time(rate)
  fb1 <- factorBeta1(tau, lambda) %>% t() %>% as.xts(order.by = t)
  fb2 <- factorBeta2(tau, lambda) %>% t() %>% as.xts(order.by = t)
  
  beta <- lm(t(rate) ~ 1 + t(fb1) + t(fb2))
  betaPar <- coef(beta) %>% t() %>% as.xts(order.by = t)
  NaValues <- na.omit(betaPar)
  if(length(NaValues) < 3) betaPar <- c(0,0,0)
  names(betaPar) <- c("beta_0", "beta_1", "beta_2")
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