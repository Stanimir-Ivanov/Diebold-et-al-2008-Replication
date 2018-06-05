library(stats)

interpolate_yield_curve <- function(yield_curve, xout){
    res <- rollapply(yield_curve, width = 1, by.column = FALSE, FUN = function(x){
      t = time(x)
      tau <- colnames(x) %>% as.numeric()
      res <- approx(x = tau, y = x, xout = xout)
      return(t(res$y) %>% as.xts(order.by = t))
    })
    colnames(res) <- xout
    return(res)
}

us_yield_curve <- interpolate_yield_curve(us_yield_curve, xout)
ca_yield_curve <- interpolate_yield_curve(ca_yield_curve, xout)
jp_yield_curve <- interpolate_yield_curve(jp_yield_curve, xout)
de_yield_curve <- interpolate_yield_curve(de_yield_curve, xout)
uk_yield_curve <- interpolate_yield_curve(uk_yield_curve, xout)