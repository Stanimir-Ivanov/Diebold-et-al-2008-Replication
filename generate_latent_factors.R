library(xts)
library(zoo)
library(reshape2)
library(dplyr)

get_lf <- function(yield_curve) {
  return(
    rollapply(yield_curve, width = 1, by.column = FALSE, FUN = function(x)
      return(Nelson.Siegel(rate = x, maturity = colnames(x) %>% as.numeric()))
    )
  )
}
