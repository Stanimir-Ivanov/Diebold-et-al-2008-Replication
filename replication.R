load("./Data/yield_curve_data.RData")
source("generate_latent_factors.R")

us_lf <- get_lf(us_yield_curve)
ca_lf <- get_lf(ca_yield_curve)
jp_lf <- get_lf(jp_yield_curve)
de_lf <- get_lf(de_yield_curve)
uk_lf <- get_lf(uk_yield_curve)