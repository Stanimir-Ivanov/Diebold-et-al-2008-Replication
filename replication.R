load("./Data/yield_curve_data.RData")
source("generate_latent_factors.R")

us_lf <- get_lf(us_yield_curve)
ca_lf <- get_lf(ca_yield_curve)
jp_lf <- get_lf(jp_yield_curve)
de_lf <- get_lf(de_yield_curve)
uk_lf <- get_lf(uk_yield_curve)

loc_level <- cbind(us_lf$beta_0, ca_lf$beta_0, jp_lf$beta_0, de_lf$beta_0, uk_lf$beta_0)
loc_slope <- cbind(us_lf$beta_1, ca_lf$beta_1, jp_lf$beta_1, de_lf$beta_1, uk_lf$beta_1)
loc_curvature <- cbind(us_lf$beta_2, ca_lf$beta_2, jp_lf$beta_2, de_lf$beta_2, uk_lf$beta_2)

sigma_level <- cov(loc_level, na.omit = TRUE)
sigma_slope <- cov(loc_slope, na.omit = TRUE)
sigma_curvature <- cov(loc_curvature, na.omit = TRUE)