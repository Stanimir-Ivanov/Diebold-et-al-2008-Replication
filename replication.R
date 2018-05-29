load("./Data/yield_curve_data.RData")
source("generate_latent_factors.R")

# estimate Nelson-Siegel latent factors
us_lf <- get_lf(us_yield_curve)
ca_lf <- get_lf(ca_yield_curve)
jp_lf <- get_lf(jp_yield_curve)
de_lf <- get_lf(de_yield_curve)
uk_lf <- get_lf(uk_yield_curve)

# use latent factor data only for time period of study
start_date <- "Jul 1986"
end_date <- "Apr 2018"
us_lf <- us_lf[time(us_lf) >= start_date & time(us_lf) <= end_date]
ca_lf <- ca_lf[time(ca_lf) >= start_date & time(ca_lf) <= end_date]
jp_lf <- jp_lf[time(jp_lf) >= start_date & time(jp_lf) <= end_date]
de_lf <- de_lf[time(de_lf) >= start_date & time(de_lf) <= end_date]
uk_lf <- uk_lf[time(uk_lf) >= start_date & time(uk_lf) <= end_date]

# group latent factors together
loc_level <- cbind(us_lf$beta_0, ca_lf$beta_0, jp_lf$beta_0, de_lf$beta_0, uk_lf$beta_0)
loc_slope <- cbind(us_lf$beta_1, ca_lf$beta_1, jp_lf$beta_1, de_lf$beta_1, uk_lf$beta_1)
loc_curvature <- cbind(us_lf$beta_2, ca_lf$beta_2, jp_lf$beta_2, de_lf$beta_2, uk_lf$beta_2)

# perform PCA
pr_level <- prcomp(loc_level, center = TRUE, scale. = TRUE)
pr_slope <- prcomp(loc_slope, center = TRUE, scale. = TRUE)
pr_curvature <- prcomp(loc_curvature, center = TRUE, scale. = TRUE)

# calculate global factors
glob_level <- loc_level %*% pr_level$rotation[,"PC1"]
glob_slope <- loc_slope %*% pr_slope$rotation[,"PC1"]
glob_curvature <- loc_curvature %*% pr_curvature$rotation[,"PC1"]

glob_level <- as.xts(glob_level, order.by = time(us_lf))
glob_slope <- as.xts(glob_slope, order.by = time(us_lf))
glob_curvature <- as.xts(glob_curvature, order.by = time(us_lf))