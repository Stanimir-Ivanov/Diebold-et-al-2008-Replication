load("./Data/yield_curve_data.RData")
source("generate_latent_factors.R")

# estimate Nelson-Siegel latent factors
lambda <- 0.7308
us_lf <- get_lf(us_yield_curve, lambda)
ca_lf <- get_lf(ca_yield_curve, lambda)
jp_lf <- get_lf(jp_yield_curve, lambda)
de_lf <- get_lf(de_yield_curve, lambda)
uk_lf <- get_lf(uk_yield_curve, lambda)

# use latent factor data only for specified time period
start_date <- "Jul 1994"
end_date <- "Aug 2005"
us_lf <- us_lf[time(us_lf) >= start_date & time(us_lf) <= end_date]
ca_lf <- ca_lf[time(ca_lf) >= start_date & time(ca_lf) <= end_date]
jp_lf <- jp_lf[time(jp_lf) >= start_date & time(jp_lf) <= end_date]
de_lf <- de_lf[time(de_lf) >= start_date & time(de_lf) <= end_date]
uk_lf <- uk_lf[time(uk_lf) >= start_date & time(uk_lf) <= end_date]

# group latent factors together
loc_level <- cbind(us_lf$beta_0, ca_lf$beta_0, jp_lf$beta_0, de_lf$beta_0, uk_lf$beta_0)
colnames(loc_level) <- c("US", "CA", "JP", "DE", "UK")
loc_slope <- cbind(us_lf$beta_1, jp_lf$beta_1, de_lf$beta_1, uk_lf$beta_1)
colnames(loc_slope) <- c("US", "JP", "DE", "UK")
loc_curvature <- cbind(us_lf$beta_2, ca_lf$beta_2, jp_lf$beta_2, de_lf$beta_2, uk_lf$beta_2)
colnames(loc_curvature) <- c("US", "CA", "JP", "DE", "UK")

# perform PCA
pr_level <- prcomp(loc_level, center = TRUE, scale. = TRUE)
pr_slope <- prcomp(loc_slope, center = TRUE, scale. = TRUE)
pr_curvature <- prcomp(loc_curvature, center = TRUE, scale. = TRUE)

glob_level <- as.xts(pr_level$x[,"PC1"], order.by = time(us_lf))
glob_slope <- as.xts(pr_slope$x[,"PC1"], order.by = time(us_lf))
glob_curvature <- as.xts(pr_curvature$x[,"PC1"], order.by = time(us_lf))