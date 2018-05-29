load("./Data/yield_curve_data.RData")
source("generate_latent_factors.R")

# filter yield curves only for specified time series
start_date <- "Sep 1995"
end_date <- "Aug 2005"
source("filter_yield_curves.R")

# interpolate yield curves for specified maturities
xout <- c(6, 9 ,12, 15, 18, 21, 24, 30, 36, 48, 60, 72, 84, 96, 108, 120)
source("interpolate_yield_curves.R")

# estimate Nelson-Siegel latent factors
lambda <- 0.0609
us_lf <- get_lf(us_yield_curve, lambda)
ca_lf <- get_lf(ca_yield_curve, lambda)
jp_lf <- get_lf(jp_yield_curve, lambda)
de_lf <- get_lf(de_yield_curve, lambda)
uk_lf <- get_lf(uk_yield_curve, lambda)

# group latent factors together
loc_level <- cbind(us_lf$beta_0, jp_lf$beta_0, de_lf$beta_0, uk_lf$beta_0)
colnames(loc_level) <- c("US", "JP", "DE", "UK")
loc_slope <- cbind(us_lf$beta_1, jp_lf$beta_1, de_lf$beta_1, uk_lf$beta_1)
colnames(loc_slope) <- c("US", "JP", "DE", "UK")
# loc_curvature <- cbind(us_lf$beta_2, jp_lf$beta_2, de_lf$beta_2, uk_lf$beta_2)
# colnames(loc_curvature) <- c("US", "JP", "DE", "UK")

# perform PCA
pr_level <- prcomp(loc_level, center = TRUE, scale. = TRUE)
pr_slope <- prcomp(loc_slope, center = TRUE, scale. = TRUE)
# pr_curvature <- prcomp(loc_curvature, center = TRUE, scale. = TRUE)

glob_level <- as.xts(pr_level$x[,"PC1"], order.by = time(us_lf))
glob_slope <- as.xts(pr_slope$x[,"PC1"], order.by = time(us_lf))
# glob_curvature <- as.xts(pr_curvature$x[,"PC1"], order.by = time(us_lf))