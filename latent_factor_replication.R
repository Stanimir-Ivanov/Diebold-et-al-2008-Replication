load("./Data/yield_curve_data.RData")
setwd("./LF Utils/")

source("generate_latent_factors.R")

# filter yield curves only for specified time series
start_date <- "Apr 1995"
end_date <- "Apr 2018"
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

source("group_latent_factors.R")

# perform PCA
pr_level <- prcomp(loc_level, center = TRUE, scale. = TRUE)
pr_slope <- prcomp(loc_slope, center = TRUE, scale. = TRUE)
pr_curvature <- prcomp(loc_curvature, center = TRUE, scale. = TRUE)

glob_level <- as.xts(-pr_level$x[,"PC1"], order.by = time(loc_level))
glob_slope <- as.xts(-pr_slope$x[,"PC1"], order.by = time(loc_slope))
glob_curvature <- as.xts(-pr_curvature$x[,"PC1"], order.by = time(loc_curvature))

glob_f <- cbind(glob_level, glob_slope, glob_curvature)
colnames(glob_f) <- c("level", "slope", "curvature")

remove(glob_level)
remove(glob_slope)
remove(glob_curvature)

remove(pr_level)
remove(pr_slope)
remove(pr_curvature)

remove(us_yield_curve)
remove(ca_yield_curve)
remove(jp_yield_curve)
remove(uk_yield_curve)
remove(de_yield_curve)

setwd("..")
save.image("./Data/latent_factor_data.RData")