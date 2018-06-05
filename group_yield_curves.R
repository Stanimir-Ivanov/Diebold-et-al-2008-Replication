load("./Data/raw_yield_curves.RData")

# filter yield curves only for specified ramge
source("./range.R")
source("./Utils/filter_yield_curves.R")

# interpolate yield curves for specified maturities
xout <- c(6, 9 ,12, 15, 18, 21, 24, 30, 36, 48, 60, 72, 84, 96, 108, 120)
source("./Utils/interpolate_yield_curves.R")

# group yield curves
yield_curves <- list("US" = us_yield_curve, "CA" = ca_yield_curve,
                     "JP" = jp_yield_curve, "DE" = de_yield_curve, "UK" = uk_yield_curve)

# cleanup
remove(ca_yield_curve)
remove(us_yield_curve)
remove(jp_yield_curve)
remove(de_yield_curve)
remove(uk_yield_curve)

save.image("./Data/grouped_yield_curves.RData")