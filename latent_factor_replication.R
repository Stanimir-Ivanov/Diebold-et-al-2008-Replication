source("./Utils/generate_latent_factors.R")
load("./Data/grouped_yield_curves.RData")
source("range.R")

# estimate Nelson-Siegel latent factors
loc_f <- lapply(yield_curves, function(x){
  res <- get_lf(yield_curve=x, lambda=lambda)
  return(res)
})

remove(yield_curves)

source("./Utils/group_latent_factors.R")

# perform PCA
pr_level <- prcomp(loc_level, center = TRUE, scale. = TRUE)
pr_slope <- prcomp(loc_slope, center = TRUE, scale. = TRUE)
pr_curvature <- prcomp(loc_curvature, center = TRUE, scale. = TRUE)

#extract global factors
glob_level <- as.xts(-pr_level$x[,"PC1"], order.by = time(loc_level))
glob_slope <- as.xts(-pr_slope$x[,"PC1"], order.by = time(loc_slope))
glob_curvature <- as.xts(-pr_curvature$x[,"PC1"], order.by = time(loc_curvature))

# group global factors into a single variable
glob_f <- cbind(glob_level, glob_slope, glob_curvature)
colnames(glob_f) <- c("level", "slope", "curvature")

# cleanup
remove(pr_level, pr_slope, pr_curvature)
remove(glob_level, glob_slope, glob_curvature)

save.image("./Data/latent_factor_data.RData")