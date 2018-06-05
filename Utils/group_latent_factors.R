# group latent factors together
loc_level <- cbind(loc_f$US$level, loc_f$CA$level, loc_f$JP$level, loc_f$DE$level, loc_f$UK$level)
colnames(loc_level) <- c("US", "CA", "JP", "DE", "UK")
loc_slope <- cbind(loc_f$US$slope, loc_f$CA$slope, loc_f$JP$slope, loc_f$DE$slope, loc_f$UK$slope)
colnames(loc_slope) <- c("US", "CA", "JP", "DE", "UK")
loc_curvature <- cbind(loc_f$US$curvature, loc_f$CA$curvature, loc_f$JP$curvature, loc_f$DE$curvature, loc_f$UK$curvature)
colnames(loc_curvature) <- c("US", "CA", "JP", "DE", "UK")