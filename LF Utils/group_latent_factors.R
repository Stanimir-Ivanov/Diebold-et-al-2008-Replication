colnames(us_lf) <- c("level", "slope", "curvature")
colnames(ca_lf) <- c("level", "slope", "curvature")
colnames(jp_lf) <- c("level", "slope", "curvature")
colnames(de_lf) <- c("level", "slope", "curvature")
colnames(uk_lf) <- c("level", "slope", "curvature")

# group latent factors together
loc_level <- cbind(us_lf$level, ca_lf$level, jp_lf$level, de_lf$level, uk_lf$level)
colnames(loc_level) <- c("US", "CA", "JP", "DE", "UK")
loc_slope <- cbind(us_lf$slope, ca_lf$slope, jp_lf$slope, de_lf$slope, uk_lf$slope)
colnames(loc_slope) <- c("US", "CA", "JP", "DE", "UK")
loc_curvature <- cbind(us_lf$curvature, ca_lf$curvature, jp_lf$curvature, de_lf$curvature, uk_lf$curvature)
colnames(loc_curvature) <- c("US", "CA", "JP", "DE", "UK")

loc_f <- list("US" = us_lf, "CA" = ca_lf, "JP" = jp_lf, "DE" = de_lf, "UK" = uk_lf)

remove(us_lf)
remove(ca_lf)
remove(jp_lf)
remove(de_lf)
remove(uk_lf)