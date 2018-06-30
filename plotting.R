load("./Data/grouped_macro.RData")
par(mfrow=c(1,1))
plot.xts(glob_f, main = "Global Latent Factors", major.ticks.on = "years", grid.ticks.on = "years", grid.ticks.lwd = 1, 
         grid.ticks.lty = 1, lwd = 1.5, cex.axis = 1.5, cex.main = 1.5)
title(ylab="Principal Component")
axis(side = 1, outer = TRUE, at = seq(1, 273, by = 36), labels = format(time(glob_f$level)[seq(1, 273, by = 36)], "%Y"), tck = 36)

par(mfrow=c(3,1))
plot.xts(
  x = cbind(
    loc_f$US$level,
    loc_f$CA$level,
    loc_f$DE$level,
    loc_f$JP$level,
    loc_f$UK$level
  ),
  main = "Local Level Factors",
  lwd = 1.5,
  main.cex = 1.75,
  cex.axis = 1.5,
  yaxis.right = FALSE,
  major.ticks.on = "years", grid.ticks.on = "years",
  grid 	
)

axis(side = 1, outer = TRUE, at = seq(1, 273, by = 36), labels = format(time(glob_f$level)[seq(1, 273, by = 36)], "%Y"), tck = 36)

plot.xts(
  x = cbind(
    loc_f$US$slope,
    loc_f$CA$slope,
    loc_f$DE$slope,
    loc_f$JP$slope,
    loc_f$UK$slope
  ),
  main = "Local Slope Factors",
  lwd = 1.5,
  main.cex = 1.75,
  cex.axis = 1.5,
  yaxis.right = FALSE,
  major.ticks.on = "years", grid.ticks.on = "years",
)

axis(side = 1, outer = TRUE, at = seq(1, 273, by = 36), labels = format(time(glob_f$level)[seq(1, 273, by = 36)], "%Y"), tck = 36)

plot.xts(
  x = cbind(
    loc_f$US$curvature,
    loc_f$CA$curvature,
    loc_f$DE$curvature,
    loc_f$JP$curvature,
    loc_f$UK$curvature
  ),
  main = "Local Curvature Factors",
  lwd = 1.5,
  main.cex = 1.75,
  cex.axis = 1.5,
  yaxis.right = FALSE,
  major.ticks.on = "years", grid.ticks.on = "years",
)

axis(side = 1, outer = TRUE, at = seq(1, 273, by = 36), labels = format(time(glob_f$level)[seq(1, 273, by = 36)], "%Y"), tck = 36)