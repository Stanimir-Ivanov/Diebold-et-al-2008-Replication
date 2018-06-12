plot.xts(cbind(ext2$US$residual, ext2$CA$residual, ext2$JP$residual, ext2$DE$residual, ext2$UK$residual), legend.loc = "topright")

load("./Data/grouped_macro.RData")

interest_rate <- cbind(loc_f$US$interest.rate, loc_f$CA$interest.rate, loc_f$JP$interest.rate, loc_f$DE$interest.rate, loc_f$UK$interest.rate)
unemployment <- cbind(loc_f$US$unemployment, loc_f$CA$unemployment, loc_f$JP$unemployment, loc_f$DE$unemployment, loc_f$UK$unemployment)
capacity_utlization <- cbind(loc_f$US$capacity.utilization, loc_f$CA$capacity.utilization, loc_f$JP$capacity.utilization, loc_f$DE$capacity.utilization, loc_f$UK$capacity.utilization)
inflation <- cbind(loc_f$US$inflation, loc_f$CA$inflation, loc_f$JP$inflation, loc_f$DE$inflation, loc_f$UK$inflation)

ir_pr <- prcomp(interest_rate, center = TRUE, scale. = TRUE)
un_pr <- prcomp(unemployment, center = TRUE, scale. = TRUE)
cu_pr <- prcomp(capacity_utlization, center = TRUE, scale. = TRUE)
in_pr <- prcomp(inflation, center = TRUE, scale. = TRUE)

mac_f <- cbind(as.xts(-ir_pr$x[,"PC1"], order.by = time(loc_f$US)),
               as.xts(-un_pr$x[,"PC1"], order.by = time(loc_f$US)),
               as.xts(-cu_pr$x[,"PC1"], order.by = time(loc_f$US)),
               as.xts(-in_pr$x[,"PC1"], order.by = time(loc_f$US)))
colnames(mac_f) <- c("Interest Rate", "Unemployment", "Capacity Utilization", "Inflation")

plot.xts(mac_f, legend.loc = "bottomleft")