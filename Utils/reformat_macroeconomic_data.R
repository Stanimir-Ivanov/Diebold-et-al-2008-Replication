load("../Data/raw_marco.RData")

# bind factors together
ca_macro <- cbind(ca_cu, ca_infl, ca_r, ca_u)
colnames(ca_macro) <- c("capacity utilization", "inflation", "interest rate", "unemployment")
de_macro <- cbind(de_cu, de_infl, de_r, de_u)
colnames(de_macro) <- c("capacity utilization", "inflation", "interest rate", "unemployment")
us_macro <- cbind(us_cu, us_infl, us_r, us_u)
colnames(us_macro) <- c("capacity utilization", "inflation", "interest rate", "unemployment")
jp_macro <- cbind(jp_cu, jp_infl, jp_r, jp_u)
colnames(jp_macro) <- c("capacity utilization", "inflation", "interest rate", "unemployment")
uk_macro <- cbind(uk_cu, uk_infl, uk_r, uk_u)
colnames(uk_macro) <- c("capacity utilization", "inflation", "interest rate", "unemployment")

# filter to range
source("../range.R")
ca_macro <- ca_macro[time(ca_macro) >= start_date & time(ca_macro) <= end_date]
de_macro <- de_macro[time(de_macro) >= start_date & time(de_macro) <= end_date]
us_macro <- us_macro[time(us_macro) >= start_date & time(us_macro) <= end_date]
jp_macro <- jp_macro[time(jp_macro) >= start_date & time(jp_macro) <= end_date]
uk_macro <- uk_macro[time(uk_macro) >= start_date & time(uk_macro) <= end_date]

# CA reports CU in Mar, so use interpolated value as first observation
if(start_date == "Apr 1995")
  ca_cu <- xts(83.05, as.Date("1 Apr 1995", format = "%d %b %Y")) %>% rbind(ca_cu)
  ca_cu <- to.monthly(ca_cu, drop.time = TRUE)
  ca_cu <- ca_cu$ca_cu.Close


# interpolate quarterly reported capacity utilization figures for CA, DE, UK to monthly frequencies

# reporting quarters
ca_cu <- ca_cu[time(ca_cu) >= start_date & time(ca_cu) <= end_date]
de_cu <- de_cu[time(de_cu) >= start_date & time(de_cu) <= end_date]
uk_cu <- uk_cu[time(uk_cu) >= start_date & time(uk_cu) <= end_date]

ca_q <- time(ca_cu) %>% as.numeric()
de_q <- time(de_cu) %>% as.numeric()
uk_q <- time(uk_cu) %>% as.numeric()

ca_macro$`capacity utilization` <- approx(x = ca_q, y = ca_cu, xout = time(ca_macro) %>% as.numeric())$y
de_macro$`capacity utilization` <- approx(x = de_q, y = de_cu, xout = time(de_macro) %>% as.numeric())$y
uk_macro$`capacity utilization` <- approx(x = uk_q, y = uk_cu, xout = time(uk_macro) %>% as.numeric())$y

load("../Data/latent_factor_data.RData")

loc_f[["CA"]] <- cbind(loc_f[["CA"]], ca_macro)
loc_f[["DE"]] <- cbind(loc_f[["DE"]], de_macro)
loc_f[["US"]] <- cbind(loc_f[["US"]], us_macro)
loc_f[["JP"]] <- cbind(loc_f[["JP"]], jp_macro)
loc_f[["UK"]] <- cbind(loc_f[["UK"]], uk_macro)

# workspace cleanup
remove(ca_cu, ca_infl, ca_r, ca_u)
remove(de_cu, de_infl, de_r, de_u)
remove(us_cu, us_infl, us_r, us_u)
remove(jp_cu, jp_infl, jp_r, jp_u)
remove(uk_cu, uk_infl, uk_r, uk_u)
remove(ca_macro, de_macro, jp_macro, us_macro, uk_macro)
remove(loc_curvature, loc_level, loc_slope)


save.image("../Data/grouped_macro.RData")
