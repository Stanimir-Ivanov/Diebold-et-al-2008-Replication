library(xts)
library(zoo)
library(reshape2)


# US
setwd("./Data/US/Yield")
us_raw <- read.zoo("FRB_H15.csv", 
                   header = TRUE, 
                   sep = ",",
                   format="%m/%d/%Y",
                   na.strings = c("", "ND"),
                   index.column = 1)
# start of 3m availability
us_raw <- us_raw[time(us_raw) >= '1982-01-04']
# maturities 3m to 10y
us_raw <- us_raw[,2:9]
colnames(us_raw) <- c(3, 6, 12, 24, 36, 60, 84, 120)
us_raw <- as.xts(us_raw)

month <- function(x) format(x, "%Y-%m")

us_raw <- us_raw[!is.na(us_raw[,1]),]
us_raw <- aggregate(us_raw, by=month, FUN=last)
us_raw <- as.xts(us_raw, order.by = as.yearmon(format(time(us_raw)), "%Y-%m"))

# Germany
setwd("./Data/DE/Yield")
file.list <- list.files(pattern='*.csv')

de_raw <- lapply(file.list,
             function(x) read.zoo(file=x, 
                                  tz="UTC", 
                                  header=FALSE, 
                                  sep=",", 
                                  index.column = 1, 
                                  FUN = as.yearmon, 
                                  skip = 5)
             )

de_raw <- do.call(merge,lapply(de_raw,as.xts))
de_raw <- de_raw[,rep(c(TRUE, FALSE), 11)]
colnames(de_raw) <- c(6, 12, 24, 36, 48, 60, 72, 84, 96, 108, 120)


# Canada
setwd("./Data/CA/Yield")
file.list <- list.files(pattern='*.csv')

ca_raw <- list()
for (file in file.list){
  ca_raw[file] <- read.csv(file, header = TRUE, sep = ",")
}

ca_raw <- lapply(file.list, function(x) read.csv(file = x, header = TRUE, sep = ","))
ca_raw <- lapply(ca_raw, function(x) melt(x, id="X"))
ca_raw <- lapply(ca_raw, function(x)
  zoo(x = x[,"value"],
      order.by = as.yearmon(paste(x[,'X'], 
                                  substr(x[,'variable'], start = 2, stop = 4), 
                                  sep = "-"))
      )
)

ca_raw <- lapply(ca_raw, as.xts)
ca_raw <- merge(ca_raw[[1]], ca_raw[[2]], ca_raw[[3]], ca_raw[[4]], ca_raw[[5]], ca_raw[[6]])
colnames(ca_raw) <- c(120,24,36,60,6,84)
ca_raw <- ca_raw[time(ca_raw) >= '1982-01-04']

m12 <- read.zoo("12m.csv", header = TRUE, sep = ",", index.column = 1, format = "%Y-%m-%d")
m48 <- read.zoo("48m.csv", header = TRUE, sep = ",", index.column = 1, format = "%Y-%m-%d")
m12 <- as.xts(m12)
m48 <- as.xts(m48)
m12 <- to.monthly(m12, drop.time = TRUE)
m48 <- to.monthly(m48, drop.time = TRUE)

ca_raw <- merge(ca_raw, m12$m12.Close, m48$m48.Close)
colnames(ca_raw) <- c(120,24,36,60,6,84,12,48)
ca_raw <- ca_raw[,c("6","12","24","36","48","60","84","120")]

# UK
setwd("./Data/UK/Yield")
uk_raw <- read.zoo("curve_data.csv", header = TRUE, sep = ",", index.column = 1, format = "%d-%b-%y")
colnames(uk_raw) <- substr(colnames(uk_raw), start =2, stop = 10)
uk_raw <- as.xts(uk_raw)
uk_raw <- uk_raw[,c("6", "12", "24", "36", "48", "60", "72", "84", "96", "108", "120")]

# Japan
setwd("./Data/JP/Yield")
jp_raw <- read.zoo("jgbcme_all.csv", header = TRUE, sep = ",", index.column = 1, format = "%m/%d/%Y")

jp_raw <- as.xts(jp_raw)
month <- function(x) format(x, "%Y-%m")
jp_raw <- aggregate(jp_raw, by=month, FUN=last)
jp_raw <- as.xts(jp_raw, order.by = as.yearmon(format(time(jp_raw)), "%Y-%m"))
m6 <- read.zoo("6m.csv", header = TRUE, sep = ",", index.column = 1, format = "%Y-%m-%d")
m6 <- as.xts(m6)
m6 <- to.monthly(m6, drop.time = TRUE)

jp_raw <- merge(jp_raw, m6$X6)
jp_raw <- jp_raw[,c("X6", "X12", "X24", "X36", "X48", "X60", "X72", "X84", "X96", "X108", "X120")]
colnames(jp_raw) <- c("6", "12", "24", "36", "48", "60", "72", "84", "96", "108", "120")

setwd("..")
save.image("yield_curve_data.RData")

setwd("..")

ca_yield_curve <- ca_raw
de_yield_curve <- de_raw
jp_yield_curve <- jp_raw
uk_yield_curve <- uk_raw
us_yield_curve <- us_raw

# macroeconomic data

# CA
setwd("./Data/CA/Macro")
ca_cu <- read.zoo("Canada Capacity Utilization.csv", header = TRUE, sep = ",", index.column = 1, format = "%Y-%m-%d")
ca_cu <- as.xts(ca_cu)
ca_infl <- read.zoo("Canada Inflation Rate.csv", header = TRUE, sep = ",", index.column = 1, format = "%Y-%m-%d")
ca_infl <- as.xts(ca_infl)
ca_r <- read.zoo("Canada Interest Rate.csv", header = TRUE, sep = ",", index.column = 1, format = "%Y-%m-%d")
ca_r <- as.xts(ca_r)
ca_u <- read.zoo("Canada Unemployment Rate.csv", header = TRUE, sep = ",", index.column = 1, format = "%Y-%m-%d")
ca_u <- as.xts(ca_u)

setwd("../..")

# DE
setwd("./DE/Macro")
de_cu <- read.zoo("Germany Capacity Utilization.csv", header = TRUE, sep = ",", index.column = 1, format = "%Y-%m-%d")
de_cu <- as.xts(de_cu)
de_infl <- read.zoo("Germany Inflation Rate.csv", header = TRUE, sep = ",", index.column = 1, format = "%Y-%m-%d")
de_infl <- as.xts(de_infl)
de_db_r <- read.zoo("Deutchebank Intrest Rate.csv", header = TRUE, sep = ",", index.column = 1, format = "%Y-%m-%d")
de_db_r <- as.xts(de_db_r)
de_ecb_r <- read.zoo("ECB Intrest Rate.csv", header = TRUE, sep = ",", index.column = 1, format = "%Y-%m-%d")
de_ecb_r <- as.xts(de_ecb_r)
de_r <- rbind(de_db_r, de_ecb_r)
remove(de_db_r)
remove(de_ecb_r)
de_u <- read.zoo("Germany Unemployment Rate.csv", header = TRUE, sep = ",", index.column = 1, format = "%Y-%m-%d")
de_u <- as.xts(de_u)

setwd("../..")

# Japan
setwd("./JP/Macro")
jp_cu <- read.zoo("Japan Capacity Utilization.csv", header = TRUE, sep = ",", index.column = 1, format = "%Y-%m-%d")
jp_cu <- as.xts(jp_cu)
jp_infl <- read.zoo("Japan Inflation Rate.csv", header = TRUE, sep = ",", index.column = 1, format = "%Y-%m-%d")
jp_infl <- as.xts(jp_infl)
jp_r <- read.zoo("Japan Interest Rate.csv", header = TRUE, sep = ",", index.column = 1, format = "%Y-%m-%d")
jp_r <- as.xts(jp_r)
jp_u <- read.zoo("Japan Unemployment Rate.csv", header = TRUE, sep = ",", index.column = 1, format = "%Y-%m-%d")
jp_u <- as.xts(jp_u)


setwd("../..")

# UK
setwd("./UK/Macro")
uk_cu <- read.zoo("United Kingdom Capacity Utilization.csv", header = TRUE, sep = ",", index.column = 1, format = "%Y-%m-%d")
uk_cu <- as.xts(uk_cu)
uk_infl <- read.zoo("United Kingdom Inflation Rate.csv", header = TRUE, sep = ",", index.column = 1, format = "%Y-%m-%d")
uk_infl <- as.xts(uk_infl)
uk_r <- read.zoo("United Kingdom Interest Rate.csv", header = TRUE, sep = ",", index.column = 1, format = "%Y-%m-%d")
uk_r <- as.xts(uk_r)
uk_u <- read.zoo("United Kingdom Unemployment Rate.csv", header = TRUE, sep = ",", index.column = 1, format = "%Y-%m-%d")
uk_u <- as.xts(uk_u)

setwd("../..")

# US (can into special)
setwd("./US/Macro")
us_cu <- read.zoo("TCU.csv", header = TRUE, sep = ",", index.column = 1, format = "%Y-%m-%d")
us_cu <- as.xts(us_cu)
us_infl <- read.zoo("CPI_growth.csv", header = TRUE, sep = ",", index.column = 1, format = "%Y-%m-%d")
us_infl <- as.xts(us_infl)
us_r <- read.zoo("FEDFUNDS.csv", header = TRUE, sep = ",", index.column = 1, format = "%Y-%m-%d")
us_r <- as.xts(us_r)
us_u <- read.zoo("Unemployment.csv", header = TRUE, sep = ",", index.column = 1, format = "%Y-%m-%d")
us_u <- as.xts(us_u)

