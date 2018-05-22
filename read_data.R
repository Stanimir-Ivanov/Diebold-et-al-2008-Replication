library(xts)
library(zoo)
library(reshape2)


# US
setwd("C:/Users/Stan/Documents/Repos/Diebold-et-al-2008-Replication/Data/US")
us_raw <- read.zoo("./Data/FRB_H15.csv", 
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

# Germany
setwd("C:/Users/Stan/Documents/Repos/Diebold-et-al-2008-Replication/Data/DE")
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
setwd("C:/Users/Stan/Documents/Repos/Diebold-et-al-2008-Replication/Data/CA")
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


# UK
setwd("C:/Users/Stan/Documents/Repos/Diebold-et-al-2008-Replication/Data/UK")
uk_raw <- read.zoo("curve_data.csv", header = TRUE, sep = ",", index.column = 1, format = "%d-%b-%y")
colnames(uk_raw) <- substr(colnames(uk_raw), start =2, stop = 10)
