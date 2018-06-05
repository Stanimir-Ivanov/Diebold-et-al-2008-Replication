m6 <- read.zoo("./Data/UK/Yield/6m.csv", header = TRUE, sep = ",", index.column = 1, format = "%m/%d/%Y")
m6 <- xts(m6, order.by = time(m6))

uk_yield_curve$'6'[is.na(uk_yield_curve$'6')] = m6[time(uk_yield_curve)[is.na(uk_yield_curve$`6`)]]

remove(m6)