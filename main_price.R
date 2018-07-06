library(xts)
library(forecast)
library(tidyverse)
require(xlsx)
library(tseries)
library(rucm)

# get the data
setwd("D:/1 Study Material/US Electricity Consumption/")
price <- read.xlsx ("data/data.xlsx", sheetName = "Average retail price cents kWh", check.names=FALSE, header = TRUE)
generation <- read.xlsx ("data/data.xlsx", sheetName = "Net generation (MWh)", check.names=FALSE, header = TRUE)

# find the top 10 consumption state
generation <- generation[order(generation$`2016`, decreasing = TRUE),]
name = as.character(generation$State[2:11])
## from name, we get the following ten states:
## "Texas","Florida","Pennsylvania","California","Illinois","Alabama","New York","Georgia","North Carolina","Ohio"

# transpose data
price <- price %>% gather(year, value, 2:18) %>% spread(State, value)
generation <- generation %>% gather(year, value, 2:18) %>% spread(State, value)


yr <- as.Date(paste(price$year, 1, 1, sep = "-"))
df <- data.frame(year = c(2000:2026))

######### begin #############

# Texas
adf.test(price$Texas) # p-value > 0.05, so we can't reject null hyphthesis: non-stationary
adf.test(diff(price$Texas)) # p-value > 0.05, so we can't reject null hyphthesis: non-stationary
adf.test(diff(price$Texas,2)) 

myts = xts(price$Texas, order.by = yr)
fit = auto.arima(myts, d = 0, D = 1)
pred = forecast(fit, 10)
png(filename = "Texas price.png")
plot(pred, showgap = FALSE, PI = FALSE, xlab = "from 2000 to 2026", ylab = "Electricity consumption (in cents/kWh)",
     main = "Texas Electricity price Estimation, from 2000 to 2026")
dev.off()
df['Texas'] = c(as.numeric(pred$x), as.numeric(pred$mean))

# Florida
adf.test(price$Florida) # p-value > 0.05, so we can't reject null hyphthesis: non-stationary
adf.test(diff(price$Florida)) # p-value > 0.05, so we can't reject null hyphthesis: non-stationary
adf.test(diff(price$Florida,2)) # p-value get higher, so we can't do the differencing

myts = xts(price$Florida, order.by = yr)
fit = auto.arima(myts, d = 2, D = 1)
pred = forecast(fit, 10)
png(filename = "Florida price.png")
plot(pred, showgap = FALSE, PI = FALSE, xlab = "from 2000 to 2026", ylab = "Electricity consumption (in cents/kWh)",
     main = "Florida Electricity price Estimation, from 2000 to 2026")
dev.off()
df['Florida'] = c(as.numeric(pred$x), as.numeric(pred$mean))

# Pennsylvania
adf.test(price$Pennsylvania) # p-value > 0.05, so we can't reject null hyphthesis: non-stationary
adf.test(diff(price$Pennsylvania, 2)) 

myts = xts(price$Pennsylvania, order.by = yr)
fit = auto.arima(myts, d=0)
pred = forecast(fit, 10)
png(filename = "Pennsylvania price.png")
plot(pred, showgap = FALSE, PI = FALSE, xlab = "from 2000 to 2026", ylab = "Electricity consumption (in cents/kWh)",
     main = "Pennsylvania Electricity price Estimation, from 2000 to 2026")
dev.off()
df['Pennsylvania'] = c(as.numeric(pred$x), as.numeric(pred$mean))

# California
adf.test(price$California) # p-value > 0.05, so we can't reject null hyphthesis: non-stationary
adf.test(diff(price$California)) # p-value > 0.05, so we can't reject null hyphthesis: non-stationary

myts = xts(price$California, order.by = yr)
fit = auto.arima(myts, stationary = FALSE)
pred = forecast(fit, 10)
png(filename = "California price.png")
plot(pred, showgap = FALSE, PI = FALSE, xlab = "from 2000 to 2026", ylab = "Electricity consumption (in cents/kWh)",
     main = "California Electricity price Estimation, from 2000 to 2026")
dev.off()
df['California'] = c(as.numeric(pred$x), as.numeric(pred$mean))

# Illinois
adf.test(price$Illinois)
adf.test(diff(price$Illinois))
adf.test(diff(price$Illinois, 2))

myts = xts(price$Illinois, order.by = yr)
fit = auto.arima(myts,d = 0)
pred = forecast(fit, 10)
png(filename = "Illinois price.png")
plot(pred, showgap = FALSE, PI = FALSE, xlab = "from 2000 to 2026", ylab = "Electricity consumption (in cents/kWh)",
     main = "Illinois Electricity price Estimation, from 2000 to 2026")
dev.off()
df['Illinois'] = c(as.numeric(pred$x), as.numeric(pred$mean))

# Alabama
adf.test(price$Alabama)
adf.test(diff(price$Alabama))
adf.test(diff(price$Alabama,2))

myts = xts(price$Alabama, order.by = yr)
fit = auto.arima(myts, d = 0)
pred = forecast(fit, 10)
png(filename = "Alabama price.png")
plot(pred, showgap = FALSE, PI = FALSE, xlab = "from 2000 to 2026", ylab = "Electricity consumption (in cents/kWh)",
     main = "Alabama Electricity price Estimation, from 2000 to 2026")
dev.off()
df['Alabama'] = c(as.numeric(pred$x), as.numeric(pred$mean))

# New York
adf.test(price$'New York')
adf.test(diff(price$'New York'))
adf.test(diff(price$'New York', 2))

myts = xts(price$'New York', order.by = yr)
fit = auto.arima(myts,d = 0)
pred = forecast(fit, 10)
png(filename = "New York price.png")
plot(pred, showgap = FALSE, PI = FALSE, xlab = "from 2000 to 2026", ylab = "Electricity consumption (in cents/kWh)",
     main = "New York Electricity price Estimation, from 2000 to 2026")
dev.off()
df['New York'] = c(as.numeric(pred$x), as.numeric(pred$mean))

# Georgia
adf.test(price$Georgia)
adf.test(diff(price$Georgia))
adf.test(diff(price$Georgia,2))

myts = xts(price$Georgia, order.by = yr)
fit = auto.arima(myts, d = 2)
pred = forecast(fit, 10)
png(filename = "Georgia price.png")
plot(pred, showgap = FALSE, PI = FALSE, xlab = "from 2000 to 2026", ylab = "Electricity consumption (in cents/kWh)",
     main = "Georgia Electricity price Estimation, from 2000 to 2026")
dev.off()
df['Georgia'] = c(as.numeric(pred$x), as.numeric(pred$mean))

# North Carolina
adf.test(price$'North Carolina')
adf.test(diff(price$'North Carolina'))
adf.test(diff(price$'North Carolina', 2))

myts = xts(price$'North Carolina', order.by = yr)
fit = auto.arima(myts, d=0,D=1)
pred = forecast(fit, 10)
png(filename = "North Carolina price.png")
plot(pred, showgap = FALSE, PI = FALSE, xlab = "from 2000 to 2026", ylab = "Electricity consumption (in cents/kWh)",
     main = "'North Carolina' Electricity price Estimation, from 2000 to 2026")
dev.off()
df['North Carolina'] = c(as.numeric(pred$x), as.numeric(pred$mean))

# Ohio
adf.test(price$Ohio)
adf.test(diff(price$Ohio))
adf.test(diff(price$Ohio, 2))

myts = xts(price$Ohio, order.by = yr)
fit = auto.arima(myts, d=0)
fit = auto.arima(myts,seasonal = TRUE, D = 1)
png(filename = "Ohio price.png")
plot(pred, showgap = FALSE, PI = FALSE, xlab = "from 2000 to 2026", ylab = "Electricity consumption (in cents/kWh)",
     main = "Ohio Electricity price Estimation, from 2000 to 2026")
dev.off()
df['Ohio'] = c(as.numeric(pred$x), as.numeric(pred$mean))















