library(xts)
library(forecast)
library(tidyverse)
require(xlsx)
library(tseries)
library(rucm)

# get the data
setwd("D:/1 Study Material/US Electricity Net Generation/")
price <- read.xlsx ("data/data.xlsx", sheetName = "Average retail price cents kWh", check.names=FALSE, header = TRUE)
generation <- read.xlsx ("data/data.xlsx", sheetName = "Net generation (MWh)", check.names=FALSE, header = TRUE)

# find the top 10 Net Generation state
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
adf.test(generation$Texas) # p-value > 0.05, so we can't reject null hyphthesis: non-stationary
adf.test(diff(generation$Texas)) # p-value > 0.05, so we can't reject null hyphthesis: non-stationary
adf.test(diff(generation$Texas,2)) # p-value < 0.05, so we reject null hyphthesis: non-stationary

myts = xts(generation$Texas, order.by = yr)
fit = auto.arima(myts, d = 2,D = 1)
pred = forecast(fit, 10)
png(filename = "Texas.png")
plot(pred, showgap = FALSE, PI = FALSE, xlab = "from 2000 to 2026", ylab = "Electricity Net Generation (in Mwh)",
     main = "Texas Electricity Net Generation Estimation, from 2000 to 2026")
dev.off()
df['Texas'] = c(as.numeric(pred$x), as.numeric(pred$mean))

# Florida
adf.test(generation$Florida) # p-value > 0.05, so we can't reject null hyphthesis: non-stationary
adf.test(diff(generation$Florida)) # p-value > 0.05, so we can't reject null hyphthesis: non-stationary
adf.test(diff(generation$Florida,2)) # p-value get higher, so we can't do the differencing

myts = xts(generation$Florida, order.by = yr)
fit = auto.arima(myts, d = 1, D = 1)
pred = forecast(fit, 10)
png(filename = "Florida.png")
plot(pred, showgap = FALSE, PI = FALSE, xlab = "from 2000 to 2026", ylab = "Electricity Net Generation (in Mwh)",
     main = "Florida Electricity Net Generation Estimation, from 2000 to 2026")
dev.off()
df['Florida'] = c(as.numeric(pred$x), as.numeric(pred$mean))

# Pennsylvania
adf.test(generation$Pennsylvania) # p-value > 0.05, so we can't reject null hyphthesis: non-stationary
adf.test(diff(generation$Pennsylvania)) # p-value < 0.05, so d = 1

myts = xts(generation$Pennsylvania, order.by = yr)
fit = auto.arima(myts, d = 1, D = 2)
pred = forecast(fit, 10)
png(filename = "Pennsylvania.png")
plot(pred, showgap = FALSE, PI = FALSE, xlab = "from 2000 to 2026", ylab = "Electricity Net Generation (in Mwh)",
     main = "Pennsylvania Electricity Net Generation Estimation, from 2000 to 2026")
dev.off()
df['Pennsylvania'] = c(as.numeric(pred$x), as.numeric(pred$mean))

# California
adf.test(generation$California) # p-value > 0.05, so we can't reject null hyphthesis: non-stationary
adf.test(diff(generation$California)) # p-value > 0.05, so we can't reject null hyphthesis: non-stationary
adf.test(diff(generation$California,2)) # still larger than 0.05

myts = xts(generation$California, order.by = yr)
fit = auto.arima(myts, stationary = FALSE)
pred = forecast(fit, 10)
png(filename = "California.png")
plot(pred, showgap = FALSE, PI = FALSE, xlab = "from 2000 to 2026", ylab = "Electricity Net Generation (in Mwh)",
     main = "California Electricity Net Generation Estimation, from 2000 to 2026")
dev.off()
df['California'] = c(as.numeric(pred$x), as.numeric(pred$mean))

# Illinois
adf.test(generation$Illinois)
adf.test(diff(generation$Illinois))
adf.test(diff(generation$Illinois, 2))

myts = xts(generation$Illinois, order.by = yr)
fit = auto.arima(myts,d = 0)
pred = forecast(fit, 10)
png(filename = "Illinois.png")
plot(pred, showgap = FALSE, PI = FALSE, xlab = "from 2000 to 2026", ylab = "Electricity Net Generation (in Mwh)",
     main = "Illinois Electricity Net Generation Estimation, from 2000 to 2026")
dev.off()
df['Illinois'] = c(as.numeric(pred$x), as.numeric(pred$mean))

# Alabama
adf.test(generation$Alabama)
adf.test(diff(generation$Alabama))
adf.test(diff(generation$Alabama,2))

myts = xts(generation$Alabama, order.by = yr)
fit = auto.arima(myts, d = 0)
pred = forecast(fit, 10)
png(filename = "Alabama.png")
plot(pred, showgap = FALSE, PI = FALSE, xlab = "from 2000 to 2026", ylab = "Electricity Net Generation (in Mwh)",
     main = "Alabama Electricity Net Generation Estimation, from 2000 to 2026")
dev.off()
df['Alabama'] = c(as.numeric(pred$x), as.numeric(pred$mean))

# New York
adf.test(generation$'New York')
adf.test(diff(generation$'New York'))
adf.test(diff(generation$'New York', 2))

myts = xts(generation$'New York', order.by = yr)
fit = auto.arima(myts,d = 0)
pred = forecast(fit, 10)
png(filename = "New York.png")
plot(pred, showgap = FALSE, PI = FALSE, xlab = "from 2000 to 2026", ylab = "Electricity Net Generation (in Mwh)",
     main = "New York Electricity Net Generation Estimation, from 2000 to 2026")
dev.off()
df['New York'] = c(as.numeric(pred$x), as.numeric(pred$mean))

# Georgia
adf.test(generation$Georgia)
adf.test(diff(generation$Georgia))
adf.test(diff(generation$Georgia,2))

myts = xts(generation$Georgia, order.by = yr)
fit = auto.arima(myts, d = 0)
pred = forecast(fit, 10)
png(filename = "Georgia.png")
plot(pred, showgap = FALSE, PI = FALSE, xlab = "from 2000 to 2026", ylab = "Electricity Net Generation (in Mwh)",
     main = "Georgia Electricity Net Generation Estimation, from 2000 to 2026")
dev.off()
df['Georgia'] = c(as.numeric(pred$x), as.numeric(pred$mean))

# North Carolina
adf.test(generation$'North Carolina')
adf.test(diff(generation$'North Carolina'))
adf.test(diff(generation$'North Carolina', 2))

myts = xts(generation$'North Carolina', order.by = yr)
fit = auto.arima(myts, d=0)
pred = forecast(fit, 10)
png(filename = "North Carolina.png")
plot(pred, showgap = FALSE, PI = FALSE, xlab = "from 2000 to 2026", ylab = "Electricity Net Generation (in Mwh)",
     main = "North Carolina Electricity Net Generation Estimation, from 2000 to 2026")
dev.off()
df['North Carolina'] = c(as.numeric(pred$x), as.numeric(pred$mean))

# Ohio
adf.test(generation$Ohio)
adf.test(diff(generation$Ohio))
adf.test(diff(generation$Ohio, 3))

myts = xts(generation$Ohio, order.by = yr)
fit = auto.arima(myts, d=0)
fit = auto.arima(myts,seasonal = TRUE, D = 1)
png(filename = "Ohio.png")
plot(pred, showgap = FALSE, PI = FALSE, xlab = "from 2000 to 2026", ylab = "Electricity Net Generation (in Mwh)",
     main = "Ohio Electricity Net Generation Estimation, from 2000 to 2026")
dev.off()
df['Ohio'] = c(as.numeric(pred$x), as.numeric(pred$mean))


# y = c(as.numeric(pred$x), as.numeric(pred$mean))
# x = c(2000:2026)
# plot(x,y, col = "red")
#par(new=TRUE)
#plot(x, c(as.numeric(pred$fitted), as.numeric(pred$mean)), col = 'blue')


# pacf(generation$Alabama)
# Box.test(test$Alabama, lag=12, type = "Ljung-Box")
# 
# 
# price <- t(price)
# generation <- t(generation)
# price[1,1] <- 'year'
# generation[1,1] <- 'year'
# 
# data_price <- price[2:dim(price)[1],]
# colnames(data_price) <- price[1,]
# data_price <- as.data.frame(data_price)
# data_generation <- generation[2:dim(generation)[1],]
# colnames(data_generation) <- generation[1,]
# 
# data_price$year
# 
# price <- price[order(price$`2016`, decreasing = TRUE),]
# generation <- generation[order(generation$`2016`, decreasing = TRUE),]
# 
