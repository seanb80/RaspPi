require(TTR)

data <- read.csv("~/RaspPi/data.csv")

# Makes a graph of the temp for each tank
plot(data$field1, type = 'l')
lines(data$field2, type = 'l', col = 'red')
lines(data$field3, type = 'l', col = 'blue')
lines(data$field4, type = 'l', col = 'green')

# A bunch of subsetting and labeling of the data
tank1 <- as.data.frame(data$field1)
tank2 <- as.data.frame(data$field2)
tank3 <- as.data.frame(data$field3)
tank4 <- as.data.frame(data$field4)

tank1[,2] <- as.factor("tank1")
tank2[,2] <- as.factor("tank2")
tank3[,2] <- as.factor("tank3")
tank4[,2] <- as.factor("tank4")

colnames(tank1) <- c('temp', 'label')
colnames(tank2) <- c('temp', 'label')
colnames(tank3) <- c('temp', 'label')
colnames(tank4) <- c('temp', 'label')

# Rejoins the data for an anova
data2 <- rbind(tank1, tank2, tank3, tank4)

# An Anova
temp.aov <- aov(temp ~ label, data = data2)

# Building the time series data set
data3 <- data[,3:6]

data.ts <- ts(data3)

# Does a moving average smoothing, with a lag of 20, doesn't seem to help much though.
smooth.MA.1 <- SMA(data.ts[,1], 20)
smooth.MA.2 <- SMA(data.ts[,2], 20)
smooth.MA.3 <- SMA(data.ts[,3], 20)
smooth.MA.4 <- SMA(data.ts[,4], 20)

# Plots the smoothed moving average. Still looks ~ the same.
plot(smooth.MA.2)
lines(smooth.MA.1)
lines(smooth.MA.3)
lines(smooth.MA.4)

# Calculates the difference of the time series to attempt to achieve a stationary process
diff.1 <- diff(data.ts[,1])
diff.2 <- diff(data.ts[,2])
diff.3 <- diff(data.ts[,3])
diff.4 <- diff(data.ts[,4])


# Looks at the autocorrelation and partial autocorrelation correlograms for each differenced time series
par(mfrow = c(1,2))
acf(diff.1)
pacf(diff.1)

acf(diff.2)
pacf(diff.2)

acf(diff.3)
pacf(diff.3)

acf(diff.4)
pacf(diff.4)

# Well this all looks like crap. Autocorrelation doesn't seem to really stabilize throughout time. Hm.
par(mfrow = c(1,1))