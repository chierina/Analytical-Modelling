library(readxl)
library(ggplot2)
library(ggfortify)
library(forecast)

SectorData <- read_excel('Table_2.1_Energy_Consumption_by_Sector.xlsx')
SectorDataTS <- ts(SectorData, start = 1973, frequency = 12)

autoplot(SectorDataTS, ts.colour = 'blue', xlab = 'Month', ylab = 'Trillion Btu')

#
ggseasonplot(SectorDataTS[, "Total Energy Consumed by the Residential Sector"], season.labels = NULL, year.labels = F, year.labels.left = F)
ggseasonplot(SectorDataTS[, "Total Energy Consumed by the Residential Sector"], polar = T, season.labels = NULL, year.labels = F, year.labels.left = F)
ggsubseriesplot(SectorDataTS[, "Total Energy Consumed by the Residential Sector"])
gglagplot(SectorDataTS[, "Total Energy Consumed by the Residential Sector"], lag = 40, nrow = NULL, ncol = NULL)

# Decomposition
components.ts <- decompose(SectorDataTS[, "Total Energy Consumed by the Residential Sector"],)
plot(components.ts)

# Remove the seasonal data
seasonal <- components.ts$seasonal
withoutSeasonal <- SectorDataTS[,"Total Energy Consumed by the Residential Sector"] - seasonal
plot(withoutSeasonal)

# Difference the data to make d = 0
ndiffs(withoutSeasonal)
withoutSeasonalAfterDiff <- diff(withoutSeasonal)
plot(withoutSeasonalAfterDiff)

# Find optimal length of p
Acf(withoutSeasonalAfterDiff) # chose 4 as p

# Find optimal length of q
Pacf(withoutSeasonalAfterDiff) # chose 5 as q

# See the quality of each models by calculating the information loss (lower is better)
AIC(arima(withoutSeasonalAfterDiff, order = c(4,0,5)), arima(withoutSeasonalAfterDiff, order = c(5,0,5)))

# Automatically train the model
# Taking the data from first column until 541
trainingModel <- auto.arima(SectorDataTS[1:541, "Total Energy Consumed by the Residential Sector"], stationary = F, seasonal = T)

testModel <- Arima(SectorDataTS[542:561, "Total Energy Consumed by the Residential Sector"], model = trainingModel)
accuracy(testModel)

# Forcast
trainingModel_2 <- auto.arima(SectorDataTS[, "Total Energy Consumed by the Residential Sector"], stationary = F, seasonal = T)
forecast <- forecast(trainingModel_2, h = 12)
plot(forecast)


