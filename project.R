library(lubridate)
library(tseries)
library(forecast)
library(prophet)
library(timetk)
library(ggplot2)
library(cowplot)

ecommerce <- read.csv('ecommerceOR.csv', sep = ';')


ecommerce <- ecommerce[ecommerce$data != '',]

# lista settori
listSett <- unique(ecommerce$settore); listSett

df <- data.frame(Sector = character(0), NumberOfRows = numeric(0))

for (i in 1:length(listSett)) {
  sector <- listSett[i]
  num_rows <- nrow(ecommerce[ecommerce$settore == sector,])
  
  temp_df <- data.frame(Sector = sector, NumberOfRows = num_rows)
  
  df <- rbind(df, temp_df)
  if (i == 30) {
    rm(temp_df)
  }
}

# riordina per settore in ordine decrescente
df <- df[order(df$NumberOfRows, decreasing = TRUE), ]

# crea lista dei 3 settori più numerosi
newList <- df$Sector[1:3]; newList


# pesca
pesca <- ecommerce[ecommerce$settore == newList[1],];
# rimozione colonna settore, reset indice e conversione data in as.Date
pesca <- pesca[1:length(pesca)-1]
row.names(pesca) <- NULL
pesca$data <- as.Date(x = pesca$data, format = "%d-%m-%Y")

plot(pesca$data, pesca$totale, type = 'l', xlab = "Years", ylab = "Profits")

# creazione colonna di anno
pesca$anno <- year(pesca$data)

# controllo numero osservazioni per anno (369 giorni?)
for (i in unique(pesca$anno)) {
  cat(i,": ", nrow(pesca[pesca$anno == i,]),"\n")
}


# rimozione duplicati
dupPesca <- pesca[duplicated(pesca$data) | duplicated(pesca$data, fromLast = TRUE), ]

for (i in seq(from = 1, to = nrow(dupPesca), by = 2)) {
  pesca[pesca$data == dupPesca$data[i],][1,2] <- pesca[pesca$data == dupPesca$data[i],][1,2] + pesca[pesca$data == dupPesca$data[i],][2,2]
}
for (i in seq(from = 2, to = nrow(dupPesca), by = 2)) {
  pesca <- subset(pesca, !(pesca$data == pesca[pesca$data == dupPesca$data[i],][1,1] & pesca$totale == pesca[pesca$data == dupPesca$data[i],][2,2]))
}


# rimozione anno 2014 e 2015
for (i in c(2014,2015)) {
  pesca <- pesca[pesca$anno != i, ]
}

row.names(pesca) <- NULL
# ricontrollo numero osservazioni
for (i in unique(pesca$anno)) {
  cat(i,": ", nrow(pesca[pesca$anno == i,]),"\n")
}

complete_df_pesca <- data.frame("data" = seq(min(pesca$data), max(pesca$data), by = "1 day"))

pesca <- merge(complete_df_pesca, pesca, by = "data", all.x = TRUE)
pesca$totale[is.na(pesca$totale)] <- 0
pesca$anno <- year(pesca$data)

for (i in unique(pesca$anno)) {
  cat(i,": ", nrow(pesca[pesca$anno == i,]),"\n")
}

plot(pesca$data, pesca$totale, type = 'l', xlab = "Years", ylab = "Profits"); abline(h = mean(pesca$totale), col = "red", lwd = 2)
# the variance appears to be constant over time, anche la media



adf.test(pesca$totale)
# it appears that your time series does not have a stochastic trend and is stationary.


acf(pesca$totale, main = "ACF Pesca Profits")
# no white noise but seasonal pattern (7 lags)

# Correlazione positiva al lag 1: La correlazione positiva al lag 1 (0.27105828) potrebbe indicare un possibile trend.
# Un valore significativo al lag 1 può suggerire che i valori della serie temporale dipendono strettamente dai valori precedenti.
# Correlazioni significative a lag multipli: Ci sono alcune correlazioni significative a lag multipli, come al lag 2 (0.19382682) e al lag 7 (0.20977197).
# Questo potrebbe indicare la presenza di una stagionalità o di un ciclo che si ripete ad intervalli specifici

pacf(pesca$totale, main = "PACF Pesca Profits")

# Ci sono alcune correlazioni significative a lag multipli.
# Questo potrebbe indicare la presenza di una stagionalità o di un ciclo che si ripete ad intervalli specifici.
# Dopo il lag 6, le correlazioni iniziano a diminuire, indicando che potrebbe non esserci una forte dipendenza oltre questi lag.
# l'analisi del PACF suggerisce la presenza di un trend al lag 1 e la possibilità di stagionalità a lag multipli



split_index_pesca <- round(nrow(pesca[, 1:2]) * 0.8)
train_Pesca <- pesca[1:split_index_pesca, 1:2]
test_Pesca <- pesca[(split_index_pesca + 1):nrow(pesca), 1:2]
colnames(train_Pesca) <- c('ds', 'y')
colnames(test_Pesca) <- c('ds', 'y')

# Train the auto.arima model
arimaPesca <- auto.arima(train_Pesca$y)

# Use the trained model to forecast the remaining 20% of the data
forecastArimaPesca <- forecast(arimaPesca, h = nrow(test_Pesca))
combinedArimaPesca <- data.frame(
  data = c(train_Pesca$ds, test_Pesca$ds),
  totale = c(train_Pesca$y, forecastArimaPesca$mean)
)
# plot(test_Pesca$ds, test_Pesca$y, type = "l", xlab = "Data", ylab = "Totale")

# lines(test_Pesca$ds, forecastArimaPesca$mean, type = "l", col = "red")

# Plot using ggplot
ggplot() +
  geom_line(data = combinedArimaPesca, aes(x = data, y = totale), color = "blue") +
  geom_line(data = data.frame(data = test_Pesca$ds, 
                              totale = forecastArimaPesca$mean),
            aes(x = data, y = totale), color = "red") +
  geom_ribbon(data = data.frame(data = test_Pesca$ds, 
                                ymin = forecastArimaPesca$mean - 100 * sd(forecastArimaPesca$mean), 
                                ymax = forecastArimaPesca$mean + 100 * sd(forecastArimaPesca$mean)), 
              aes(x = data, ymin = ymin, ymax = ymax), fill = "red", alpha = 0.3) +
  labs(x = "Date", y = "Total") +
  theme_minimal()





rmse <- function(actual, predicted) {
  sqrt(mean((predicted - actual)^2))
}
rmseNorm <- function(actual, predicted) {
  sqrt(mean((predicted - actual)^2))/(mean(actual)) * 100
}
mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}
mdape <- function(actual, predicted) {
  median(abs((actual - predicted) / actual)) * 100
}

# RMSE
rmseArimaPesca <- rmse(test_Pesca$y, forecastArimaPesca$mean)
cat("RMSE ARIMA PESCA:", rmseArimaPesca, "\n")

# RMSE NORM
rmsePercArimaPesca <- rmseNorm(test_Pesca$y, forecastArimaPesca$mean)
cat("%RMSE ARIMA PESCA:", rmsePercArimaPesca, "%\n")

# MAPE
mapeArimaPesca <- mape(test_Pesca$y, forecastArimaPesca$mean)
cat("MAPE ARIMA PESCA:", mapeArimaPesca, "%\n")

# MdAPE
mdapeArimaPesca <- mdape(test_Pesca$y, forecastArimaPesca$mean)
cat("MdAPE ARIMA PESCA:", mdapeArimaPesca, "%\n")


# ACF Residuals
acf(forecastArimaPesca$residuals, main = "ACF Arima Pesca Residui")



# forecast, dei restanti giorni del 2023
forecastArimaPesca_new <- forecast(forecastArimaPesca, h = 233)
newDatesPesca <- seq(pesca$data[nrow(pesca)]+1, by = "1 day", length.out = 233)


# combino pesca e i restanti giorni stimati del 2023
combinedArimaPesca_new <-  data.frame(
    data = c(train_Pesca$ds, test_Pesca$ds, newDatesPesca),
    totale = c(train_Pesca$y, forecastArimaPesca$mean, forecastArimaPesca_new$mean)
)

# Plot using ggplot
ggplot() +
  geom_line(data = combinedArimaPesca_new, aes(x = data, y = totale), color = "blue") +
  geom_line(data = data.frame(data = newDatesPesca,
                              totale = forecastArimaPesca_new$mean),
            aes(x = data, y = totale), color = "red") +
  geom_ribbon(data = data.frame(data = newDatesPesca,
                                ymin = forecastArimaPesca_new$mean - 100 * sd(forecastArimaPesca_new$mean),
                                ymax = forecastArimaPesca_new$mean + 100 * sd(forecastArimaPesca_new$mean)),
              aes(x = data, ymin = ymin, ymax = ymax), fill = "red", alpha = 0.3) +
  labs(x = "Date", y = "Total") +
  theme_minimal()


par(mar = c(5, 4, 1, 1), mfrow = c(3,1))
plot(rowMeans(matrix(forecastArimaPesca_new$residuals[1:(length(forecastArimaPesca_new$residuals) %/% 365 * 365)], nrow = 365), na.rm = TRUE),
     type = "l", ylab = "Yearly", xlab = "Time")
plot(rowMeans(matrix(forecastArimaPesca_new$residuals[1:(length(forecastArimaPesca_new$residuals) %/% 30 * 30)], nrow = 30), na.rm = TRUE),
     type = "l", ylab = "Monthly", xlab = "Time")
plot(rowMeans(matrix(forecastArimaPesca_new$residuals[1:(length(forecastArimaPesca_new$residuals) %/% 7 * 7)], nrow = 7), na.rm = TRUE),
     type = "l", ylab = "Weekly", xlab = "Time")
par(mar = c(5, 4, 4, 2) + 0.1, mfrow = c(1,1))


best <- 0
end <- 0
for (i in 1:20) {
  for (o in 1:20){
    for (p in 1:20){
      if (AIC(arima(train_Pesca$y, order = c(i,o,p))) < AIC(arima(train_Pesca$y, order = c(i,o,p+1)))) {
        pescaAR_Best <- arima(train_Pesca$y, order = c(i,o,p))
        best <- p
        break
      }
    }
    if (AIC(arima(train_Pesca$y, order = c(i,o,best))) < AIC(arima(train_Pesca$y, order = c(i,o+1,best)))) {
      pescaAR_Best <- arima(train_Pesca$y, order = c(i,o,best))
      best2 <- o
      break
    }
  }
  if (AIC(arima(train_Pesca$y, order = c(i,best2,best))) < AIC(arima(train_Pesca$y, order = c(i+1,best2,best)))) {
    pescaAR_Best <- arima(train_Pesca$y, order = c(i,best2,best))
    best3 <- i
    break
  }
}
cat(paste0("\nAr(",best3,",", best2,",",best,") model:")); print(pescaAR_Best);
forecastPesca <- forecast(pescaAR_Best, h = nrow(test_Pesca))
combinedPesca <- data.frame(
  data = c(train_Pesca$ds, test_Pesca$ds),
  totale = c(train_Pesca$y, forecastPesca$mean)
)
# plot(test_Pesca$ds, test_Pesca$y, type = "l", xlab = "Data", ylab = "Totale")

# lines(test_Pesca$ds, forecastArimaPesca$mean, type = "l", col = "red")

# Plot using ggplot
ggplot() +
  geom_line(data = combinedPesca, aes(x = data, y = totale), color = "blue") +
  geom_line(data = data.frame(data = test_Pesca$ds, 
                              totale = forecastPesca$mean),
            aes(x = data, y = totale), color = "red") +
  geom_ribbon(data = data.frame(data = test_Pesca$ds, 
                                ymin = forecastPesca$mean - 100 * sd(forecastPesca$mean), 
                                ymax = forecastPesca$mean + 100 * sd(forecastPesca$mean)), 
              aes(x = data, ymin = ymin, ymax = ymax), fill = "red", alpha = 0.3) +
  labs(x = "Date", y = "Total") +
  theme_minimal()






# Modello Prophet
# Test Modello
# Inizializza il modello
modello_prophet_pesca <- prophet()

# Addestra il modello sul set di addestramento
modello_prophet_pesca <- fit.prophet(modello_prophet_pesca, train_Pesca)

# Crea un dataframe per le previsioni future
futuro_pesca <- data.frame(ds = test_Pesca$ds)

# Ottieni le previsioni
previsioni_pesca <- predict(modello_prophet_pesca, futuro_pesca)


# RMSE
rmseProphetPesca <- rmse(test_Pesca$y, previsioni_pesca$yhat)
cat("RMSE PROPHET PESCA:", rmseProphetPesca, "\n")

# RMSE NORM
rmsePercProphetPesca <- rmseNorm(test_Pesca$y, previsioni_pesca$yhat)
cat("%RMSE PROPHET PESCA:", rmsePercProphetPesca, "%\n")

# MAPE
mapeProphetPesca <- mape(test_Pesca$y, previsioni_pesca$yhat)
cat("MAPE PROPHET PESCA:", mapeProphetPesca, "%\n")

# MdAPE
mdapeProphetPesca <- mdape(test_Pesca$y, previsioni_pesca$yhat)
cat("MdAPE PROPHET PESCA:", mdapeProphetPesca, "%\n")




# Previsione
tempPesca <- pesca[, 1:2]
colnames(tempPesca) <- c('ds','y')

# Inizializza il modello Prophet
modello_prophet_pr_pesca <- prophet()

# Addestra il modello con i dati esistenti
modello_prophet_pr_pesca <- fit.prophet(modello_prophet_pr_pesca, tempPesca)

# Crea un dataframe per le previsioni future
futuro_pesca_pr <- make_future_dataframe(modello_prophet_pr_pesca, periods = 232)

# Ottieni solo le previsioni future
previsioni_prophet_pr_pesca <- predict(modello_prophet_pr_pesca, futuro_pesca_pr)
previsioni_futures_pr_pesca <- subset(previsioni_prophet_pr_pesca, ds > max(tempPesca$ds))

# grafico

#tempPesca$ds <- as.Date(tempPesca$ds)
previsioni_futures_pr_pesca$ds <- as.Date(previsioni_futures_pr_pesca$ds)

ggplot() +
  geom_line(data = tempPesca, aes(x = ds, y = y), color = "blue") +
  geom_ribbon(data = previsioni_futures_pr_pesca, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = "red", alpha = 0.3) +
  geom_line(data = previsioni_futures_pr_pesca, aes(x = ds, y = yhat), color = "red") +
  ggtitle("Previsioni Pesca") +
  theme(plot.title = element_text(hjust = 0.5))

#grafico trend annuale, settimanale e mensile

prophet_plot_components(modello_prophet_pr_pesca, previsioni_prophet_pr_pesca)





# TBATS
# Fit TBATS model on the training set
modello_tbats_pesca <- tbats(train_Pesca$y, seasonal.periods = c(7, 365))  # Adjust seasonal periods as needed

# Check residuals
acf(modello_tbats_pesca$errors)

# Forecast using the fitted model on the test set for the remaining days of 2023
forecastTbatsPesca <- forecast(modello_tbats_pesca, h = nrow(test_Pesca))

# Combine original data and forecasted values for the remaining days of 2023
combinedTbatsPesca <- data.frame(
  data = c(train_Pesca$ds, test_Pesca$ds),
  totale = c(train_Pesca$y, forecastTbatsPesca$mean)
)

# Plot using ggplot
ggplot() +
  geom_line(data = combinedTbatsPesca, aes(x = data, y = totale), color = "blue") +
  geom_line(data = data.frame(data = test_Pesca$ds, 
                              totale = forecastTbatsPesca$mean),
            aes(x = data, y = totale), color = "red") +
  geom_ribbon(data = data.frame(data = test_Pesca$ds, 
                                ymin = forecastTbatsPesca$mean - sd(forecastTbatsPesca$mean), 
                                ymax = forecastTbatsPesca$mean + sd(forecastTbatsPesca$mean)), 
              aes(x = data, ymin = ymin, ymax = ymax), fill = "red", alpha = 0.3) +
  labs(x = "Date", y = "Total") +
  theme_minimal()

# RMSE
rmseTbatsPesca <- rmse(test_Pesca$y, forecastTbatsPesca$mean)
cat("RMSE TBATS PESCA:", rmseTbatsPesca, "\n")

# RMSE NORM
rmsePercTbatsPesca <- rmseNorm(test_Pesca$y, forecastTbatsPesca$mean)
cat("%RMSE TBATS PESCA:", rmsePercTbatsPesca, "%\n")

# MAPE
mapeTbatsPesca <- mape(test_Pesca$y, forecastTbatsPesca$mean)
cat("MAPE TBATS PESCA:", mapeTbatsPesca, "%\n")

# MdAPE
mdapeTbatsPesca <- mdape(test_Pesca$y, forecastTbatsPesca$mean)
cat("MdAPE TBATS PESCA:", mdapeTbatsPesca, "%\n")





# TBATS Extension and Analysis

# Create a Time Series Object for the Entire Dataset
pesca_ts <- ts(pesca$totale, frequency = 7)  # Assuming weekly seasonality

# Re-fit the TBATS Model
model_tbats_extended_pesca <- tbats(pesca_ts, seasonal.periods = c(7, 365))  # Adjust seasonal periods as needed

last_day_pesca <- tail(pesca$data, 1)
diff_days_pesca <- 365 - as.numeric(difftime(last_day_pesca+2, as.Date(paste0(as.numeric(format((last_day_pesca+1), "%Y")),"-01-01")), units = "days"))

# Create an extended time series for forecasting
extended_time_series_pesca <- ts(rep(NA, diff_days_pesca), frequency = 7, start = c(as.numeric(format((last_day_pesca+1), "%Y")),
                                                                  as.numeric(format((last_day_pesca+1), "%m")),
                                                                  as.numeric(format((last_day_pesca+1), "%d"))))

# Forecast for the Remaining Days of 2023
forecast_tbats_extended_pesca <- forecast(model_tbats_extended_pesca, h = diff_days_pesca, newdata = extended_time_series_pesca)

# Plot the Extended Forecast
plot(forecast_tbats_extended_pesca, xlab = "Date", ylab = "Totale", main = "Extended Forecast for 2023 - TBATS Model")



par(mar = c(5, 4, 1, 1), mfrow = c(3,1))
plot(rowMeans(matrix(forecast_tbats_extended_pesca$residuals[1:(length(forecast_tbats_extended_pesca$residuals) %/% 365 * 365)], nrow = 365), na.rm = TRUE),
     xlab = "Time", ylab = "Yearly", type = "l")
plot(rowMeans(matrix(forecast_tbats_extended_pesca$residuals[1:(length(forecast_tbats_extended_pesca$residuals) %/% 30 * 30)], nrow = 30), na.rm = TRUE),
     xlab = "Time", ylab = "Monthly", type = "l")
plot(rowMeans(matrix(forecast_tbats_extended_pesca$residuals[1:(length(forecast_tbats_extended_pesca$residuals) %/% 7 * 7)], nrow = 7), na.rm = TRUE),
     xlab = "Time", ylab = "Weekly", type = "l")
par(mar = c(5, 4, 4, 2) + 0.1, mfrow = c(1,1))







# confronto indicatori
comparisonPesca <- data.frame(
  Modello = c("Arima", "Prophet", "Tbats"),
  RMSE = c(rmseArimaPesca, rmseProphetPesca, rmseTbatsPesca),
  `%RMSE` = sprintf("%.2f%%", c(rmsePercArimaPesca, rmsePercProphetPesca, rmsePercTbatsPesca)),
  MAPE = sprintf("%.2f%%", c(mapeArimaPesca, mapeProphetPesca, mapeTbatsPesca)),
  MDAPE = sprintf("%.2f%%", c(mdapeArimaPesca, mdapeProphetPesca, mdapeTbatsPesca)),
  check.names = FALSE
); comparisonPesca








# calcio
calcio <- ecommerce[ecommerce$settore == newList[2],];
calcio <- calcio[1:length(calcio)-1]
row.names(calcio) <- NULL
calcio$data <- as.Date(x = calcio$data, format = "%d-%m-%Y")

plot(calcio$data, calcio$totale, type = 'l', xlab = "Years", ylab = "Profits")

calcio$anno <- year(calcio$data)

# controllo numero osservazioni per anno
for (i in unique(calcio$anno)) {
  cat(i,": ", nrow(calcio[calcio$anno == i,]),"\n")
}


# rimozione duplicati
dupCalcio <- calcio[duplicated(calcio$data) | duplicated(calcio$data, fromLast = TRUE), ]

for (i in seq(from = 1, to = nrow(dupCalcio), by = 2)) {
  calcio[calcio$data == dupCalcio$data[i],][1,2] <- calcio[calcio$data == dupCalcio$data[i],][1,2] + calcio[calcio$data == dupCalcio$data[i],][2,2]
}
for (i in seq(from = 2, to = nrow(dupCalcio), by = 2)) {
  calcio <- subset(calcio, !(calcio$data == calcio[calcio$data == dupCalcio$data[i],][1,1] & calcio$totale == calcio[calcio$data == dupCalcio$data[i],][2,2]))
}

for (i in unique(calcio$anno)) {
  cat(i,": ", nrow(calcio[calcio$anno == i,]),"\n")
}

# rimozione anno 2013, 2014 e 2015
for (i in c(2013, 2014, 2015)) {
  calcio <- calcio[calcio$anno != i, ]
}


row.names(calcio) <- NULL
# ricontrollo numero osservazioni per anno
for (i in unique(calcio$anno)) {
  cat(i,": ", nrow(calcio[calcio$anno == i,]),"\n")
}

complete_df_calcio <- data.frame("data" = seq(min(calcio$data), max(calcio$data), by = "1 day"))

calcio <- merge(complete_df_calcio, calcio, by = "data", all.x = TRUE)
calcio$totale[is.na(calcio$totale)] <- 0
calcio$anno <- year(calcio$data)

for (i in unique(calcio$anno)) {
  cat(i,": ", nrow(calcio[calcio$anno == i,]),"\n")
}


plot(calcio$data, calcio$totale, type = 'l', xlab = "Years", ylab = "Profits"); abline(h = mean(calcio$totale), col = "red", lwd = 2)
# mean constant

adf.test(calcio$totale)
# it appears that your time series does not have a stochastic trend and is stationary.

acf(calcio$totale) # no white noise, trend and seasonality
# Dal tuo output, sembra che la correlazione diminuisca man mano che aumenta il ritardo (lag).
# L'ACF mostra valori significativi ai primi ritardi, ma diminuisce man mano che ci si allontana da zero.
# Questo può indicare la presenza di un possibile "trend" o stagionalità nella serie temporale.
# La presenza di valori significativi a intervalli regolari potrebbe indicare la presenza di una stagionalità nella serie temporale.
pacf(calcio$totale)
# c'è un trend nelle prime osservazioni della serie temporale,ma questo trend perde importanza man mano che ci si sposta in avanti nel tempo.
# sembra esserci una correlazione significativa con il giorno precedente e il giorno prima di quello,
# suggerendo un possibile modello di trend giornaliero o una struttura stagionale a intervalli di due giorni.

# Quindi, in sintesi:
# - Trend: Il test di Dickey-Fuller suggerisce che la serie temporale è stazionaria, indicando l'assenza di un trend significativo.
# - Stagionalità: Gli ACF e PACF mostrano evidenze di correlazione nei primi due ritardi, suggerendo una possibile stagionalità nella serie temporale.


split_index_calcio <- round(nrow(calcio[, 1:2]) * 0.8)
train_Calcio <- calcio[1:split_index_calcio, 1:2]
test_Calcio <- calcio[(split_index_calcio + 1):nrow(calcio), 1:2]
colnames(train_Calcio) <- c('ds', 'y')
colnames(test_Calcio) <- c('ds', 'y')

# Train the auto.arima model
arimaCalcio <- auto.arima(train_Calcio$y)

# Use the trained model to forecast the remaining 20% of the data
forecastArimaCalcio <- forecast(arimaCalcio, h = nrow(test_Calcio))
combinedArimaCalcio <- data.frame(
  data = c(train_Calcio$ds, test_Calcio$ds),
  totale = c(train_Calcio$y, forecastArimaCalcio$mean)
)


#plot(test_Calcio$ds, test_Calcio$y, type = "l", xlab = "Data", ylab = "Totale")
#lines(test_Calcio$ds, forecastArimaCalcio$mean, type = "l", col = "red")

ggplot() +
  geom_line(data = combinedArimaCalcio, aes(x = data, y = totale), color = "blue") +
  geom_line(data = data.frame(data = test_Calcio$ds, 
                              totale = forecastArimaCalcio$mean),
            aes(x = data, y = totale), color = "red") +
  geom_ribbon(data = data.frame(data = test_Calcio$ds, 
                                ymin = forecastArimaCalcio$mean - 200 * sd(forecastArimaCalcio$mean), 
                                ymax = forecastArimaCalcio$mean + 200 * sd(forecastArimaCalcio$mean)), 
              aes(x = data, ymin = ymin, ymax = ymax), fill = "red", alpha = 0.3) +
  labs(x = "Date", y = "Total") +
  theme_minimal()



# RMSE
rmseArimaCalcio <- rmse(test_Calcio$y, forecastArimaCalcio$mean)
cat("RMSE ARIMA CALCIO:", rmseArimaCalcio, "\n")

# RMSE NORM
rmsePercArimaCalcio <- rmseNorm(test_Calcio$y, forecastArimaCalcio$mean)
cat("%RMSE ARIMA CALCIO:", rmsePercArimaCalcio, "%\n")

# MAPE
mapeArimaCalcio <- mape(test_Calcio$y, forecastArimaCalcio$mean)
cat("MAPE ARIMA CALCIO:", mapeArimaCalcio, "%\n")

# MdAPE
mdapeArimaCalcio <- mdape(test_Calcio$y, forecastArimaCalcio$mean)
cat("MdAPE ARIMA CALCIO:", mdapeArimaCalcio, "%\n")


# ACF Residuals
acf(forecastArimaCalcio$residuals, main = "ACF Arima Calcio Residui")





# forecast, dei restanti giorni del 2023
forecastArimaCalcio_new <- forecast(forecastArimaCalcio, h = 233)
newDatesCalcio <- seq(calcio$data[nrow(calcio)]+1, by = "1 day", length.out = 233)


# combino pesca e i restanti giorni stimati del 2023
combinedArimaCalcio_new <-  data.frame(
  data = c(train_Calcio$ds, test_Calcio$ds, newDatesCalcio),
  totale = c(train_Calcio$y, forecastArimaCalcio$mean, forecastArimaCalcio_new$mean)
)

# Plot using ggplot
ggplot() +
  geom_line(data = combinedArimaCalcio_new, aes(x = data, y = totale), color = "blue") +
  geom_line(data = data.frame(data = newDatesCalcio,
                              totale = forecastArimaCalcio_new$mean),
            aes(x = data, y = totale), color = "red") +
  geom_ribbon(data = data.frame(data = newDatesCalcio,
                                ymin = forecastArimaCalcio_new$mean - 100 * sd(forecastArimaCalcio_new$mean),
                                ymax = forecastArimaCalcio_new$mean + 100 * sd(forecastArimaCalcio_new$mean)),
              aes(x = data, ymin = ymin, ymax = ymax), fill = "red", alpha = 0.3) +
  labs(x = "Date", y = "Total") +
  theme_minimal()


par(mar = c(5, 4, 1, 1), mfrow = c(3,1))
plot(rowMeans(matrix(forecastArimaCalcio_new$residuals[1:(length(forecastArimaCalcio_new$residuals) %/% 365 * 365)], nrow = 365), na.rm = TRUE),
     type = "l", ylab = "Yearly", xlab = "Time")
plot(rowMeans(matrix(forecastArimaCalcio_new$residuals[1:(length(forecastArimaCalcio_new$residuals) %/% 30 * 30)], nrow = 30), na.rm = TRUE),
     type = "l", ylab = "Monthly", xlab = "Time")
plot(rowMeans(matrix(forecastArimaCalcio_new$residuals[1:(length(forecastArimaCalcio_new$residuals) %/% 7 * 7)], nrow = 7), na.rm = TRUE),
     type = "l", ylab = "Weekly", xlab = "Time")
par(mar = c(5, 4, 4, 2) + 0.1, mfrow = c(1,1))



# Modello Prophet
# Test Modello
# Inizializza il modello
modello_prophet_calcio <- prophet()

# Addestra il modello sul set di addestramento
modello_prophet_calcio <- fit.prophet(modello_prophet_calcio, train_Calcio)

# Crea un dataframe per le previsioni future
futuro_calcio <- data.frame(ds = test_Calcio$ds)

# Ottieni le previsioni
previsioni_calcio <- predict(modello_prophet_calcio, futuro_calcio)


# RMSE
rmseProphetCalcio <- rmse(test_Calcio$y, previsioni_calcio$yhat)
cat("RMSE PROPHET CALCIO:", rmseProphetCalcio, "\n")

# RMSE NORM
rmsePercProphetCalcio <- rmseNorm(test_Calcio$y, previsioni_calcio$yhat)
cat("%RMSE PROPHET CALCIO:", rmsePercProphetCalcio, "%\n")

# MAPE
mapeProphetCalcio <- mape(test_Calcio$y, previsioni_calcio$yhat)
cat("MAPE PROPHET CALCIO:", mapeProphetCalcio, "%\n")

# MdAPE
mdapeProphetCalcio <- mdape(test_Calcio$y, previsioni_calcio$yhat)
cat("MdAPE PROPHET CALCIO:", mdapeProphetCalcio, "%\n")


# Previsione
tempCalcio <- calcio[, 1:2]
colnames(tempCalcio) <- c('ds','y')

# Inizializza il modello Prophet
modello_prophet_pr_calcio <- prophet()

# Addestra il modello con i dati esistenti
modello_prophet_pr_calcio <- fit.prophet(modello_prophet_pr_calcio, tempCalcio)

# Crea un dataframe per le previsioni future
futuro_calcio_pr <- make_future_dataframe(modello_prophet_pr_calcio, periods = 232)

# Ottieni solo le previsioni future
previsioni_prophet_pr_calcio <- predict(modello_prophet_pr_calcio, futuro_calcio_pr)
previsioni_futures_pr_calcio <- subset(previsioni_prophet_pr_calcio, ds > max(tempCalcio$ds))

# grafico

#tempCalcio$ds <- as.Date(tempCalcio$ds)
previsioni_futures_pr_calcio$ds <- as.Date(previsioni_futures_pr_calcio$ds)

ggplot() +
  geom_line(data = tempCalcio, aes(x = ds, y = y), color = "blue") +
  geom_ribbon(data = previsioni_futures_pr_calcio, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = "red", alpha = 0.3) +
  geom_line(data = previsioni_futures_pr_calcio, aes(x = ds, y = yhat), color = "red") +
  ggtitle("Previsioni Pesca") +
  theme(plot.title = element_text(hjust = 0.5))

#grafico trend annuale, settimanale e mensile
prophet_plot_components(modello_prophet_pr_calcio, previsioni_prophet_pr_calcio)




# TBATS
# Fit TBATS model on the training set
modello_tbats_calcio <- tbats(train_Calcio$y, seasonal.periods = c(7, 365))  # Adjust seasonal periods as needed

# Check residuals
acf(modello_tbats_calcio$errors)

# Forecast using the fitted model on the test set for the remaining days of 2023
forecastTbatsCalcio <- forecast(modello_tbats_calcio, h = nrow(test_Calcio))

# Combine original data and forecasted values for the remaining days of 2023
combinedTbatsCalcio <- data.frame(
  data = c(train_Calcio$ds, test_Calcio$ds),
  totale = c(train_Calcio$y, forecastTbatsCalcio$mean)
)

# Plot using ggplot
ggplot() +
  geom_line(data = combinedTbatsCalcio, aes(x = data, y = totale), color = "blue") +
  geom_line(data = data.frame(data = test_Calcio$ds, 
                              totale = forecastTbatsCalcio$mean),
            aes(x = data, y = totale), color = "red") +
  geom_ribbon(data = data.frame(data = test_Calcio$ds, 
                                ymin = forecastTbatsCalcio$mean - sd(forecastTbatsCalcio$mean), 
                                ymax = forecastTbatsCalcio$mean + sd(forecastTbatsCalcio$mean)), 
              aes(x = data, ymin = ymin, ymax = ymax), fill = "red", alpha = 0.3) +
  labs(x = "Date", y = "Total") +
  theme_minimal()

# RMSE
rmseTbatsCalcio <- rmse(test_Calcio$y, forecastTbatsCalcio$mean)
cat("RMSE TBATS CALCIO:", rmseTbatsCalcio, "\n")

# RMSE NORM
rmsePercTbatsCalcio <- rmseNorm(test_Calcio$y, forecastTbatsCalcio$mean)
cat("%RMSE TBATS CALCIO:", rmsePercTbatsCalcio, "%\n")

# MAPE
mapeTbatsCalcio <- mape(test_Calcio$y, forecastTbatsCalcio$mean)
cat("MAPE TBATS CALCIO:", mapeTbatsCalcio, "%\n")

# MdAPE
mdapeTbatsCalcio <- mdape(test_Calcio$y, forecastTbatsCalcio$mean)
cat("MdAPE TBATS CALCIO:", mdapeTbatsCalcio, "%\n")



# TBATS Extension and Analysis

# Create a Time Series Object for the Entire Dataset
calcio_ts <- ts(calcio$totale, frequency = 7)  # Assuming weekly seasonality

# Re-fit the TBATS Model
model_tbats_extended_calcio <- tbats(calcio_ts, seasonal.periods = c(7, 365))  # Adjust seasonal periods as needed

last_day_calcio <- tail(calcio$data, 1)
diff_days_calcio <- 365 - as.numeric(difftime(last_day_calcio+2, as.Date(paste0(as.numeric(format((last_day_calcio+1), "%Y")),"-01-01")), units = "days"))

# Create an extended time series for forecasting
extended_time_series_calcio <- ts(rep(NA, diff_days_calcio), frequency = 7, start = c(as.numeric(format((last_day_calcio+1), "%Y")),
                                                                                    as.numeric(format((last_day_calcio+1), "%m")),
                                                                                    as.numeric(format((last_day_calcio+1), "%d"))))

# Forecast for the Remaining Days of 2023
forecast_tbats_extended_calcio <- forecast(model_tbats_extended_calcio, h = diff_days_calcio, newdata = extended_time_series_calcio)

# Plot the Extended Forecast
plot(forecast_tbats_extended_calcio, xlab = "Date", ylab = "Totale", main = "Extended Forecast for 2023 - TBATS Model")



par(mar = c(5, 4, 1, 1), mfrow = c(3,1))
plot(rowMeans(matrix(forecast_tbats_extended_calcio$residuals[1:(length(forecast_tbats_extended_calcio$residuals) %/% 365 * 365)], nrow = 365), na.rm = TRUE),
     xlab = "Time", ylab = "Yearly", type = "l")
plot(rowMeans(matrix(forecast_tbats_extended_calcio$residuals[1:(length(forecast_tbats_extended_calcio$residuals) %/% 30 * 30)], nrow = 30), na.rm = TRUE),
     xlab = "Time", ylab = "Monthly", type = "l")
plot(rowMeans(matrix(forecast_tbats_extended_calcio$residuals[1:(length(forecast_tbats_extended_calcio$residuals) %/% 7 * 7)], nrow = 7), na.rm = TRUE),
     xlab = "Time", ylab = "Weekly", type = "l")
par(mar = c(5, 4, 4, 2) + 0.1, mfrow = c(1,1))



# confronto indicatori
comparisonCalcio <- data.frame(
  Modello = c("Arima", "Prophet", "Tbats"),
  RMSE = c(rmseArimaCalcio, rmseProphetCalcio, rmseTbatsCalcio),
  `%RMSE` = sprintf("%.2f%%", c(rmsePercArimaCalcio, rmsePercProphetCalcio, rmsePercTbatsCalcio)),
  MAPE = sprintf("%.2f%%", c(mapeArimaCalcio, mapeProphetCalcio, mapeTbatsCalcio)),
  MDAPE = sprintf("%.2f%%", c(mdapeArimaCalcio, mdapeProphetCalcio, mdapeTbatsCalcio)),
  check.names = FALSE
); comparisonCalcio











# casual
casual <- ecommerce[ecommerce$settore == newList[3],];
casual <- casual[1:length(casual)-1]
row.names(casual) <- NULL
casual$data <- as.Date(x = casual$data, format = "%d-%m-%Y")

plot(casual$data, casual$totale, type = 'l', xlab = "Years", ylab = "Profits")

casual$anno <- year(casual$data)

# controllo numero osservazioni per anno
for (i in unique(casual$anno)) {
  cat(i,": ", nrow(casual[casual$anno == i,]),"\n")
}


# rimozione duplicati
dupCasual <- casual[duplicated(casual$data) | duplicated(casual$data, fromLast = TRUE), ]

for (i in seq(from = 1, to = nrow(dupCasual), by = 2)) {
  casual[casual$data == dupCasual$data[i],][1,2] <- casual[casual$data == dupCasual$data[i],][1,2] + casual[casual$data == dupCasual$data[i],][2,2]
}
for (i in seq(from = 2, to = nrow(dupCasual), by = 2)) {
  casual <- subset(casual, !(casual$data == casual[casual$data == dupCasual$data[i],][1,1] & casual$totale == casual[casual$data == dupCasual$data[i],][2,2]))
}

for (i in unique(casual$anno)) {
  cat(i,": ", nrow(casual[casual$anno == i,]),"\n")
}

# rimozione anno 2015
for (i in c(2015)) {
  casual <- casual[casual$anno != i, ]
}



row.names(casual) <- NULL
# ricontrollo numero osservazioni per anno
for (i in unique(casual$anno)) {
  cat(i,": ", nrow(casual[casual$anno == i,]),"\n")
}


complete_df_casual <- data.frame("data" = seq(min(casual$data), max(casual$data), by = "1 day"))

casual <- merge(complete_df_casual, casual, by = "data", all.x = TRUE)
casual$totale[is.na(casual$totale)] <- 0
casual$anno <- year(casual$data)

for (i in unique(casual$anno)) {
  cat(i,": ", nrow(casual[casual$anno == i,]),"\n")
}


plot(casual$data, casual$totale, type = 'l', xlab = "Years", ylab = "Profits"); abline(h = mean(casual$totale), col = "red", lwd = 2)
# mean constant

adf.test(casual$totale)

acf(casual$totale) # no white noise
# il primo ritardo è 0.60, il che potrebbe indicare una forte correlazione con il ritardo precedente, suggerendo un trend
# i ritardi 11 e 22 mostrano valori significativi, suggerendo un possibile ciclo di stagionalità di 11 o 22 periodi.

pacf(casual$totale)
# dalla PACF sembra che ci sia una correlazione significativa almeno fino al primo ritardo, suggerendo la presenza di un possibile trend.
# La PACF suggerisce anche la possibilità di stagionalità o altri pattern significativi a ritardi intermedi.
# effettuare una differenziazione non è consigliato. Se la differenziazione porta a una serie temporale che appare come una white noise,
# potrebbe indicare che la serie originale era già stazionaria o che il modello di differenziazione applicato è troppo forte.



split_index_casual <- round(nrow(casual[, 1:2]) * 0.8)
train_Casual <- casual[1:split_index_casual, 1:2]
test_Casual <- casual[(split_index_casual + 1):nrow(casual), 1:2]
colnames(train_Casual) <- c('ds', 'y')
colnames(test_Casual) <- c('ds', 'y')

# Train the auto.arima model
arimaCasual <- auto.arima(train_Casual$y)

# Use the trained model to forecast the remaining 20% of the data
forecastArimaCasual <- forecast(arimaCasual, h = nrow(test_Casual))
combinedArimaCasual <- data.frame(
  data = c(train_Casual$ds, test_Casual$ds),
  totale = c(train_Casual$y, forecastArimaCasual$mean)
)


#plot(test_Casual$ds, test_Casual$y, type = "l", xlab = "Data", ylab = "Totale")
#lines(test_Casual$ds, forecastArimaCasual$mean, type = "l", col = "red")

ggplot() +
  geom_line(data = combinedArimaCasual, aes(x = data, y = totale), color = "blue") +
  geom_line(data = data.frame(data = test_Casual$ds, 
                              totale = forecastArimaCasual$mean),
            aes(x = data, y = totale), color = "red") +
  geom_ribbon(data = data.frame(data = test_Casual$ds, 
                                ymin = forecastArimaCasual$mean - 150 * sd(forecastArimaCasual$mean), 
                                ymax = forecastArimaCasual$mean + 150 * sd(forecastArimaCasual$mean)), 
              aes(x = data, ymin = ymin, ymax = ymax), fill = "red", alpha = 0.3) +
  labs(x = "Date", y = "Total") +
  theme_minimal()

# RMSE
rmseArimaCasual <- rmse(test_Casual$y, forecastArimaCasual$mean)
cat("RMSE ARIMA CASUAL:", rmseArimaCasual, "\n")

# RMSE NORM
rmsePercArimaCasual <- rmseNorm(test_Casual$y, forecastArimaCasual$mean)
cat("%RMSE ARIMA CASUAL:", rmsePercArimaCasual, "%\n")

# MAPE
mapeArimaCasual <- mape(test_Casual$y, forecastArimaCasual$mean)
cat("MAPE ARIMA CASUAL:", mapeArimaCasual, "%\n")

# MdAPE
mdapeArimaCasual <- mdape(test_Casual$y, forecastArimaCasual$mean)
cat("MdAPE ARIMA CASUAL:", mdapeArimaCasual, "%\n")

# ACF Residuals
acf(forecastArimaCasual$residuals, main = "ACF Arima Casual Residui")






# forecast, dei restanti giorni del 2023
forecastArimaCasual_new <- forecast(forecastArimaCasual, h = 242)
newDatesCasual <- seq(casual$data[nrow(casual)]+1, by = "1 day", length.out = 242)


# combino pesca e i restanti giorni stimati del 2023
combinedArimaCasual_new <-  data.frame(
  data = c(train_Casual$ds, test_Casual$ds, newDatesCasual),
  totale = c(train_Casual$y, forecastArimaCasual$mean, forecastArimaCasual_new$mean)
)

# Plot using ggplot
ggplot() +
  geom_line(data = combinedArimaCasual_new, aes(x = data, y = totale), color = "blue") +
  geom_line(data = data.frame(data = newDatesCasual,
                              totale = forecastArimaCasual_new$mean),
            aes(x = data, y = totale), color = "red") +
  geom_ribbon(data = data.frame(data = newDatesCasual,
                                ymin = forecastArimaCasual_new$mean - 100 * sd(forecastArimaCasual_new$mean),
                                ymax = forecastArimaCasual_new$mean + 100 * sd(forecastArimaCasual_new$mean)),
              aes(x = data, ymin = ymin, ymax = ymax), fill = "red", alpha = 0.3) +
  labs(x = "Date", y = "Total") +
  theme_minimal()



par(mar = c(5, 4, 1, 1), mfrow = c(3,1))
plot(rowMeans(matrix(forecastArimaCasual_new$residuals[1:(length(forecastArimaCasual_new$residuals) %/% 365 * 365)], nrow = 365), na.rm = TRUE),
     type = "l", ylab = "Yearly", xlab = "Time")
plot(rowMeans(matrix(forecastArimaCasual_new$residuals[1:(length(forecastArimaCasual_new$residuals) %/% 30 * 30)], nrow = 30), na.rm = TRUE),
     type = "l", ylab = "Monthly", xlab = "Time")
plot(rowMeans(matrix(forecastArimaCasual_new$residuals[1:(length(forecastArimaCasual_new$residuals) %/% 7 * 7)], nrow = 7), na.rm = TRUE),
     type = "l", ylab = "Weekly", xlab = "Time")
par(mar = c(5, 4, 4, 2) + 0.1, mfrow = c(1,1))





# Modello Prophet
# Test Modello
# Inizializza il modello
modello_prophet_casual <- prophet()

# Addestra il modello sul set di addestramento
modello_prophet_casual <- fit.prophet(modello_prophet_casual, train_Casual)

# Crea un dataframe per le previsioni future
futuro_casual <- data.frame(ds = test_Casual$ds)

# Ottieni le previsioni
previsioni_casual <- predict(modello_prophet_casual, futuro_casual)


# RMSE
rmseProphetCasual <- rmse(test_Casual$y, previsioni_casual$yhat)
cat("RMSE PROPHET CASUAL:", rmseProphetCasual, "\n")

# RMSE NORM
rmsePercProphetCasual <- rmseNorm(test_Casual$y, previsioni_casual$yhat)
cat("%RMSE PROPHET CASUAL:", rmsePercProphetCasual, "%\n")

# MAPE
mapeProphetCasual <- mape(test_Casual$y, previsioni_casual$yhat)
cat("MAPE PROPHET CASUAL:", mapeProphetCasual, "%\n")

# MdAPE
mdapeProphetCasual <- mdape(test_Casual$y, previsioni_casual$yhat)
cat("MdAPE PROPHET CASUAL:", mdapeProphetCasual, "%\n")


# Previsione
tempCasual <- casual[, 1:2]
colnames(tempCasual) <- c('ds','y')

# Inizializza il modello Prophet
modello_prophet_pr_casual <- prophet()

# Addestra il modello con i dati esistenti
modello_prophet_pr_casual <- fit.prophet(modello_prophet_pr_casual, tempCasual)

# Crea un dataframe per le previsioni future
futuro_casual_pr <- make_future_dataframe(modello_prophet_pr_casual, periods = 232)

# Ottieni solo le previsioni future
previsioni_prophet_pr_casual <- predict(modello_prophet_pr_casual, futuro_casual_pr)
previsioni_futures_pr_casual <- subset(previsioni_prophet_pr_casual, ds > max(tempCasual$ds))

# grafico

#tempCasual$ds <- as.Date(tempCasual$ds)
previsioni_futures_pr_casual$ds <- as.Date(previsioni_futures_pr_casual$ds)

ggplot() +
  geom_line(data = tempCasual, aes(x = ds, y = y), color = "blue") +
  geom_ribbon(data = previsioni_futures_pr_casual, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = "red", alpha = 0.3) +
  geom_line(data = previsioni_futures_pr_casual, aes(x = ds, y = yhat), color = "red") +
  ggtitle("Previsioni Pesca") +
  theme(plot.title = element_text(hjust = 0.5))

#grafico trend annuale, settimanale e mensile
prophet_plot_components(modello_prophet_pr_casual, previsioni_prophet_pr_casual)



# TBATS
# Fit TBATS model on the training set
modello_tbats_casual <- tbats(train_Casual$y, seasonal.periods = c(7, 365))  # Adjust seasonal periods as needed

# Check residuals
acf(modello_tbats_casual$errors)

# Forecast using the fitted model on the test set for the remaining days of 2023
forecastTbatsCasual <- forecast(modello_tbats_casual, h = nrow(test_Casual))

# Combine original data and forecasted values for the remaining days of 2023
combinedTbatsCasual <- data.frame(
  data = c(train_Casual$ds, test_Casual$ds),
  totale = c(train_Casual$y, forecastTbatsCasual$mean)
)

# Plot using ggplot
ggplot() +
  geom_line(data = combinedTbatsCasual, aes(x = data, y = totale), color = "blue") +
  geom_line(data = data.frame(data = test_Casual$ds, 
                              totale = forecastTbatsCasual$mean),
            aes(x = data, y = totale), color = "red") +
  geom_ribbon(data = data.frame(data = test_Casual$ds, 
                                ymin = forecastTbatsCasual$mean - 40 * sd(forecastTbatsCasual$mean), 
                                ymax = forecastTbatsCasual$mean + 40 * sd(forecastTbatsCasual$mean)), 
              aes(x = data, ymin = ymin, ymax = ymax), fill = "red", alpha = 0.3) +
  labs(x = "Date", y = "Total") +
  theme_minimal()

# RMSE
rmseTbatsCasual <- rmse(test_Casual$y, forecastTbatsCasual$mean)
cat("RMSE TBATS CASUAL:", rmseTbatsCasual, "\n")

# RMSE NORM
rmsePercTbatsCasual <- rmseNorm(test_Casual$y, forecastTbatsCasual$mean)
cat("%RMSE TBATS CASUAL:", rmsePercTbatsCasual, "%\n")

# MAPE
mapeTbatsCasual <- mape(test_Casual$y, forecastTbatsCasual$mean)
cat("MAPE TBATS CASUAL:", mapeTbatsCasual, "%\n")

# MdAPE
mdapeTbatsCasual <- mdape(test_Casual$y, forecastTbatsCasual$mean)
cat("MdAPE TBATS CASUAL:", mdapeTbatsCasual, "%\n")



# TBATS Extension and Analysis

# Create a Time Series Object for the Entire Dataset
casual_ts <- ts(casual$totale, frequency = 7)  # Assuming weekly seasonality

# Re-fit the TBATS Model
model_tbats_extended_casual <- tbats(casual_ts, seasonal.periods = c(7, 365))  # Adjust seasonal periods as needed

last_day_casual <- tail(casual$data, 1)
diff_days_casual <- 365 - as.numeric(difftime(last_day_casual+2, as.Date(paste0(as.numeric(format((last_day_casual+1), "%Y")),"-01-01")), units = "days"))

# Create an extended time series for forecasting
extended_time_series_casual <- ts(rep(NA, diff_days_casual), frequency = 7, start = c(as.numeric(format((last_day_casual+1), "%Y")),
                                                                                      as.numeric(format((last_day_casual+1), "%m")),
                                                                                      as.numeric(format((last_day_casual+1), "%d"))))

# Forecast for the Remaining Days of 2023
forecast_tbats_extended_casual <- forecast(model_tbats_extended_casual, h = diff_days_casual, newdata = extended_time_series_casual)

# Plot the Extended Forecast
plot(forecast_tbats_extended_casual, xlab = "Date", ylab = "Totale", main = "Extended Forecast for 2023 - TBATS Model")



par(mar = c(5, 4, 1, 1), mfrow = c(3,1))
plot(rowMeans(matrix(forecast_tbats_extended_casual$residuals[1:(length(forecast_tbats_extended_casual$residuals) %/% 365 * 365)], nrow = 365), na.rm = TRUE),
     xlab = "Time", ylab = "Yearly", type = "l")
plot(rowMeans(matrix(forecast_tbats_extended_casual$residuals[1:(length(forecast_tbats_extended_casual$residuals) %/% 30 * 30)], nrow = 30), na.rm = TRUE),
     xlab = "Time", ylab = "Monthly", type = "l")
plot(rowMeans(matrix(forecast_tbats_extended_casual$residuals[1:(length(forecast_tbats_extended_casual$residuals) %/% 7 * 7)], nrow = 7), na.rm = TRUE),
     xlab = "Time", ylab = "Weekly", type = "l")
par(mar = c(5, 4, 4, 2) + 0.1, mfrow = c(1,1))



# confronto indicatori
comparisonCasual <- data.frame(
  Modello = c("Arima", "Prophet", "Tbats"),
  RMSE = c(rmseArimaCasual, rmseProphetCasual, rmseTbatsCasual),
  `%RMSE` = sprintf("%.2f%%", c(rmsePercArimaCasual, rmsePercProphetCasual, rmsePercTbatsCasual)),
  MAPE = sprintf("%.2f%%", c(mapeArimaCasual, mapeProphetCasual, mapeTbatsCasual)),
  MDAPE = sprintf("%.2f%%", c(mdapeArimaCasual, mdapeProphetCasual, mdapeTbatsCasual)),
  check.names = FALSE
); comparisonCasual

