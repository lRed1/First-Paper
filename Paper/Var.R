# Cargar las bibliotecas necesarias
library(vars)
library(data.table)
library(xts)
library(readxl)

# Leer los datos desde el archivo Excel
data <- read_excel("C:/Users/IVAN/Documents/VSCODE/Python/Tesis/DATA/data_prueba.xlsx")

# Crear una serie temporal a partir de la columna DATE
data_ts <- xts(data[, -1], order.by = as.Date(data$DATE))

# Realizar diferenciación y transformación logarítmica en las columnas necesarias
data_ts$RATE <- diff(log(data_ts$RATE))
data_ts$WTI <- diff(log(data_ts$WTI))
data_ts$XAU <- diff(log(data_ts$XAU))
#data_ts$GDP_US <- diff(log(data_ts$GDP_US))
#data_ts$M1_EU <- diff(log(data_ts$M1_EU))
#data_ts$CPI_EU <- diff(log(data_ts$CPI_EU))
#data_ts$GDP_EU <- diff(log(data_ts$GDP_EU))

# Remover las filas NA resultantes de la diferenciación
data_ts <- data_ts[complete.cases(data_ts),]

# Ajustar un modelo VAR a los datos
var_model <- VAR(data_ts, p = 1)  # Ajusta el valor de 'p' según tus necesidades

# Resumen del modelo VAR
summary(var_model)

#Graficos
plot(data_ts, main="Series Temporales Originales", xlab="Fecha", ylab="Valor")
#Grafico de autocorrelacion simple
par(mfrow=c(3, 1))  
acf(data_ts$RATE, main="ACF de RATE")
acf(data_ts$WTI, main="ACF de WTI")
acf(data_ts$XAU, main="ACF de XAU")
#Grafico de autocorrelacion parcial
par(mfrow=c(3, 1))  #
pacf(data_ts$RATE, main="PACF de RATE")
pacf(data_ts$WTI, main="PACF de WTI")
pacf(data_ts$XAU, main="PACF de XAU")



