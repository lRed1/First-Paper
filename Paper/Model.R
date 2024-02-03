library(vars)
library(urca)
library(data.table)
library(xts)
library(readxl)

data <- read_excel("C:/Users/IVAN/Documents/VSCODE/Python/Tesis/DATA/data_T.xlsx")
data_ts <- xts(data[, -1], order.by = as.Date(data$DATE))

names(data_ts)

# Realizar diferenciación y transformación logarítmica en las columnas necesarias
data_ts$RATE <- diff(log(data_ts$RATE))
data_ts$M1_US <- diff(log(data_ts$M1_US))
data_ts$CPI_US <- diff(log(data_ts$CPI_US))
data_ts$GDP_US <- diff(log(data_ts$GDP_US))
data_ts$M1_EU <- diff(log(data_ts$M1_EU))
data_ts$CPI_EU <- diff(log(data_ts$CPI_EU))
data_ts$GDP_EU <- diff(log(data_ts$GDP_EU))

#Limpieza
data_ts <- data_ts[complete.cases(data_ts),]

# Calcular la matriz de correlaciones entre las variables originales
correlation_matrix <- cor(data[, -1])  # Excluye la columna DATE

# Mostrar la matriz de correlaciones
print("Matriz de Correlaciones:")
print(correlation_matrix)

#VEC
# Prueba de cointegración de Johansen
johansen_test <- ca.jo(data_ts[, c("RATE", "GDP_EU")], type = "eigen", ecdet = "none", K = 2)
print("Prueba de Johansen:")
print(summary(johansen_test))

# Gráfico de Eigenvalues
plot(johansen_test)

# Gráfico de Trace Statistic
plot(johansen_test, type = "trace")





#VAR
# Ajustar un modelo VAR a los datos
var_model <- VAR(data_ts, p = 5)  # Ajusta el valor de 'p' según tus necesidades
# Resumen del modelo VAR
summary(var_model)


# Crear una matriz de datos para el gráfico de correlaciones
correlation_data <- as.data.frame(correlation_matrix)

# Gráfico de correlaciones
# Crear una matriz de datos para el gráfico de correlaciones
correlation_data <- as.data.frame(correlation_matrix)
correlation_data$rowname <- rownames(correlation_data)  # Agregar una columna con los nombres de las filas

# Reorganizar los datos en formato largo (long format)
library(reshape2)
correlation_data_long <- melt(correlation_data, id.vars = "rowname")

# Gráfico de correlaciones
library(ggplot2)
ggplot(data = correlation_data_long, aes(x = rowname, y = variable, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Matriz de Correlaciones", x = "", y = "") +
  theme(legend.title = element_blank())


library(ggplot2)

ggplot(data = data_ts, aes(x = index(data_ts))) +
  geom_line(aes(y = RATE, color = "RATE")) +
  geom_line(aes(y = M1_US, color = "M1_US")) +
  geom_line(aes(y = CPI_US, color = "CPI_US")) +
  geom_line(aes(y = GDP_US, color = "GDP_US")) +
  geom_line(aes(y = M1_EU, color = "M1_EU")) +
  geom_line(aes(y = CPI_EU, color = "CPI_EU")) +
  geom_line(aes(y = GDP_EU, color = "GDP_EU")) +
  labs(title = "Serie Temporal de Variables", x = "Fecha", y = "Valor", color = "Variable")

#Grafico de dispersion para variables
ggplot(data = data_ts, aes(x = RATE, y = GDP_EU)) +
  geom_point() +
  labs(title = "Gráfico de Dispersión entre RATE y GDP_EU", x = "RATE", y = "GDP_EU")

#Grafico de Autocorrelacion
library(vars)

plot(var_model)

