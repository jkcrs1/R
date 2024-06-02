
cat("\014")

#setwd("C:/UDLA/R/R")#diredtorio de R


# Cargar librerías necesarias
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(prophet)
library(stringr)

# Enlace raw al archivo de funciones en GitHub
source("https://raw.githubusercontent.com/jkcrs1/R/main/funciones.R")



# Permisos circulacion desde 2023 - today
url3 <- "https://raw.githubusercontent.com/jkcrs1/R/cdcae61126cd9adaa1b4033f0a97441e7d2bb691/permiso_circulacion_2023_2024_calbuco.csv"

permiso = read.csv(url3, sep = ";")

permiso = data.frame(permiso)
#permiso <- data.frame(data("permiso"))
str(permiso)
dim(permiso)

head(permiso)

#cambio de nombres de columnas
colnames(permiso) <- colnames(permiso) %>%
  str_replace_all("\\.", "_")


#normaliza de datos 
tipo_vehiculo_mapeo <- c(
  "AMBULANCIA" = "AMBULANCIA",
  "AUTOMOVIL" = "AUTOMOVIL",
  "BUS" = "BUS",
  "Cabriolet" = "AUTOMOVIL",
  "CAMION" = "CAMION",
  "CAMIONETA" = "CAMIONETA",
  "CARRO ARRASTRE A" = "REMOLQUE",
  "CARRO BOMBA" = "CARRO BOMBA",
  "CASA RODANTE" = "CASA RODANTE",
  "Comercial" = "COMERCIAL",
  "CUATRIMOTO" = "CUATRIMOTO",
  "FURGON" = "FURGON",
  "GRUA" = "MAQUINA PESADA",
  "Hatchback" = "AUTOMOVIL",
  "JEEP" = "AUTOMOVIL",
  "MAQUINA INDUSTRIAL" = "MAQUINA INDUSTRIAL",
  "MINIBUS" = "MINIBUS",
  "MINIBUS ESCOLAR" = "MINIBUS",
  "MINIBUS PARTICULAR" = "MINIBUS",
  "MINIBUS PRIVADO" = "MINIBUS",
  "MINIBUS TURISMO" = "MINIBUS",
  "MOTO" = "MOTOCICLETA",
  "MOTOCICLETA" = "MOTOCICLETA",
  "OTROS" = "OTROS",
  "REMOLQUE A" = "REMOLQUE",
  "REMOLQUE B" = "REMOLQUE",
  "RETROEXCAVADORA" = "MAQUINA PESADA",
  "Sedan" = "AUTOMOVIL",
  "SEMI REMOLQUE" = "REMOLQUE",
  "STATION WAGON" = "AUTOMOVIL",
  "SUV" = "SUV",
  "TAXI EJECUTIVO" = "TAXI",
  "TAXI BASICO" = "TAXI",
  "TAXI COLECTIVO" = "TAXI",
  "TRACTOCAMION" = "CAMION",
  "TRACTOR" = "TRACTOR",
  "VAN" = "VAN"
)

tipo_combustible_mapeo <- c(
  "Benc" = "Bencina",
  "Dies" = "Diesel",
  "NULL" = "NULL",
  "DUAL" = "Hibrido",
  "Hibr" = "Hibrido",
  "Elec" = "Electrico"
)

transmision_mapeo <- c(
  "Mec" = "Mecanica",
  "Aut" = "Automatica",
  "NULL" = "NULL",
  "CVT" = "Automatica",
  "DCT" = "Automatica"
)


# Aplicar todas las transformaciones y normalizaciones
permiso <- permiso %>%
  mutate(
    Tipo_Vehiculo = recode(Tipo_Vehiculo, !!!tipo_vehiculo_mapeo),
    Tipo_Combustible = recode(Tipo_Combustible, !!!tipo_combustible_mapeo),
    Transmision = recode(Transmision, !!!transmision_mapeo),
    across(everything(), reemplazar_nulos),
    across(c(Municipalidad, Grupo_Vehiculo, Placa, Digito, Codigo_SII,
             Forma_Pago, Tipo_Vehiculo, Marca, Modelo, Color,
             Transmision, Tipo_Combustible, Equipamiento), str_to_title)
  ) %>%
  mutate(
    Ano_Vehiculo = as.integer(Ano_Vehiculo),
    Valor_Neto = as.numeric(Valor_Neto),
    Fecha_Pago = as.Date(Fecha_Pago, format = "%d-%m-%Y"),
    Ano = year(Fecha_Pago),
    Mes = month(Fecha_Pago, label = TRUE)
  ) %>%
  filter(!is.na(Valor_Pagado) & !is.na(Fecha_Pago) & Valor_Pagado > 0) %>%
  distinct()

# Mostrar los primeros registros para verificar
head(permiso)



# Verificar valores faltantes
sum(is.na(permiso))








# muestra 10%
#n <- 0.1 * nrow(permiso)  

#set.seed(1)  
#cant <- sample(nrow(permiso), n)
#permiso_muestra <- permiso[cant, ]



# mustra aleatorio segun Desviacion Estandar

cant=nrow(permiso) 
sd=sd(permiso$Valor_Pagado)
n=tam.muestra(alfa=0.05,epsilon=10000,s=sd,N=cant)
n


set.seed(2)  
cant <- sample(nrow(permiso), n)
permiso_muestra <- permiso[cant, ]
permiso_muestra <- permiso



# tratamiento de outliers

# Calcular el IQR y los límites para identificar valores atípicos
IQR_Valor_Pagado <- IQR(permiso_muestra$Valor_Pagado, na.rm=TRUE)
Q1 <- quantile(permiso_muestra$Valor_Pagado, 0.25, na.rm=TRUE)
Q3 <- quantile(permiso_muestra$Valor_Pagado, 0.75, na.rm=TRUE)
lower_bound <- Q1 - 1.5 * IQR_Valor_Pagado
upper_bound <- Q3 + 1.5 * IQR_Valor_Pagado

# Filtrar los valores atípicos
outliers <- permiso_muestra %>%
  filter(Valor_Pagado < lower_bound | Valor_Pagado > upper_bound)

# Eliminar los valores atípicos
permiso_sin_outliers <- permiso_muestra %>%
  filter(Valor_Pagado >= lower_bound & Valor_Pagado <= upper_bound)

# Transformación logarítmica
permiso_muestra$log_Valor_Pagado <- log(permiso_muestra$Valor_Pagado + 1)

# Reemplazar valores atípicos con los límites
permiso_reemplazado <- permiso_muestra %>%
  mutate(Valor_Pagado = ifelse(Valor_Pagado < lower_bound, lower_bound,
                               ifelse(Valor_Pagado > upper_bound, upper_bound, Valor_Pagado)))

# Visualización de valores atípicos eliminados
ggplot(permiso_sin_outliers, aes(y=Valor_Pagado)) +
  geom_boxplot(fill="lightblue") +
  labs(title="Diagrama de Caja de Valor Pagado (Sin Outliers)",
       y="Valor Pagado") +
  theme_minimal()

# Visualización de transformación logarítmica
ggplot(permiso_muestra, aes(y=log_Valor_Pagado)) +
  geom_boxplot(fill="lightblue") +
  labs(title="Diagrama de Caja de Valor Pagado (Log Transformado)",
       y="Valor Pagado (Log)") +
  theme_minimal()

# Visualización de valores atípicos reemplazados
ggplot(permiso_reemplazado, aes(y=Valor_Pagado)) +
  geom_boxplot(fill="lightblue") +
  labs(title="Diagrama de Caja de Valor Pagado (Outliers Reemplazados)",
       y="Valor Pagado") +
  theme_minimal()



#ANALISIS



#definir data para analisis

permiso_analisis <- permiso_sin_outliers

permiso_analisis <- permiso



tabla_estadistica <- tabla_estadistica(permiso, permiso$Valor_Neto, permiso$Tipo_de_Pago)
print(tabla_estadistica$Titulo) 
View(tabla_estadistica$ExplicacionesValores)
print(tabla_estadistica$Resumen)





#graficos por funcion

# Variables para gráficos
data <- permiso_analisis 
var_x <- "Valor_Pagado"
var_y <- "Tipo_Vehiculo"

# Gráfico de torta
var_tipo_grafico <- "torta"
grafico_comparado_prueba(
  data = data, 
  var_cuan = var_x, 
  var_cual = var_y, 
  tipo_grafico = var_tipo_grafico)


multiplas

var_x <- "Valor_Pagado"
var_y <- "Tipo_de_Pago"

# Gráfico de líneas
var_tipo_grafico <- "lineas"
grafico_comparado_prueba(
  data = data, 
  var_cuan = var_x, 
  var_cual = var_y, 
  tipo_grafico = var_tipo_grafico,
  var_tiempo = "Fecha_Pago")  



# Gráfico de líneas
var_tipo_grafico <- "lineas"
grafico_comparado_prueba(
  data = data, 
  var_cuan = var_x, 
  var_cual = var_y, 
  tipo_grafico = var_tipo_grafico)

# Gráfico de caja
var_tipo_grafico <- "caja"
grafico_comparado_prueba(
  data = data, 
  var_cuan = var_x, 
  var_cual = var_y, 
  tipo_grafico = var_tipo_grafico)


# Variables para gráficos
data <- permiso_analisis 
var_x <- "Valor_Pagado"
var_y <- "Tipo_Vehiculo"


# Gráfico de barras (suma)
var_tipo_grafico <- "barra"
var_tipo_calculo <- "suma"
grafico_comparado_prueba(
  data = data, 
  var_cuan = var_x, 
  var_cual = var_y, 
  tipo_grafico = var_tipo_grafico,
  tipo_calculo = var_tipo_calculo)



# Gráfico de columnas (suma)
var_tipo_grafico <- "columnas"
var_tipo_calculo <- "suma"
grafico_comparado_prueba(
  data = data, 
  var_cuan = var_x, 
  var_cual = var_y, 
  tipo_grafico = var_tipo_grafico,
  tipo_calculo = var_tipo_calculo)



# Gráfico de columnas (suma)
var_tipo_grafico <- "columnas"
var_tipo_calculo <- "suma"
grafico_comparado_prueba(
  data = data, 
  var_cuan = var_x, 
  var_cual = var_y, 
  tipo_grafico = var_tipo_grafico,
  tipo_calculo = var_tipo_calculo)

# Gráfico de densidad
var_tipo_grafico <- "densidad"
grafico_comparado_prueba(
  data = data, 
  var_cuan = var_x,
  var_cual = var_y, 
  tipo_grafico = var_tipo_grafico)

# Gráfico de puntos
var_tipo_grafico <- "puntos"
grafico_comparado_prueba(
  data = data, 
  var_cuan = var_x,
  var_cual = var_y, 
  tipo_grafico = var_tipo_grafico)

# Gráfico de líneas acumuladas (suma) por año
var_tipo_grafico <- "lineas acumuladas"
var_z <- "Ano_Vehiculo"
var_tipo_calculo <- "suma"
grafico_comparado_prueba(
  data = data, 
  var_cuant = var_x,
  var_cual = var_y,
  var_tiempo = var_z, 
  tipo_grafico = var_tipo_grafico, 
  tipo_calculo = var_tipo_calculo)

# Gráfico de líneas acumuladas (suma) por mes y año
var_tipo_grafico <- "lineas acumuladas"
var_z <- "Mes"
var_y <- "Ano"
var_tipo_calculo <- "suma"
grafico_comparado_prueba(
  data = data, 
  var_cuant = var_x,
  var_cual = var_y,
  var_tiempo = var_z, 
  tipo_grafico = var_tipo_grafico, 
  tipo_calculo = var_tipo_calculo)

# Crear mapa de calor de correlaciones
mapa_calor_correlaciones(data)









#Modelo 




library(dplyr)
library(lubridate)
library(xgboost)
library(caret)
library(ggplot2)
library(ROSE)


# Definir la semilla para reproducibilidad
set.seed(123)
# Filtrar solo los vehículos livianos
vehiculos_livianos <- permiso_analisis %>% filter(Grupo_Vehiculo == "Vehiculo Liviano")


# Balanceo de datos usando ROSE


balanced_data <- ROSE(Grupo_Vehiculo ~ ., data = permiso_analisis, seed = 123)$data

# Asegúrate de que los datos están balanceados
table(balanced_data$Grupo_Vehiculo)


# Agregar columnas de año y mes
vehiculos_livianos <- vehiculos_livianos %>%
  mutate(Ano = year(Fecha_Pago),
         Mes = month(Fecha_Pago, label = TRUE))

# Resumir la cantidad de vehículos livianos atendidos por año y mes
resumen_mensual_livianos <- vehiculos_livianos %>%
  group_by(Ano, Mes) %>%
  summarise(cantidad = n()) %>%
  arrange(Ano, Mes)

# Convertir a serie temporal
resumen_mensual_livianos <- resumen_mensual_livianos %>%
  mutate(date = as.Date(paste(Ano, Mes, "01", sep = "-"), format = "%Y-%B-%d"))

# Dividir los datos en conjunto de entrenamiento y prueba (80%-20%)
# Dividir los datos en conjunto de entrenamiento y prueba (80%-20%)
train_size <- floor(0.8 * nrow(resumen_mensual_livianos))
train_data <- resumen_mensual_livianos[1:train_size, ]
test_data <- resumen_mensual_livianos[(train_size + 1):nrow(resumen_mensual_livianos), ]

# Asegurarse de que solo las columnas numéricas se utilicen
train_matrix <- xgb.DMatrix(data = as.matrix(train_data %>% select(Ano, Mes)),
                            label = train_data$cantidad)
test_matrix <- xgb.DMatrix(data = as.matrix(test_data %>% select(Ano, Mes)),
                           label = test_data$cantidad)


# Parámetros del modelo xgboost
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse"
)

# Entrenar el modelo xgboost
xgb_model <- xgb.train(
  params = params,
  data = train_matrix,
  nrounds = 100,
  watchlist = list(train = train_matrix, eval = test_matrix),
  early_stopping_rounds = 10,
  verbose = 0
)


# Realizar predicciones en el conjunto de prueba
preds <- predict(xgb_model, newdata = test_matrix)

# Calcular el RMSE
rmse <- sqrt(mean((preds - test_data$cantidad)^2))
cat("RMSE en el conjunto de prueba:", rmse, "\n")

# Visualización de las predicciones
ggplot() +
  geom_line(data = test_data, aes(x = date, y = cantidad), color = "blue") +
  geom_line(data = test_data, aes(x = date, y = preds), color = "red") +
  labs(title = "Predicción de la cantidad de vehículos livianos atendidos",
       x = "Fecha", y = "Cantidad de Vehículos") +
  theme_minimal()


# Crear un data frame con las fechas futuras para los próximos 12 meses
future_dates <- seq.Date(from = max(resumen_mensual_livianos$date) + months(1),
                         by = "month", length.out = 12)

future_data <- data.frame(
  Ano = year(future_dates),
  Mes = month(future_dates, label = TRUE),
  date = future_dates
)

# Preparar la matriz de características para predicción
future_matrix <- xgb.DMatrix(data = as.matrix(future_data %>% select(-date)))

# Realizar predicciones
future_preds <- predict(xgb_model, newdata = future_matrix)

# Visualización de las predicciones futuras
future_data$cantidad <- future_preds

ggplot() +
  geom_line(data = resumen_mensual_livianos, aes(x = date, y = cantidad), color = "blue") +
  geom_line(data = future_data, aes(x = date, y = cantidad), color = "red") +
  labs(title = "Predicción de la cantidad de vehículos livianos atendidos para los próximos 12 meses",
       x = "Fecha", y = "Cantidad de Vehículos") +
  theme_minimal()
