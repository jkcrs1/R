
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
    Fecha_Pago = as.Date(Fecha_Pago, format = "%d-%m-%Y")
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





tabla_estadistica <- tabla_estadistica(permiso_analisis, "Valor_Neto", "Tipo_de_Pago")
print(tabla_estadistica$Titulo) 
View(tabla_estadistica$ExplicacionesValores)
print(tabla_estadistica$Resumen)







grafico_comparado(data = permiso_analisis, var_cuant = "Valor_Pagado", var_cual = "Tipo_Vehiculo", tipo_grafico = "lineas", tipo_calculo = "suma")
grafico_comparado(data = permiso_analisis, var_cuant = "Valor_Pagado", var_cual = "Tipo_Vehiculo", tipo_grafico = "lineas", tipo_calculo = "promedio")
grafico_comparado(data = permiso_analisis, var_cuant = "Valor_Pagado", var_cual = "Tipo_Vehiculo", tipo_grafico = "lineas", tipo_calculo = "cuenta")
grafico_comparado(data = permiso_analisis, var_cuant = "Valor_Pagado", var_cual = "Tipo_Vehiculo", tipo_grafico = "caja")
grafico_comparado(data = permiso_analisis, var_cuant = "Valor_Pagado", var_cual = "Tipo_Vehiculo", tipo_grafico = "histograma")
grafico_comparado(data = permiso_analisis, var_cuant = "Valor_Pagado", var_cual = "Tipo_Vehiculo", tipo_grafico = "barra")
grafico_comparado(data = permiso_analisis, var_cuant = "Valor_Pagado", var_cual = "Tipo_Vehiculo", tipo_grafico = "dispercion")
grafico_comparado(data = permiso_analisis, var_cuant = "Valor_Pagado", var_cual = "Tipo_Vehiculo", tipo_grafico = "densidad")
grafico_comparado(data = permiso_analisis, var_cuant = "Valor_Pagado", var_cual = "Tipo_Vehiculo", tipo_grafico = "violin")
grafico_comparado(data = permiso_analisis, var_cuant = "Valor_Pagado", var_cual = "Tipo_Vehiculo", tipo_grafico = "puntos")
grafico_comparado(data = permiso_analisis, var_cuant = "Valor_Pagado", var_cual = "Tipo_Vehiculo", tipo_grafico = "burbujas")
grafico_comparado(data = permiso_analisis, var_cuant = "Valor_Pagado", var_cual = "Tipo_Vehiculo", tipo_grafico = "area ampliada")
grafico_comparado(data = permiso_analisis, var_cuant = "Valor_Pagado", var_cual = "Tipo_Vehiculo", tipo_grafico = "barras ampliadas")
grafico_comparado(data = permiso_analisis, var_cuant = "Valor_Pagado", var_cual = "Tipo_Vehiculo", tipo_grafico = "torta")


  

# Histograma de la permisos pagados

ggplot(permiso_analisis, aes(x=Valor_Pagado)) +
  geom_histogram(binwidth=5000, fill="blue", color="grey") +
  scale_x_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  labs(title="Distribución de Salarios",
       x="Salario",
       y="Frecuencia") +
  theme_minimal()





# liena temporal mensual con cantidad de permisos y el promedio del pagado

#agrupado mensual con el promedio de pagos y la cantidad e pagos realizados
resumen_diario <- permiso_analisis %>%
  group_by(Fecha_Pago) %>%
  summarise(
    promedio_valor_pagado = mean(Valor_Pagado, na.rm=TRUE),
    cantidad_registros = n()
  )

# Obtener el máximo de la cantidad de registros para ajustar las escalas
max_count <- max(resumen_diario$cantidad_registros)

# Crear el gráfico combinado
ggplot(resumen_diario, aes(x=Fecha_Pago)) +
  geom_bar(aes(y=cantidad_registros), stat="identity", fill="lightblue", alpha=0.6) +
  geom_line(aes(y=promedio_valor_pagado * (max_count / max(resumen_diario$promedio_valor_pagado))), color="orange", size=1) +
  geom_point(aes(y=promedio_valor_pagado * (max_count / max(resumen_diario$promedio_valor_pagado))), color="orange", size=1) +
  labs(title="Cantidad de Registros y Valor Pagado Promedio por Día",
       x="Fecha de Pago",
       y="Cantidad de Registros") +
  scale_y_continuous(
    name = "Cantidad de Registros",
    sec.axis = sec_axis(~ . / (max_count / max(resumen_diario$promedio_valor_pagado)), name = "Valor Pagado Promedio")
  ) +
  theme_minimal()








# Gráfico de barras del salario promedio por cargo

permiso_por_tipo_auto <- aggregate(Valor_Pagado ~ Tipo_Vehiculo, data=permiso_analisis, mean)


ggplot(permiso_por_tipo_auto, aes(x=reorder(Tipo_Vehiculo, Valor_Pagado), y=Valor_Pagado)) +
  geom_bar(stat="identity", fill="lightblue") +
  geom_text(aes(label=scales::comma(Valor_Pagado, big.mark = ".", decimal.mark = ",")), hjust=-0.1) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  labs(title="Salario Promedio por Cargo",
       x="Cargo",
       y="Salario Promedio") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# Calcular por tipo de de vehiculo la cantidad permisos pagados
cantidad_por_tipo_vehiculo <- aggregate(Valor_Pagado ~ Tipo_Vehiculo, data=permiso_analisis, length)
colnames(cantidad_por_tipo_vehiculo) <- c("Tipo_Vehiculo", "count")

# Calcular el permiso promedio por tipo de vehículo
permiso_por_tipo_auto <- aggregate(Valor_Pagado ~ Tipo_Vehiculo, data=permiso_analisis, mean)

# Combinar las dos tablas
datos_combinados <- merge(permiso_por_tipo_auto, cantidad_por_tipo_vehiculo, by="Tipo_Vehiculo")

# Obtener el máximo de la cantidad para ajustar las escalas
max_count <- max(datos_combinados$count)

# Crear el gráfico combinado
ggplot(datos_combinados, aes(x = reorder(Tipo_Vehiculo, Valor_Pagado))) +
  geom_bar(aes(y = count), stat = "identity", fill = "lightblue", alpha = 0.6) +
  geom_text(aes(y = count, label = scales::comma(count, big.mark = ".", decimal.mark = ",")), hjust = -0.2, color = "blue", size = 3) +
  geom_line(aes(y = Valor_Pagado * (max_count / max(Valor_Pagado))), color = "red", size = 1, group = 1) +
  geom_point(aes(y = Valor_Pagado * (max_count / max(Valor_Pagado))), color = "red", size = 2) +
  geom_text(aes(y = Valor_Pagado * (max_count / max(Valor_Pagado)), label = scales::comma(Valor_Pagado, big.mark = ".", decimal.mark = ",")), hjust = -0.2, color = "red", size = 3) +
  scale_y_continuous(
    name = "Cantidad de Datos",
    sec.axis = sec_axis(~ . / (max_count / max(datos_combinados$Valor_Pagado)), name = "Permiso Promedio", labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) 
  ) +
  labs(title = "Cantidad de Datos y Permiso Pagdos por Tipo de Vehiculo",
       x = "Tipo de Vehículo") +
  coord_flip() +
  theme_minimal()






# comparativo mensual acumulado por monto de pagos de permisos


# Agregar columnas de año y mes
permiso_analisis <- permiso_analisis %>%
  mutate(Ano = year(Fecha_Pago),
         Mes = month(Fecha_Pago, label = TRUE))

# Resumir el valor pagado por año y mes y calcular el monto acumulado
resumen_mensual <- permiso_analisis %>%
  group_by(Ano, Mes) %>%
  summarise(monto_pagado = sum(Valor_Pagado, na.rm=TRUE)) %>%
  arrange(Ano, Mes) %>%
  group_by(Ano) %>%
  mutate(monto_acumulado = cumsum(monto_pagado))

# Crear un gráfico acumulado mensual del monto pagado por año
ggplot(resumen_mensual, aes(x = Mes, y = monto_acumulado, group = Ano, color = as.factor(Ano))) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = scales::comma(monto_acumulado, big.mark = ".", decimal.mark = ",")),
            vjust = -0.5, size = 3, color = "black") +
  labs(title = "Monto Pagado Acumulado Mensualmente por Año",
       x = "Mes",
       y = "Monto Pagado Acumulado",
       color = "Año") +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#mapa de correlacion

# Seleccionar solo variables numéricas
numeric_vars <- permiso_analisis %>% select_if(is.numeric)

# Calcular matriz de correlación
cor_matrix <- cor(numeric_vars, use="complete.obs")

# Visualizar con un mapa de calor

melted_cor_matrix <- melt(cor_matrix)

ggplot(melted_cor_matrix, aes(Var1, Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low = "orange", high = "red", mid = "white", midpoint = 0) +
  labs(title="Mapa de Calor de Correlaciones",
       x="Variables",
       y="Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))








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
