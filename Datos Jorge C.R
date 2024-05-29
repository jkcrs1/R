
setwd("C:/UDLA/R/R")#diredtorio de R

cat("\014")

# Cargar librerías necesarias
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(prophet)


# Carga archivo con funcones
source("C:\\UDLA\\R\\R\\funciones.R") #leer un archivo con funciones


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


# Convertir tipos de datos si es necesario
permiso$Ano_Vehiculo <- as.integer(permiso$Ano_Vehiculo)
permiso$Valor_Neto <- as.numeric(permiso$Valor_Neto)
permiso$Fecha_Pago <- as.Date(permiso$Fecha_Pago, format="%d-%m-%Y")


# Verificar valores faltantes
sum(is.na(permiso))


# Verifica que no haya valores NA en la columna Valor_Pagado
permiso <- permiso %>% 
  filter(!is.na(Valor_Pagado) & !is.na(Fecha_Pago) & Valor_Pagado > 0)


# Eliminar filas duplicadas
permiso <- permiso %>% distinct()


# Análisis Exploratorio de Datos
# Resumen estadístico de las variables numéricas
summary(permiso)



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


#histograma de permisos pagados por año de fabricación

ggplot(permiso_analisis, aes(x=Ano_Vehiculo)) +
  geom_histogram(binwidth=1, fill="lightgreen", color="black") +
  labs(title="Distribución del Año de Fabricación de los Vehículos",
       x="Año de Fabricación",
       y="Frecuencia") +
  theme_minimal()


# Grafico de Caja para el valor pagado

ggplot(permiso_analisis, aes(x="", y=Valor_Pagado)) +
  geom_boxplot(fill="lightblue") +
  labs(title="Diagrama de Caja de Valor Pagado",
       y="Valor Pagado") +
  theme_minimal()







# Linea de Tenciancia por pago promedio mensual 
ggplot(permiso_analisis, aes(x=Fecha_Pago, y=Valor_Pagado)) +
  geom_line(stat="summary", color="blue") +
  labs(title="Tendencia Temporal del Valor Pagado",
       x="Fecha de Pago",
       y="Valor Pagado Promedio") +
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
  geom_line(aes(y=promedio_valor_pagado * (max_count / max(resumen_diario$promedio_valor_pagado))), color="red", size=1) +
  geom_point(aes(y=promedio_valor_pagado * (max_count / max(resumen_diario$promedio_valor_pagado))), color="red", size=2) +
  labs(title="Cantidad de Registros y Valor Pagado Promedio por Día",
       x="Fecha de Pago",
       y="Cantidad de Registros") +
  scale_y_continuous(
    name = "Cantidad de Registros",
    sec.axis = sec_axis(~ . / (max_count / max(resumen_diario$promedio_valor_pagado)), name = "Valor Pagado Promedio")
  ) +
  theme_minimal()





# Histograma de la permisos pagados

ggplot(permiso_analisis, aes(x=Valor_Pagado)) +
  geom_histogram(binwidth=5000, fill="blue", color="grey") +
  scale_x_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  labs(title="Distribución de Salarios",
       x="Salario",
       y="Frecuencia") +
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





# Calcular el salario promedio y la cantidad de datos por título de trabajo
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
  labs(title = "Cantidad de Datos y Permiso Promedio por Tipo de Vehiculo",
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




















#graficos por funcion

# Definir los gráficos a crear
graficos <- list(
  list(var_cuant = "Valor_Pagado", var_cual = "Grupo_Vehiculo", tipo_grafico = "torta"),
  list(var_cuant = "Valor_Pagado", var_cual = "Grupo_Vehiculo", tipo_grafico = "caja")
  #list(var_cuant = "Valor_Pagado", var_cual = "Tipo_Vehiculo", tipo_grafico = "caja")
)

# Crear los gráficos

crear_graficos_multiple(permiso_analisis, graficos)


# Boxplot del valor neto por tipo de vehículo
ggplot(permiso_analisis, aes(x = Tipo_Vehiculo, y = Valor_Pagado)) +
  geom_boxplot() +
  labs(title = "Valor Neto por Tipo de Vehículo", x = "Tipo de Vehículo", y = "Valor Neto")




crear_grafico_comparado_cuali_cuanti(data = permiso_analisis,var_cuant = "Valor_Pagado", var_cual = "Fecha_Pago",tipo_grafico = "barra")


crear_grafico_comparado_cuali_cuanti(data = permiso_analisis,var_cuant = "Valor_Pagado", var_cual = "Grupo_Vehiculo",tipo_grafico = "torta")

crear_grafico_comparado_cuali_cuanti(data = permiso_analisis,var_cuant = "Valor_Pagado", var_cual = "Grupo_Vehiculo",tipo_grafico = "caja")

crear_grafico_comparado_cuali_cuanti(data = permiso_analisis,var_cuant = "Valor_Pagado", var_cual = "Tipo_Vehiculo",tipo_grafico = "caja")


tabla_estadistica <- crear_tabla_estadistica(permiso_analisis, "Valor_Neto", "Tipo_de_Pago")
View(tabla_estadistica)














#mapa de correlacion

# Seleccionar solo variables numéricas
numeric_vars <- permiso_analisis %>% select_if(is.numeric)

# Calcular matriz de correlación
cor_matrix <- cor(numeric_vars, use="complete.obs")

# Visualizar con un mapa de calor
library(reshape2)
melted_cor_matrix <- melt(cor_matrix)

ggplot(melted_cor_matrix, aes(Var1, Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title="Mapa de Calor de Correlaciones",
       x="Variables",
       y="Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))












#MODELO PARA DETERMINAR LA CANTIDAD DE PERMISOS EN LOS PROXIMOS MESES

# Instalar y cargar librerías necesarias
library(prophet)
library(dplyr)
library(lubridate)

# Asegúrate de que la columna Fecha_Pago esté correctamente convertida a tipo de fecha
permiso_analisis$Fecha_Pago <- as.Date(permiso_analisis$Fecha_Pago, format="%d-%m-%Y")

# Crear un resumen de los datos con la media de Valor_Pagado y la cantidad de registros por día
resumen_diario <- permiso_analisis %>%
  group_by(Fecha_Pago) %>%
  summarise(
    promedio_valor_pagado = mean(Valor_Pagado, na.rm=TRUE),
    cantidad_registros = n()
  )

# Preparar datos para el modelo de cantidad de registros
data_cantidad <- resumen_diario %>%
  select(ds = Fecha_Pago, y = cantidad_registros)

# Preparar datos para el modelo de valor pagado
data_valor <- resumen_diario %>%
  select(ds = Fecha_Pago, y = promedio_valor_pagado)

# Entrenar el modelo para cantidad de registros con estacionalidad anual habilitada
modelo_cantidad <- prophet(data_cantidad, yearly.seasonality=TRUE)

# Entrenar el modelo para valor pagado con estacionalidad anual habilitada
modelo_valor <- prophet(data_valor, yearly.seasonality=TRUE)

# Crear un data frame con las fechas futuras para los próximos 6 meses
futuro_cantidad <- make_future_dataframe(modelo_cantidad, periods = 180)
futuro_valor <- make_future_dataframe(modelo_valor, periods = 180)

# Hacer predicciones
predicciones_cantidad <- predict(modelo_cantidad, futuro_cantidad)
predicciones_valor <- predict(modelo_valor, futuro_valor)

# Visualizar predicciones para cantidad de registros
plot(modelo_cantidad, predicciones_cantidad) +
  labs(title="Predicción de la Cantidad de Registros",
       x="Fecha",
       y="Cantidad de Registros")

# Visualizar predicciones para valor pagado
plot(modelo_valor, predicciones_valor) +
  labs(title="Predicción del Valor Pagado Promedio",
       x="Fecha",
       y="Valor Pagado Promedio")






# Crear un gráfico de área acumulado mensual del monto pagado por año con etiquetas
ggplot(resumen_mensual, aes(x = Mes, y = monto_acumulado, fill = as.factor(Ano), group = Ano)) +
  geom_area(position = "identity", alpha = 0.6) +
  geom_text(aes(label = scales::comma(monto_acumulado, big.mark = ".", decimal.mark = ",")),
            position = position_stack(vjust = 0.9), size = 3, color = "black") +
  labs(title = "Monto Pagado Acumulado Mensualmente por Año",
       x = "Mes",
       y = "Monto Pagado Acumulado",
       fill = "Año") +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







#con los predccion de cantidad de pagos en los proximos meses, realizar un grafico que muestre dicha evolucion 


# Agregar columnas de año y mes
permiso_analisis <- permiso_analisis %>%
  mutate(Ano = year(Fecha_Pago),
         Mes = month(Fecha_Pago, label = TRUE, abbr = FALSE))

# Resumir el valor pagado por año y mes
resumen_mensual <- permiso_analisis %>%
  group_by(Ano, Mes) %>%
  summarise(monto_pagado = sum(Valor_Pagado, na.rm=TRUE)) %>%
  arrange(Ano, Mes)

# Preparar datos para el modelo de valor pagado
data_valor <- permiso_analisis %>%
  group_by(Fecha_Pago) %>%
  summarise(y = sum(Valor_Pagado, na.rm=TRUE)) %>%
  rename(ds = Fecha_Pago)

# Entrenar el modelo para valor pagado con estacionalidad anual y mensual habilitada
modelo_valor <- prophet(data_valor, weekly.seasonality=FALSE)

# Añadir estacionalidad mensual explícitamente
modelo_valor <- add_seasonality(modelo_valor, name='monthly', period=30.5, fourier.order=5)

# Crear un data frame con las fechas futuras para los próximos 6 meses
futuro_valor <- make_future_dataframe(modelo_valor, periods = 180)

# Hacer predicciones
predicciones_valor <- predict(modelo_valor, futuro_valor)

# Visualizar los componentes del modelo
prophet_plot_components(modelo_valor, predicciones_valor)

# Preparar las predicciones para unirlas con los datos originales
predicciones_valor <- predicciones_valor %>%
  select(ds, yhat) %>%
  rename(Fecha_Pago = ds, monto_pagado = yhat) %>%
  mutate(Ano = year(Fecha_Pago), Mes = month(Fecha_Pago, label = TRUE, abbr = FALSE))

# Unir los datos originales y las predicciones
todos_datos <- bind_rows(
  resumen_mensual %>% mutate(tipo = "Real"),
  predicciones_valor %>% mutate(tipo = "Predicción")
)

# Calcular el monto acumulado para todos los datos
todos_datos <- todos_datos %>%
  arrange(Ano, Mes) %>%
  group_by(Ano, tipo) %>%
  mutate(monto_acumulado = cumsum(monto_pagado))

# Crear un gráfico de líneas con puntos y etiquetas, incluyendo las predicciones
ggplot(todos_datos, aes(x = Mes, y = monto_acumulado, group = interaction(Ano, tipo), color = as.factor(Ano))) +
  geom_line(aes(linetype = tipo)) +
  geom_point(data = filter(todos_datos, tipo == "Real")) +
  geom_text(data = filter(todos_datos, tipo == "Real"), aes(label = scales::comma(monto_acumulado, big.mark = ".", decimal.mark = ",")),
            vjust = -0.5, size = 3, color = "black") +
  labs(title = "Monto Pagado Acumulado Mensualmente por Año con Predicciones",
       x = "Mes",
       y = "Monto Pagado Acumulado",
       color = "Año",
       linetype = "Tipo de Datos") +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
