cat("\014")

# Cargar librerías necesarias
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(prophet)
library(stringr)
library(caret)
library(corrplot)
library(e1071)
library(xgboost)
library(ROSE)
library(DMwR2)
library(pROC)

# Enlace raw al archivo de funciones en GitHub
source("https://raw.githubusercontent.com/jkcrs1/R/main/funciones.R")

# Permisos circulación desde 2023 - today
url3 <- "https://raw.githubusercontent.com/jkcrs1/R/cdcae61126cd9adaa1b4033f0a97441e7d2bb691/permiso_circulacion_2023_2024_calbuco.csv"
permiso <- read.csv(url3, sep = ";")

# Convertir el dataframe
permiso <- data.frame(permiso)
str(permiso)
dim(permiso)
head(permiso)

# Cambio de nombres de columnas
colnames(permiso) <- colnames(permiso) %>% str_replace_all("\\.", "_")

# Normalización de datos
tipo_vehiculo_mapeo <- c(
  "AMBULANCIA" = "AMBULANCIA", "AUTOMOVIL" = "AUTOMOVIL", "BUS" = "BUS",
  "Cabriolet" = "AUTOMOVIL", "CAMION" = "CAMION", "CAMIONETA" = "CAMIONETA",
  "CARRO ARRASTRE A" = "REMOLQUE", "CARRO BOMBA" = "CARRO BOMBA",
  "CASA RODANTE" = "CASA RODANTE", "Comercial" = "COMERCIAL", "CUATRIMOTO" = "CUATRIMOTO",
  "FURGON" = "FURGON", "GRUA" = "MAQUINA PESADA", "Hatchback" = "AUTOMOVIL",
  "JEEP" = "AUTOMOVIL", "MAQUINA INDUSTRIAL" = "MAQUINA INDUSTRIAL",
  "MINIBUS" = "MINIBUS", "MINIBUS ESCOLAR" = "MINIBUS", "MINIBUS PARTICULAR" = "MINIBUS",
  "MINIBUS PRIVADO" = "MINIBUS", "MINIBUS TURISMO" = "MINIBUS", "MOTO" = "MOTOCICLETA",
  "MOTOCICLETA" = "MOTOCICLETA", "OTROS" = "OTROS", "REMOLQUE A" = "REMOLQUE",
  "REMOLQUE B" = "REMOLQUE", "RETROEXCAVADORA" = "MAQUINA PESADA", "Sedan" = "AUTOMOVIL",
  "SEMI REMOLQUE" = "REMOLQUE", "STATION WAGON" = "AUTOMOVIL", "SUV" = "SUV",
  "TAXI EJECUTIVO" = "TAXI", "TAXI BASICO" = "TAXI", "TAXI COLECTIVO" = "TAXI",
  "TRACTOCAMION" = "CAMION", "TRACTOR" = "TRACTOR", "VAN" = "VAN"
)

tipo_combustible_mapeo <- c(
  "Benc" = "Bencina", "Dies" = "Diesel", "NULL" = "NULL",
  "DUAL" = "Hibrido", "Hibr" = "Hibrido", "Elec" = "Electrico"
)

transmision_mapeo <- c(
  "Mec" = "Mecanica", "Aut" = "Automatica", "NULL" = "NULL",
  "CVT" = "Automatica", "DCT" = "Automatica"
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
    Tasacion = as.numeric(Tasacion),
    Fecha_Pago = as.Date(Fecha_Pago, format = "%d-%m-%Y"),
    Ano = year(Fecha_Pago),
    Mes = month(Fecha_Pago, label = TRUE)
  ) %>%
  filter(!is.na(Valor_Pagado) & !is.na(Fecha_Pago) & Valor_Pagado > 0) %>%
  distinct()

sapply(permiso, class)

# Mostrar los primeros registros para verificar
head(permiso)

# Verificar valores faltantes
sum(is.na(permiso))

# Calcular la muestra aleatoria según Desviación Estándar
cant <- nrow(permiso)
sd <- sd(permiso$Valor_Pagado)
n <- tam.muestra(alfa = 0.05, epsilon = 10000, s = sd, N = cant)
set.seed(2)
cant <- sample(nrow(permiso), n)
permiso_muestra <- permiso[cant, ]
permiso_muestra <- permiso

# Tratamiento de outliers
# Calcular el IQR y los límites para identificar valores atípicos
IQR_Valor_Pagado <- IQR(permiso_muestra$Valor_Pagado, na.rm = TRUE)
Q1 <- quantile(permiso_muestra$Valor_Pagado, 0.25, na.rm = TRUE)
Q3 <- quantile(permiso_muestra$Valor_Pagado, 0.75, na.rm = TRUE)
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







#definir data para analisis

permiso_analisis <- permiso_sin_outliers

permiso_analisis <- permiso



tabla_estadistica <- tabla_estadistica(permiso, "Valor_Neto", "Tipo_de_Pago")
print(tabla_estadistica$Titulo) 
View(tabla_estadistica$ExplicacionesValores)
print(tabla_estadistica$Resumen)




# Variables para gráficos
data <- permiso_analisis 
var_x <- "Valor_Pagado"
var_y <- "Grupo_Vehiculo"

# Gráfico de torta
var_tipo_grafico <- "torta"
grafico(
  data = data, 
  var_cuant_x = var_x, 
  var_cual_x = var_y, 
  tipo_grafico = var_tipo_grafico
)

var_x <- "Valor_Pagado"
var_y <- "Tipo_de_Pago"

# Gráfico de líneas
var_tipo_grafico <- "lineas"
grafico(
  data = data, 
  var_cuant_x = var_x, 
  var_cual_x = var_y, 
  tipo_grafico = var_tipo_grafico,
  var_cuant_y = "Fecha_Pago"
)

# Gráfico de líneas sin variable de tiempo adicional
var_tipo_grafico <- "lineas"
grafico(
  data = data, 
  var_cuant_x = var_x, 
  var_cual_x = var_y, 
  tipo_grafico = var_tipo_grafico
)

# Gráfico de caja
var_tipo_grafico <- "caja con puntos"
grafico(
  data = data, 
  var_cuant_x = var_x, 
  var_cual_x = var_y, 
  tipo_grafico = var_tipo_grafico
)

# Variables para gráficos
data <- permiso_analisis 
var_x <- "Valor_Pagado"
var_y <- "Tipo_Vehiculo"

# Gráfico de barras (suma)
var_tipo_grafico <- "barra"
var_tipo_calculo <- "suma"
grafico(
  data = data, 
  var_cuant_x = var_x, 
  var_cual_x = var_y, 
  tipo_grafico = var_tipo_grafico, 
  tipo_calculo = var_tipo_calculo
)

# Gráfico de columnas (suma)
var_tipo_grafico <- "columnas"
var_tipo_calculo <- "suma"
grafico(
  data = data, 
  var_cuant_x = var_x, 
  var_cual_x = var_y, 
  tipo_grafico = var_tipo_grafico, 
  tipo_calculo = var_tipo_calculo
)

# Gráfico de densidad
var_tipo_grafico <- "densidad"
grafico(
  data = data, 
  var_cuant_x = var_x, 
  var_cual_x = var_y, 
  tipo_grafico = var_tipo_grafico
)

# Gráfico de puntos
var_cuant_y=
var_tipo_grafico <- "puntos"
grafico(
  data = data, 
  var_cuant_x = var_x, 
  var_cual_x = var_y,
  va
  tipo_grafico = var_tipo_grafico
)




# Gráfico de puntos
var_x <- "Valor_Pagado"
var_y <- "Ano_Vehiculo"
var_z <- "Tipo_de_Pago"
var_tipo_grafico <- "puntos"
grafico(
  data = data, 
  var_cuant_x = var_x, 
  var_cuant_y = var_y, 
  var_cual_x = var_z, 
  tipo_grafico = var_tipo_grafico
)




# Gráfico de líneas acumuladas (suma) por año
var_tipo_grafico <- "lineas acumuladas"
var_z <- "Ano_Vehiculo"
var_x <- "Valor_Pagado"
var_y <- "Tipo_de_Pago"
var_tipo_calculo <- "suma"
grafico(
  data = data, 
  var_cuant_x = var_x, 
  var_cual_x = var_y, 
  var_cuant_y = var_z, 
  tipo_grafico = var_tipo_grafico, 
  tipo_calculo = var_tipo_calculo
)

# Gráfico de líneas acumuladas (suma) por mes y año
var_tipo_grafico <- "lineas acumuladas"
var_z <- "Mes"
var_y <- "Ano"
var_tipo_calculo <- "suma"
grafico(
  data = data, 
  var_cuant_x = var_x, 
  var_cual_x = var_y, 
  var_cuant_y = var_z, 
  tipo_grafico = var_tipo_grafico, 
  tipo_calculo = var_tipo_calculo
)



var_tipo_grafico <- "mapa correlacion"
grafico(
  data = data,
  tipo_grafico = var_tipo_grafico
)


# Crear mapa de calor de correlaciones
mapa_calor(data)








#MODELOS ML

# Definición de la variable objetivo y variables predictoras
target <- "Pagos_n_meses"
predictors <- setdiff(names(permiso_reemplazado), target)

# Modelos
set.seed(42)
models <- list(
  "Logistic Regression" = train(permiso_reemplazado[, predictors], permiso_reemplazado[, target], method = "glm", family = "binomial"),
  "Decision Tree" = train(permiso_reemplazado[, predictors], permiso_reemplazado[, target], method = "rpart"),
  "Random Forest" = train(permiso_reemplazado[, predictors], permiso_reemplazado[, target], method = "rf"),
  "XGBoost" = train(permiso_reemplazado[, predictors], permiso_reemplazado[, target], method = "xgbTree")
)

# Evaluación de Modelos
evaluation_results <- data.frame(Model = character(), Dataset = character(), Accuracy = double(), ROC_AUC = double(), Precision = double(), Recall = double(), F1_Score = double())

datasets <- list("Validation" = list(permiso_reemplazado, permiso_reemplazado), "Test" = list(permiso_reemplazado, permiso_reemplazado))

for (model_name in names(models)) {
  model <- models[[model_name]]
  for (dataset_name in names(datasets)) {
    data_scaled <- datasets[[dataset_name]][[1]]
    data_original <- datasets[[dataset_name]][[2]]
    predictions <- predict(model, data_scaled[, predictors])
    confusion <- confusionMatrix(predictions, data_original[, target])
    accuracy <- confusion$overall['Accuracy']
    roc_auc <- roc(data_original[, target], predict(model, data_scaled[, predictors], type = "prob")[,2])$auc
    precision <- confusion$byClass['Precision']
    recall <- confusion$byClass['Recall']
    f1_score <- confusion$byClass['F1']
    evaluation_results <- r
    evaluation_results <- rbind(evaluation_results, data.frame(Model = model_name, Dataset = dataset_name, Accuracy = accuracy, ROC_AUC = roc_auc, Precision = precision, Recall = recall, F1_Score = f1_score))
  }
}

print(evaluation_results)

# Validación cruzada y optimización de hiperparámetros para Logistic Regression
set.seed(42)
log_reg_grid <- expand.grid(C = c(0.1, 1, 10, 100))
train_control <- trainControl(method = "cv", number = 5)
log_reg_cv <- train(permiso_reemplazado[, predictors], permiso_reemplazado[, target], method = "glmnet", trControl = train_control, tuneGrid = log_reg_grid)
print(log_reg_cv)

# Validación Cruzada para otros modelos
# Decision Tree
set.seed(42)
dt_grid <- expand.grid(cp = seq(0.01, 0.1, by = 0.01))
dt_cv <- train(permiso_reemplazado[, predictors], permiso_reemplazado[, target], method = "rpart", trControl = train_control, tuneGrid = dt_grid)
print(dt_cv)

# Random Forest
set.seed(42)
rf_grid <- expand.grid(mtry = c(2, 4, 6, 8))
rf_cv <- train(permiso_reemplazado[, predictors], permiso_reemplazado[, target], method = "rf", trControl = train_control, tuneGrid = rf_grid)
print(rf_cv)

# XGBoost
set.seed(42)
xgb_grid <- expand.grid(nrounds = c(50, 100), max_depth = c(3, 6, 9), eta = c(0.01, 0.1, 0.3))
xgb_cv <- train(permiso_reemplazado[, predictors], permiso_reemplazado[, target], method = "xgbTree", trControl = train_control, tuneGrid = xgb_grid)
print(xgb_cv)

# Prueba de hipótesis para la precisión del modelo
hypothesis_test_results <- data.frame(Model = character(), p_value = double())

for (model_name in names(models)) {
  model <- models[[model_name]]
  for (dataset_name in names(datasets)) {
    data_scaled <- datasets[[dataset_name]][[1]]
    data_original <- datasets[[dataset_name]][[2]]
    predictions <- predict(model, data_scaled[, predictors])
    confusion <- confusionMatrix(predictions, data_original[, target])
    accuracy <- confusion$overall['Accuracy']
    if (accuracy >= 0.90) {
      p_value <- binom.test(sum(predictions == data_original[, target]), length(predictions), p = 0.90)$p.value
      hypothesis_test_results <- rbind(hypothesis_test_results, data.frame(Model = model_name, p_value = p_value))
    }
  }
}

print(hypothesis_test_results)
