# Instalar y cargar las librerías necesarias
required_packages <- c("ggplot2", "gridExtra", "dplyr", "stringr", 
                       "paletteer","palette", "scales","lubridate",
                       "prophet","reshape2","e1071","tibble")
installed_packages <- rownames(installed.packages())

for (pkg in required_packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
}

lapply(required_packages, library, character.only = TRUE)

tam.muestra <- function(alfa, epsilon, s, N = Inf) {
  za2 <- qnorm(1 - alfa / 2)
  n <- if (N == Inf) (s * za2 / epsilon)^2 else N * ((za2 * s)^2) / ((N - 1) * epsilon^2 + (za2 * s)^2)
  return(ceiling(n))
}

reemplazar_nulos <- function(x) {
  ifelse(x %in% c( "", "NULL", "Null", "null"), NA, x)
}

reemplazar_por_mediana <- function(x) {
  if (is.numeric(x) && any(is.na(x))) {
    x[is.na(x)] <- median(x, na.rm = TRUE)
  }
  return(x)
}

paleta_colores <- function() {
  colores <- paletteer::paletteer_d("ggthemes::Classic_10_Light")
  return(colores)
}

grafico_cualitativo <- function(data, var_cual_x, tipo_grafico) {
  
  # Verificar que la variable cualitativa sea un factor
  if (!is.factor(data[[var_cual_x]])) {
    data[[var_cual_x]] <- as.factor(data[[var_cual_x]])
  }
  
  # Obtener la paleta de colores
  colores <- paleta_colores()
  
  # Crear el gráfico base
  p <- ggplot(data, aes(x = !!sym(var_cual_x), fill = !!sym(var_cual_x))) 
  
  # Añadir diferentes tipos de gráficos
  p <- switch(tipo_grafico,
              "torta" = {
                data_pie <- data %>%
                  count(!!sym(var_cual_x)) %>%
                  mutate(percentage = n / sum(n))
                ggplot(data_pie, aes(x = "", y = percentage, fill = !!sym(var_cual_x))) +
                  geom_bar(stat = "identity", width = 1) +
                  coord_polar("y") +
                  geom_text(aes(label = scales::percent(percentage), y = cumsum(percentage) - percentage / 2)) +
                  labs(title = paste("Proporción de categorías de", var_cual_x), x = NULL, y = NULL, caption = paste("Este gráfico de torta muestra la proporción de cada categoría de", var_cual_x, ".")) +
                  scale_fill_manual(values = colores) +
                  theme_void() +
                  
                  theme(legend.background = element_blank())
              },
              "anillo" = {
                data_pie <- data %>%
                  count(!!sym(var_cual_x)) %>%
                  mutate(percentage = n / sum(n))
                ggplot(data_pie, aes(x = 2, y = percentage, fill = !!sym(var_cual_x))) +
                  geom_bar(stat = "identity", width = 1) +
                  coord_polar(theta = "y") +
                  xlim(0.5, 2.5) +
                  geom_text(aes(label = scales::percent(percentage), y = cumsum(percentage) - percentage / 2)) +
                  labs(title = paste("Proporción de categorías de", var_cual_x), x = NULL, y = NULL, caption = paste("Este gráfico de anillo muestra la proporción de cada categoría de", var_cual_x, ".")) +
                  scale_fill_manual(values = colores) +
                  theme_void() +
                  theme(legend.position = "right")
              },
              "barra" = {
                ggplot(data, aes(x = !!sym(var_cual_x), fill = !!sym(var_cual_x))) +
                  geom_bar() +
                  geom_text(stat='count', aes(label=after_stat(count)), angle = 90, vjust = 0.5) +
                  labs(title = paste("Frecuencia de", var_cual_x), x = var_cual_x, y = "Frecuencia", caption = paste("Este gráfico de barras muestra la frecuencia de", var_cual_x, ".")) +
                  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + #
                  scale_fill_manual(values = colores) +
                  theme_void() +
                  theme(legend.background = element_blank())
              },
              stop(paste(tipo_grafico, "Tipo de gráfico no soportado."))
  )
  
  # Mostrar el gráfico
  print(p)
}


grafico_cuantitativo <- function(data, var_cuant_x, tipo_grafico) {
  # Verificar si la variable es numérica
  if (!is.numeric(data[[var_cuant_x]])) {
    stop("La variable cuantitativa seleccionada debe ser numérica.")
  }
  
  # Definir la variable x
  x <- sym(var_cuant_x)
  
  # Obtener la paleta de colores
  colores <- paleta_colores()
  
  # Calcular estadísticos para el boxplot
  stats <- data %>% 
    summarise(
      Min = min(!!x, na.rm = TRUE),
      Q1 = quantile(!!x, 0.25, na.rm = TRUE),
      Mediana = median(!!x, na.rm = TRUE),
      Q3 = quantile(!!x, 0.75, na.rm = TRUE),
      Max = max(!!x, na.rm = TRUE)
    )
  
  IQR <- stats$Q3 - stats$Q1
  bigote_superior <- min(stats$Max, stats$Q3 + 1.5 * IQR)
  bigote_inferior <- max(stats$Min, stats$Q1 - 1.5 * IQR)
  
  # Crear el gráfico base
  p <- ggplot(data, aes(x = factor(1), y = !!x)) +
    theme_minimal() +
    scale_fill_manual(values = colores) +
    theme(legend.position = "none")  # Eliminar la leyenda
  
  # Añadir diferentes tipos de gráficos
  p <- switch(tipo_grafico,
              "histograma" = ggplot(data, aes(x = !!x, fill = ..count..)) +
                geom_histogram(binwidth = 10) +
                geom_text(stat='count', aes(label=..count..), angle = 90, hjust = -0.1, vjust = 0.5) +
                scale_fill_gradientn(colors = colores) +
                labs(title = paste("Distribución de", var_cuant_x), x = var_cuant_x, y = "Frecuencia", caption = paste("Este histograma muestra la distribución de", var_cuant_x, ".")),
              "violin" = ggplot(data, aes(x = factor(1), y = !!x)) +
                geom_violin(aes(fill = factor(1))) +
                scale_fill_manual(values = colores) +
                labs(title = paste("Distribución de", var_cuant_x), x = NULL, y = var_cuant_x, caption = paste("Este gráfico de violín muestra la distribución de", var_cuant_x, ".")) +
                theme(legend.position = "none"),
              "boxplot" = {
                
                ggplot(data, aes(x = factor(1), y = !!x)) +
                  geom_boxplot(alpha = 0.5, outlier.shape = NULL, aes(fill = factor(1))) + 
                  geom_jitter(shape = 10, position = position_jitter(0.15), size = 1, alpha = 0.7) +
                  scale_fill_manual(values = colores) +
                  scale_color_manual(values = colores) +
                  theme_minimal() +
                  theme(legend.position = "none") + 
                  annotate("label", x = 1.3, y = stats$Min, label = paste("Min:", round(stats$Min, 2)), hjust = 0, size = 3, fill = "grey90") +
                  annotate("label", x = 1.3, y = stats$Q1, label = paste("Q1:", round(stats$Q1, 2)), hjust = 0, size = 3, fill = "grey90") +
                  annotate("label", x = 1.3, y = stats$Mediana, label = paste("Mediana:", round(stats$Mediana, 2)), hjust = 0, size = 3, fill = "grey90") +
                  annotate("label", x = 1.3, y = stats$Q3, label = paste("Q3:", round(stats$Q3, 2)), hjust = 0, size = 3, fill = "grey90") +
                  annotate("label", x = 1.3, y = bigote_superior, label = paste("Bigote Sup.:", round(bigote_superior, 2)), hjust = 0, size = 3, fill = "grey90") +
                  annotate("label", x = 1.3, y = bigote_inferior, label = paste("Bigote Inf.:", round(bigote_inferior, 2)), hjust = 0, size = 3, fill = "grey90") +
                  annotate("label", x = 1.3, y = stats$Max, label = paste("Max:", round(stats$Max, 2)), hjust = 0, size = 3, fill = "grey90") +
                  labs(title = paste("Boxplot con puntos de", var_cuant_x), x = NULL, y = var_cuant_x, caption = paste("Este boxplot con puntos muestra la distribución de", var_cuant_x, "."))
              },
              stop(paste(tipo_grafico, " Tipo de gráfico no soportado."))
  )
  
  # Aplicar tema minimalista y formato de etiquetas en eje y
  p <- p + theme_minimal() +
    scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ","))
  
  # Mostrar el gráfico
  print(p)
}



grafico <- function(data, var, tipo_grafico) {
  # Verificar si la variable es cualitativa o cuantitativa
  if (is.factor(data[[var]]) || is.character(data[[var]])) {
    # Llamar a la función grafico_cualitativo
    grafico_cualitativo(data, var, tipo_grafico)
  } else if (is.numeric(data[[var]])) {
    # Llamar a la función grafico_cuantitativo
    grafico_cuantitativo(data, var, tipo_grafico)
  } else {
    stop("El tipo de variable no es soportado. Debe ser una variable cualitativa o cuantitativa.")
  }
}




grafico_dispersion <- function(data, var_x, var_y) {
  # Verificar si las variables son numéricas
  if (!is.numeric(data[[var_x]]) || !is.numeric(data[[var_y]])) {
    stop("Ambas variables seleccionadas deben ser numéricas.")
  }
  
  # Obtener la paleta de colores
  colores <- paleta_colores()
  
  # Calcular la covarianza y el coeficiente de correlación
  covarianza <- cov(data[[var_x]], data[[var_y]], use = "complete.obs")
  correlacion <- cor(data[[var_x]], data[[var_y]], use = "complete.obs")
  
  # Evaluación de la relación
  abs_correlacion <- abs(correlacion)
  fuerza <- if (abs_correlacion < 0.1) {
    "Correlación inexistente"
  } else if (abs_correlacion < 0.3) {
    "Correlación débil"
  } else if (abs_correlacion < 0.5) {
    "Correlación moderada"
  } else {
    "Correlación fuerte"
  }
  
  direccion <- if (correlacion > 0) {
    "relación positiva (directa)"
  } else if (correlacion < 0) {
    "relación negativa (inversa)"
  } else {
    "sin relación"
  }
  
  evaluacion <- paste(fuerza, "con", direccion)
  
  # Crear el gráfico de dispersión
  p <- ggplot(data, aes(x = !!sym(var_x), y = !!sym(var_y))) +
    geom_point(color = colores[1], alpha = 0.6) +
    geom_smooth(method = "lm", color = colores[2], se = FALSE, formula = y ~ x) + # Añadir línea de tendencia con fórmula explícita
    theme_minimal() +
    labs(title = paste("Gráfico de Dispersión de", var_x, "vs", var_y),
         x = var_x,
         y = var_y,
         caption = paste("Este gráfico de dispersión muestra la relación entre", var_x, "y", var_y, "\nLa relaciòn es:", evaluacion)) +
    scale_x_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
    scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ","))
  
  # Obtener los límites actuales del gráfico
  xlim <- range(data[[var_x]], na.rm = TRUE)
  ylim <- range(data[[var_y]], na.rm = TRUE)
  
  # Añadir anotaciones de covarianza y correlación
  p <- p + annotate("text", x = xlim[2], y = ylim[2], 
                    label = paste("Covarianza:", round(covarianza, 6), "\nCorrelación:", round(correlacion, 6)),
                    hjust = 1, vjust = 1, size = 3, color = "black") 
  
  # Mostrar el gráfico
  print(p)
}

# Función para crear tablas con valores estadísticos y sus explicaciones
tabla_estadistica <- function(data, var_cuant, var_cual) {
  var_cuant_sym <- rlang::sym(var_cuant)
  var_cual_sym <- rlang::sym(var_cual)
  
  # Nombre del dataset
  dataset_name <- deparse(substitute(data))
  
  # Calcular moda
  moda <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # Calcular las estadísticas
  calculos <- data %>%
    group_by(!!var_cual_sym) %>%
    summarise(
      cantidad_registros = n(),
      media = mean(!!var_cuant_sym, na.rm = TRUE),
      mediana = median(!!var_cuant_sym, na.rm = TRUE),
      moda = moda(!!var_cuant_sym),
      desviacion_estandar = sd(!!var_cuant_sym, na.rm = TRUE),
      varianza = var(!!var_cuant_sym, na.rm = TRUE),
      minimo = min(!!var_cuant_sym, na.rm = TRUE),
      maximo = max(!!var_cuant_sym, na.rm = TRUE),
      q1 = quantile(!!var_cuant_sym, 0.25, na.rm = TRUE),
      q3 = quantile(!!var_cuant_sym, 0.75, na.rm = TRUE),
      rango_intercuartil = IQR(!!var_cuant_sym, na.rm = TRUE),
      coeficiente_variacion = sd(!!var_cuant_sym, na.rm = TRUE) / mean(!!var_cuant_sym, na.rm = TRUE),
      skewness = e1071::skewness(!!var_cuant_sym, na.rm = TRUE),
      kurtosis = e1071::kurtosis(!!var_cuant_sym, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Crear explicaciones para cada grupo en la variable cualitativa
  explicaciones <- bind_rows(lapply(seq_len(nrow(calculos)), function(i) {
    grupo <- calculos[[var_cual]][i]
    tibble::tibble(
      Tipo = paste(var_cual, grupo, sep = ":"),
      Variable = var_cuant,
      Medida = c(
        "Número de registros:",
        "Media:",
        "Mediana:",
        "Moda:",
        "Desviación estándar:",
        "Varianza:",
        "Valor mínimo:",
        "Valor máximo:",
        "Primer cuartil (25%):",
        "Tercer cuartil (75%):",
        "Rango intercuartil (Q3 - Q1):",
        "Coeficiente de variación (desviación estándar / media):",
        "Asimetría (skewness):",
        "Curtosis (kurtosis):"
      ),
      Valor = as.character(unlist(calculos[i, -1]))
    )
  }))
  
  # Generar resumen explicativo
  resumen <- lapply(seq_len(nrow(calculos)), function(i) {
    grupo <- calculos[[var_cual]][i]
    paste(
      "Para el grupo", grupo, "de la variable", var_cual, "en el dataset", dataset_name, "se observaron los siguientes valores:",
      "Número de registros:", calculos$cantidad_registros[i], ",",
      "Media:", round(calculos$media[i], 2), ",",
      "Mediana:", round(calculos$mediana[i], 2), ",",
      "Moda:", calculos$moda[i], ",",
      "Desviación estándar:", round(calculos$desviacion_estandar[i], 2), ",",
      "Varianza:", round(calculos$varianza[i], 2), ",",
      "Valor mínimo:", calculos$minimo[i], ",",
      "Valor máximo:", calculos$maximo[i], ",",
      "Primer cuartil (25%):", calculos$q1[i], ",",
      "Tercer cuartil (75%):", calculos$q3[i], ",",
      "Rango intercuartil (Q3 - Q1):", calculos$rango_intercuartil[i], ",",
      "Coeficiente de variación:", round(calculos$coeficiente_variacion[i], 2), ",",
      "Asimetría (skewness):", round(calculos$skewness[i], 2), ",",
      "Curtosis (kurtosis):", round(calculos$kurtosis[i], 2), "."
    )
  })
  
  # Título de la tabla
  titulo <- paste("Tabla detalle de variables estadísticas para el dataset", dataset_name, 
                  "con las variables", var_cuant, "y", var_cual)
  
  list(Titulo = titulo, Calculos = calculos, ExplicacionesValores = explicaciones, Resumen = resumen)
}



# Función para crear serie temporal y ajustar varios modelos
create_ts_models <- function(data, group, tipo_pago, tipo_vehiculo, n_months) {
  subset_data <- data %>%
    filter(`Grupo_Vehiculo` == group, `Tipo_de_Pago` == tipo_pago, `Tipo_Vehiculo` == tipo_vehiculo)
  
  if (nrow(subset_data) > 0) {
    ts_data <- ts(subset_data$Pagos, start = c(year(min(subset_data$YearMonth)), month(min(subset_data$YearMonth))), frequency = 12)
    
    # Dividir los datos en entrenamiento y prueba
    train_size <- round(length(ts_data) * 0.8)
    train_ts <- ts_data[1:train_size]
    test_ts <- ts_data[(train_size + 1):length(ts_data)]
    
    # Modelos de series temporales
    arima_model <- auto.arima(train_ts)
    hw_model <- HoltWinters(train_ts)
    ses_model <- ets(train_ts)
    
    # Preprocesamiento para XGBoost
    train_matrix <- as.matrix(data.frame(value = train_ts))
    test_matrix <- as.matrix(data.frame(value = test_ts))
    dtrain <- xgb.DMatrix(data = train_matrix, label = train_ts)
    dtest <- xgb.DMatrix(data = test_matrix, label = test_ts)
    
    # Normalización
    preProcessParams <- preProcess(as.data.frame(train_matrix), method = c("center", "scale"))
    train_matrix_normalized <- predict(preProcessParams, as.data.frame(train_matrix))
    test_matrix_normalized <- predict(preProcessParams, as.data.frame(test_matrix))
    
    dtrain_normalized <- xgb.DMatrix(data = as.matrix(train_matrix_normalized), label = train_ts)
    dtest_normalized <- xgb.DMatrix(data = as.matrix(test_matrix_normalized), label = test_ts)
    
    # Parámetros y entrenamiento de XGBoost
    param <- list(objective = "reg:squarederror", max_depth = 6, eta = 0.1, nthread = 2)
    xgb_model <- xgb.train(params = param, data = dtrain_normalized, nrounds = 100)
    
    xgb_predictions <- predict(xgb_model, dtest_normalized)
    
    # Realizar predicciones para los próximos n meses
    forecast_arima <- forecast(arima_model, h = n_months)
    forecast_hw <- forecast(hw_model, h = n_months)
    forecast_ses <- forecast(ses_model, h = n_months)
    forecast_xgb <- ts(xgb_predictions, start = c(year(end(test_ts)) + 1, 1), frequency = 12)
    
    # Almacenar los modelos y las predicciones
    models[[paste(group, tipo_pago, tipo_vehiculo, sep = "_")]] <<- list(
      arima = arima_model,
      hw = hw_model,
      ses = ses_model,
      xgb = xgb_model
    )
    
    predictions[[paste(group, tipo_pago, tipo_vehiculo, sep = "_")]] <<- list(
      arima = forecast_arima,
      hw = forecast_hw,
      ses = forecast_ses,
      xgb = forecast_xgb
    )
    
    # Evaluar precisión de los modelos
    accuracy_results[[paste(group, tipo_pago, tipo_vehiculo, sep = "_")]] <<- list(
      arima = accuracy(forecast_arima, test_ts),
      hw = accuracy(forecast_hw, test_ts),
      ses = accuracy(forecast_ses, test_ts),
      xgb = accuracy(xgb_predictions, test_ts)
    )
    
    return(list(
      arima = forecast_arima,
      hw = forecast_hw,
      ses = forecast_ses,
      xgb = forecast_xgb
    ))
  } else {
    return(NULL)
  }
}

