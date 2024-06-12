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


# Función para reemplazar valores 0, "" y "NULL" con NA
reemplazar_nulos <- function(x) {
  ifelse(x %in% c( "", "NULL", "Null", "null"), NA, x)
}


# Función para determinar el tamaño de la muestra
tam.muestra <- function(alfa, epsilon, s, N = Inf) {
  za2 <- qnorm(1 - alfa / 2)
  n <- if (N == Inf) (s * za2 / epsilon)^2 else N * ((za2 * s)^2) / ((N - 1) * epsilon^2 + (za2 * s)^2)
  return(ceiling(n))
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


grafico_cualitativo <- function(data, var_cual_x, tipo_grafico) {
  
  # Verificar que la variable cualitativa sea un factor
  if (!is.factor(data[[var_cual_x]])) {
    data[[var_cual_x]] <- as.factor(data[[var_cual_x]])
  }
  
  # Crear el gráfico base
  p <- ggplot(data, aes(x = !!sym(var_cual_x), fill = !!sym(var_cual_x))) +
    theme_minimal()
  
  # Añadir diferentes tipos de gráficos
  p <- switch(tipo_grafico,
              "torta" = {
                data_pie <- data %>%
                  count(!!sym(var_cual_x)) %>%
                  mutate(percentage = n / sum(n))
                ggplot(data_pie, aes(x = "", y = percentage, fill = !!sym(var_cual_x))) +
                  geom_bar(stat = "identity", width = 1) +
                  coord_polar("y") +
                  geom_text(aes(label = scales::percent(percentage)), position = position_stack(vjust = 0.5)) +
                  labs(title = paste("Proporción de categorías de", var_cual_x), x = NULL, y = NULL, caption = paste("Este gráfico de torta muestra la proporción de cada categoría de", var_cual_x, ".")) +
                  theme_void()
              },
              "anillo" = {
                data_pie <- data %>%
                  count(!!sym(var_cual_x)) %>%
                  mutate(percentage = n / sum(n))
                ggplot(data_pie, aes(x = 2, y = percentage, fill = !!sym(var_cual_x))) +
                  geom_bar(stat = "identity", width = 1) +
                  coord_polar(theta = "y") +
                  xlim(0.5, 2.5) +
                  geom_text(aes(label = scales::percent(percentage)), position = position_stack(vjust = 0.5)) +
                  labs(title = paste("Proporción de categorías de", var_cual_x), x = NULL, y = NULL, caption = paste("Este gráfico de anillo muestra la proporción de cada categoría de", var_cual_x, ".")) +
                  theme_void() +
                  theme(legend.position = "none",
                        axis.text = element_blank(),
                        axis.ticks = element_blank())
              },
              "barra" = ggplot(data, aes(x = !!sym(var_cual_x), fill = !!sym(var_cual_x))) +
                geom_bar() +
                labs(title = paste("Frecuencia de", var_cual_x), x = var_cual_x, y = "Frecuencia", caption = paste("Este gráfico de barras muestra la frecuencia de", var_cual_x, ".")),
              stop(paste(tipo_grafico, " Tipo de gráfico no soportado."))
  )
  
  # Aplicar tema minimalista y formato de etiquetas en eje y
  p <- p + theme_minimal() +
    scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ","))
  
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
              "densidad" = ggplot(data, aes(x = !!x, fill = ..density..)) +
                geom_density(alpha = 0.5) +
                scale_fill_gradientn(colors = colores) +
                labs(title = paste("Densidad de", var_cuant_x), x = var_cuant_x, y = "Densidad", caption = paste("Este gráfico de densidad muestra la distribución de", var_cuant_x, ".")),
              "barra" = ggplot(data, aes(x = !!x, fill = !!x)) +
                geom_bar(stat = "count") +
                geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
                scale_fill_manual(values = colores) +
                labs(title = paste("Frecuencia de", var_cuant_x), x = var_cuant_x, y = "Frecuencia", caption = paste("Este gráfico de barras muestra la frecuencia de", var_cuant_x, ".")),
              "violin" = ggplot(data, aes(x = factor(1), y = !!x)) +
                geom_violin(aes(fill = factor(1))) +
                scale_fill_manual(values = colores) +
                labs(title = paste("Distribución de", var_cuant_x), x = NULL, y = var_cuant_x, caption = paste("Este gráfico de violín muestra la distribución de", var_cuant_x, ".")) +
                theme(legend.position = "none"),  # Eliminar la leyenda
              "boxplot" = {
               
                ggplot(data, aes(x = factor(1), y = !!x)) +
                  geom_boxplot(alpha = 0.5, outlier.shape = NA, aes(fill = factor(1))) + 
                  geom_jitter(shape = 16, position = position_jitter(0.2), size = 2, alpha = 0.7) +
                  scale_fill_manual(values = colores) +
                  scale_color_manual(values = colores) +
                  theme_minimal() +
                  theme(legend.position = "none") +  # Eliminar la leyenda
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

paleta_colores <- function() {
  colores <- paletteer::paletteer_d("ggthemes::Classic_10_Light")
  return(colores)
}



# Función para crear un gráfico combinado
grafico_combinado <- function(data, var_cuant, var_cual) {
  # Definir las variables y, t

  
  y <- sym(var_cuant)
  t <- sym(var_cual)
  
  # Asegurar que la variable cualitativa es un factor
  data[[var_cual]] <- as.factor(data[[var_cual]])
  
  # Variables locales para etiquetas
  label_x <- var_cual
  label_y <- var_cuant
  label_titulo <- paste( var_cuant, "por", var_cual)
  
  # Obtener la paleta de colores
  colores <- paleta_colores()
  
  # Calcular por tipo de vehículo la cantidad de permisos pagados
  cantidad_por_cual <- aggregate(data[[as.character(y)]] ~ data[[as.character(t)]], data = data, length)
  colnames(cantidad_por_cual) <- c("Variable_Cualitativa", "count")
  
  # Calcular el permiso promedio por tipo de vehículo
  promedio_por_cual <- aggregate(data[[as.character(y)]] ~ data[[as.character(t)]], data = data, mean)
  colnames(promedio_por_cual) <- c("Variable_Cualitativa", "Promedio_Cuantitativo")
  
  # Combinar las dos tablas
  datos_combinados <- merge(promedio_por_cual, cantidad_por_cual, by = "Variable_Cualitativa")
  
  # Obtener el máximo de la cantidad para ajustar las escalas
  max_count <- max(datos_combinados$count)
  
  # Crear el gráfico combinado
  p <- ggplot(datos_combinados, aes(x = reorder(Variable_Cualitativa, count))) +
    geom_bar(aes(y = count), stat = "identity", fill = "lightblue", alpha = 0.6) +
    geom_text(aes(y = count, label = scales::comma(count, big.mark = ".", decimal.mark = ",")), hjust = -0.2, color = "blue", size = 3) +
    geom_line(aes(y = Promedio_Cuantitativo * (max_count / max(Promedio_Cuantitativo))), color = "red", size = 1, group = 1) +
    geom_point(aes(y = Promedio_Cuantitativo * (max_count / max(Promedio_Cuantitativo))), color = "red", size = 2) +
    geom_text(aes(y = Promedio_Cuantitativo * (max_count / max(Promedio_Cuantitativo)), label = scales::comma(Promedio_Cuantitativo, big.mark = ".", decimal.mark = ",")), hjust = -0.2, color = "red", size = 3) +
    scale_y_continuous(
      name = "Cantidad de Datos",
      sec.axis = sec_axis(~ . / (max_count / max(datos_combinados$Promedio_Cuantitativo)), name = "Promedio Cuantitativo", labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) 
    ) +
    labs(title = label_titulo,
         x = label_x) +
    coord_flip() +
    theme_minimal()
  
  # Mostrar el gráfico
  print(p)
}


# Función para crear diferentes tipos de gráficos
grafico <- function(data, var_cuant_x = NULL, var_cual_x = NULL, var_cuant_y = NULL, tipo_grafico, tipo_calculo = NULL) {

    # Funcion para generar grafico de correlacion
    generar_mapa_correlacion <- function(data) {
    numeric_vars <- data %>% select_if(is.numeric)
    cor_matrix <- cor(numeric_vars, use = "complete.obs")
    melted_cor_matrix <- melt(cor_matrix)
    colores <- paleta_colores()
    ggplot(melted_cor_matrix, aes(Var1, Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradientn(colors = colores, limits = c(-1, 1)) +
      labs(title = "Mapa de Calor de Correlaciones", x = "Variables", y = "Variables", 
           caption = "Este gráfico muestra la correlación entre diferentes variables numéricas en el dataset.
           Las correlaciones van de -1 (inversamente relacionados) a 1 (directamente relacionados).") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  # Función para generar el gráfico del codo
  generar_grafico_codo <- function(data, var_cuant_x) {
    var_cuant_data <- data[[var_cuant_x]]
    if (!is.numeric(var_cuant_data)) {
      stop("La variable cuantitativa seleccionada debe ser numérica para el gráfico de cluster")
    }
    # Convertir el vector en un dataframe y limpiar los datos
    var_cuant_data_clean <- data.frame(Valor = var_cuant_data) %>%
      filter(!is.na(Valor) & !is.nan(Valor) & !is.infinite(Valor))
    fviz_nbclust(var_cuant_data_clean, kmeans, method = "wss") +
      labs(title = "Número óptimo de clusters", x = "Número de Clusters k", y = "Suma total de las distancias al cuadrado",
           caption = "El número óptimo de clusters se encuentra donde la reducción en la suma de cuadrados comienza a disminuir.") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal()
  }
  
  # Verificar el tipo de gráfico y generar el correspondiente
  switch(tipo_grafico,
         "mapa correlacion" = {
           if (is.null(var_cuant_x) && is.null(var_cual_x)) {
             print(generar_mapa_correlacion(data))
             return()
           } else {
             stop("Debe especificar las variables cuantitativa y cualitativa, o elegir 'mapa correlacion' como tipo de gráfico.")
           }
         },
         "cluster" = {
           if (!is.null(var_cuant_x)) {
             print(generar_grafico_codo(data, var_cuant_x))
             return()
           } else {
             stop("Debe especificar una variable cuantitativa para el gráfico del codo.")
           }
         }
  )
  
  # Definir las variables x, y, t
  y <- if (!is.null(var_cuant_x)) sym(var_cuant_x) else NULL
  t <- if (!is.null(var_cual_x)) sym(var_cual_x) else NULL
  x <- if (!is.null(var_cuant_y)) sym(var_cuant_y) else NULL
  
  # Asegurar que la variable cualitativa es un factor
  if (!is.null(var_cual_x)) {
    data[[var_cual_x]] <- as.factor(data[[var_cual_x]])
  }
  
  # Variables locales para etiquetas
  label_x <- if (!is.null(var_cuant_y)) var_cuant_y else var_cual_x
  label_y <- var_cuant_x
  label_titulo <- paste(tipo_calculo, "de", var_cuant_x, "por", var_cual_x)
  
  # Generar una paleta de colores basada en la cantidad de niveles en var_cual_x
  niveles <- if (!is.null(var_cual_x)) length(levels(data[[var_cual_x]])) else 1
  colores <- scales::hue_pal()(niveles)
  
  # Agrupar y calcular según el tipo de cálculo si es necesario
  if (!is.null(tipo_calculo)) {
    data <- data %>%
      group_by(!!t, !!x) %>%
      summarise(
        Valor = case_when(
          tipo_calculo == "suma" ~ sum(!!y, na.rm = TRUE),
          tipo_calculo == "promedio" ~ mean(!!y, na.rm = TRUE),
          tipo_calculo == "cuenta" ~ n()
        ), .groups = 'drop'
      )
  } else {
    data <- data %>%
      mutate(Valor = !!y)
  }
  
  # Ordenar los datos de mayor a menor
  if (!is.null(y) && !is.null(t)) {
    data <- data %>%
      arrange(desc(Valor))
  }
  
  # Crear el gráfico base
  p <- ggplot(data, aes(x = !!t, y = Valor, fill = !!t)) +
    scale_color_manual(values = colores) +
    scale_fill_manual(values = colores) +
    theme_minimal()
  
  # Añadir diferentes tipos de gráficos
  p <- switch(tipo_grafico,
              "torta" = {
                data_pie <- data %>%
                  count(!!t) %>%
                  mutate(percentage = n / sum(n))
                ggplot(data_pie, aes(x = "", y = percentage, fill = !!t)) +
                  geom_bar(stat = "identity", width = 1) +
                  coord_polar("y") +
                  geom_text(aes(label = scales::percent(percentage)), position = position_stack(vjust = 0.5)) +
                  labs(title = paste("Proporción de categorías de", label_x), x = NULL, y = NULL, caption = paste("Este gráfico de torta muestra la proporción de cada categoría de", label_x, ".")) +
                  theme_void()
              },
              "anillo" = {
                data_pie <- data %>%
                  count(!!t) %>%
                  mutate(percentage = n / sum(n))
                ggplot(data_pie, aes(x = 2, y = percentage, fill = !!t)) +
                  geom_bar(stat = "identity", width = 1) +
                  coord_polar(theta = "y") +
                  xlim(0.5, 2.5) +
                  geom_text(aes(label = scales::percent(percentage)), position = position_stack(vjust = 0.5)) +
                  labs(title = paste("Proporción de categorías de", label_x), x = NULL, y = NULL, caption = paste("Este gráfico de anillo muestra la proporción de cada categoría de", label_x, ".")) +
                  theme_void() +
                  theme(legend.position = "none",
                        axis.text = element_blank(),
                        axis.ticks = element_blank())
              },
              "caja" = ggplot(data, aes(x = !!t, y = !!y, fill = !!t)) +
                geom_boxplot() +
                labs(title = paste("Distribución de", label_y, "por categorías de", label_x), x = label_x, y = label_y, caption = paste("Este gráfico de caja muestra la distribución de", label_y, "por las diferentes categorías de", label_x, ".")),
              "caja con puntos" = {
                niveles <- if (!is.null(var_cual_x)) length(levels(data[[var_cual_x]])) else 1
                colores <- scales::hue_pal()(niveles)
                colores_oscuros <- scales::hue_pal()(niveles) %>%
                  sapply(function(col) adjustcolor(col, alpha.f = 0.8))
                ggplot(data, aes(x = !!t, y = !!y, fill = !!t)) +
                  geom_boxplot(alpha = 0.5, outlier.shape = NA) + 
                  geom_jitter(shape = 16, position = position_jitter(0.2), size = 2, aes(color = !!t), alpha = 0.7) +
                  scale_fill_manual(values = colores) +
                  scale_color_manual(values = colores_oscuros) +
                  labs(title = paste("Distribución de", label_y, "por categorías de", label_x), x = label_x, y = label_y, caption = paste("Este gráfico de caja con puntos muestra la distribución de", label_y, "por las diferentes categorías de", label_x, ".")) +
                  theme_minimal() +
                  theme(legend.position = "none")
              },
              "histograma" = ggplot(data, aes(x = !!y, fill = !!t)) +
                geom_histogram(position = "dodge", binwidth = 10) +
                labs(title = paste("Distribución de", label_y, "agrupada por", label_x), x = label_y, y = "Frecuencia", caption = paste("Este histograma muestra la distribución de", label_y, "agrupada por las categorías de", label_x, ".")),
              "barra" = ggplot(data, aes(x = reorder(!!t, Valor), y = Valor, fill = !!t)) +
                geom_bar(stat = "identity") +
                geom_text(aes(label = scales::comma(Valor, big.mark = ".", decimal.mark = ",")), hjust = 1.2) +
                labs(title = paste("Suma de", label_y, "por categorías de", label_x), x = label_x, y = tipo_calculo, caption = paste("Este gráfico de barras muestra la suma de", label_y, "para cada categoría de", label_x, ".")) +
                theme(axis.text.y = element_text(hjust = 0.5, vjust = 0.5)) +
                coord_flip(),
              "columnas" = ggplot(data, aes(x = reorder(!!t, -Valor), y = Valor, fill = !!t)) +
                geom_col() +
                geom_text(aes(label = scales::comma(Valor, big.mark = ".", decimal.mark = ",")), vjust = -0.5) +
                labs(title = paste("Suma de", label_y, "por categorías de", label_x), x = label_x, y = tipo_calculo, caption = paste("Este gráfico de columnas muestra la suma de", label_y, "para cada categoría de", label_x, ".")) +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)),
              "dispercion" = ggplot(data, aes(x = !!t, y = !!y, color = !!t)) +
                geom_point() +
                labs(title = paste("Relación entre", label_y, "y", label_x), x = label_x, y = label_y, caption = paste("Este gráfico de dispersión muestra la relación entre", label_y, "y", label_x, ".")),
              "densidad" = ggplot(data, aes(x = !!y, fill = !!t)) +
                geom_density(alpha = 0.5) +
                labs(title = paste("Densidad de", label_y, "por categorías de", label_x), x = label_y, y = "Densidad", caption = paste("Este gráfico de densidad muestra la distribución de", label_y, "para cada categoría de", label_x, ".")),
              "violin" = ggplot(data, aes(x = !!t, y = !!y, fill = !!t)) +
                geom_violin() +
                labs(title = paste("Distribución de", label_y, "por categorías de", label_x), x = label_x, y = label_y, caption = paste("Este gráfico de violín muestra la distribución de", label_y, "para cada categoría de", label_x, ".")),
              "lineas" = ggplot(data, aes(x = !!x, y = Valor, color = !!t, group = !!t)) +
                geom_line() +
                geom_point() +
                labs(title = paste("Tendencia de", tipo_calculo, "de", label_y, "por", label_x), x = label_x, y = paste(tipo_calculo, "de", label_y), caption = paste("Este gráfico de líneas muestra la tendencia de", tipo_calculo, "de", label_y, "a lo largo de", label_x, ".")),
              "lineas acumuladas" = {
                data <- data %>%
                  group_by(!!t) %>%
                  arrange(!!x) %>%
                  mutate(Valor = cumsum(Valor))
                ggplot(data, aes(x = !!x, y = Valor, color = !!t, group = !!t)) +
                  geom_line() +
                  geom_point() +
                  scale_color_manual(values = colores) +
                  labs(title = paste("Suma acumulada de", tipo_calculo, "de", label_y, "por", label_x), x = label_x, y = paste(tipo_calculo, "de", label_y), caption = paste("Este gráfico de líneas acumuladas muestra la suma acumulada de", tipo_calculo, "de", label_y, "a lo largo de", label_x, ".")) +
                  theme_minimal()
              },
              "puntos" = if (!is.null(var_cuant_y)) {
                x <- sym(var_cuant_y)
                ggplot(data, aes(x = !!x, y = !!y, color = !!t)) +
                  geom_point(size = 3) +
                  labs(title = paste("Relación entre", label_y, "y", label_x), x = label_x, y = label_y, caption = paste("Este gráfico de puntos muestra la relación entre", label_y, "y", label_x, "categorizado por", label_x, ".")) +
                  theme_minimal() +
                  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
              } else {
                stop("El gráfico de puntos requiere una segunda variable cuantitativa.")
              },
              "burbujas" = ggplot(data, aes(x = !!t, y = !!y, size = !!y, color = !!t)) +
                geom_point(alpha = 0.5) +
                labs(title = paste("Relación entre", label_y, "y", label_x), x = label_x, y = label_y, caption = paste("Este gráfico de burbujas muestra la relación entre", label_y, "y", label_x, "con el tamaño de las burbujas representando", label_y, ".")),
             
              "distribucion normal" = {
                ggplot(data, aes(x = !!y)) +
                  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "yellow", color = "black") +
                  stat_function(fun = dnorm, args = list(mean = mean(data[[as.character(y)]], na.rm = TRUE), 
                                                         sd = sd(data[[as.character(y)]], na.rm = TRUE)), color = "red", size = 1) +
                  labs(title = paste("Comparación de la distribución de", var_cuant_x, "con una distribución normal"), x = var_cuant_x, y = "Densidad", caption = paste("Este gráfico compara la distribución de", var_cuant_x, "con una distribución normal teórica.")) +
                  theme_minimal()
              },stop(paste(tipo_grafico, " Tipo de gráfico no soportado."))
  )
  
  # Aplicar tema minimalista y formato de etiquetas en eje y
  p <- p + theme_minimal() +
    scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
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
  
  # Crear el gráfico base
  p <- ggplot(data, aes(x = !!x)) +
    theme_minimal()
  
  # Añadir diferentes tipos de gráficos
  p <- switch(tipo_grafico,
              "histograma" = ggplot(data, aes(x = !!x)) +
                geom_histogram(binwidth = 10) +
                labs(title = paste("Distribución de", var_cuant_x), x = var_cuant_x, y = "Frecuencia", caption = paste("Este histograma muestra la distribución de", var_cuant_x, ".")),
              "densidad" = ggplot(data, aes(x = !!x)) +
                geom_density(alpha = 0.5) +
                labs(title = paste("Densidad de", var_cuant_x), x = var_cuant_x, y = "Densidad", caption = paste("Este gráfico de densidad muestra la distribución de", var_cuant_x, ".")),
              "barra" = ggplot(data, aes(x = !!x)) +
                geom_bar(stat = "count") +
                labs(title = paste("Frecuencia de", var_cuant_x), x = var_cuant_x, y = "Frecuencia", caption = paste("Este gráfico de barras muestra la frecuencia de", var_cuant_x, ".")),
              "violin" = ggplot(data, aes(x = factor(1), y = !!x)) +
                geom_violin() +
                labs(title = paste("Distribución de", var_cuant_x), x = NULL, y = var_cuant_x, caption = paste("Este gráfico de violín muestra la distribución de", var_cuant_x, ".")),
              stop(paste(tipo_grafico, " Tipo de gráfico no soportado."))
  )
  
  # Aplicar tema minimalista y formato de etiquetas en eje y
  p <- p + theme_minimal() +
    scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ","))
  
  # Mostrar el gráfico
  print(p)
}




grafico_cualitativo <- function(data, var_cual_x, tipo_grafico) {
  
  # Verificar que la variable cualitativa sea un factor
  if (!is.factor(data[[var_cual_x]])) {
    data[[var_cual_x]] <- as.factor(data[[var_cual_x]])
  }
  
  # Crear el gráfico base
  p <- ggplot(data, aes(x = !!sym(var_cual_x), fill = !!sym(var_cual_x))) +
    theme_minimal()
  
  # Añadir diferentes tipos de gráficos
  p <- switch(tipo_grafico,
              "torta" = {
                data_pie <- data %>%
                  count(!!sym(var_cual_x)) %>%
                  mutate(percentage = n / sum(n))
                ggplot(data_pie, aes(x = "", y = percentage, fill = !!sym(var_cual_x))) +
                  geom_bar(stat = "identity", width = 1) +
                  coord_polar("y") +
                  geom_text(aes(label = scales::percent(percentage)), position = position_stack(vjust = 0.5)) +
                  labs(title = paste("Proporción de categorías de", var_cual_x), x = NULL, y = NULL, caption = paste("Este gráfico de torta muestra la proporción de cada categoría de", var_cual_x, ".")) +
                  theme_void()
              },
              "anillo" = {
                data_pie <- data %>%
                  count(!!sym(var_cual_x)) %>%
                  mutate(percentage = n / sum(n))
                ggplot(data_pie, aes(x = 2, y = percentage, fill = !!sym(var_cual_x))) +
                  geom_bar(stat = "identity", width = 1) +
                  coord_polar(theta = "y") +
                  xlim(0.5, 2.5) +
                  geom_text(aes(label = scales::percent(percentage)), position = position_stack(vjust = 0.5)) +
                  labs(title = paste("Proporción de categorías de", var_cual_x), x = NULL, y = NULL, caption = paste("Este gráfico de anillo muestra la proporción de cada categoría de", var_cual_x, ".")) +
                  theme_void() +
                  theme(legend.position = "none",
                        axis.text = element_blank(),
                        axis.ticks = element_blank())
              },
              "barra" = ggplot(data, aes(x = !!sym(var_cual_x), fill = !!sym(var_cual_x))) +
                geom_bar() +
                labs(title = paste("Frecuencia de", var_cual_x), x = var_cual_x, y = "Frecuencia", caption = paste("Este gráfico de barras muestra la frecuencia de", var_cual_x, ".")),
              stop(paste(tipo_grafico, " Tipo de gráfico no soportado."))
  )
  
  # Aplicar tema minimalista y formato de etiquetas en eje y
  p <- p + theme_minimal() +
    scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ","))
  
  # Mostrar el gráfico
  print(p)
}
