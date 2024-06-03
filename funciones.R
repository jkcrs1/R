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
  ifelse(x %in% c(0, "", "NULL", "Null", "null"), NA, x)
}


# Función para determinar el tamaño de la muestra
tam.muestra <- function(alfa, epsilon, s, N = Inf) {
  za2 <- qnorm(1 - alfa / 2)
  n <- if (N == Inf) (s * za2 / epsilon)^2 else N * ((za2 * s)^2) / ((N - 1) * epsilon^2 + (za2 * s)^2)
  return(ceiling(n))
}




# Función para crear una paleta de colores fija
paleta_colores <- function() {
  colores <- paletteer::paletteer_c("grDevices::Zissou 1", 30)
  return(colores)
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
grafico <- function(data, var_cuant_x, var_cual_x, var_cuant_y = NULL, tipo_grafico, tipo_calculo = NULL) {
  
  # Definir las variables x, y, t
  y <- sym(var_cuant_x)
  t <- sym(var_cual_x)
  x <- if (!is.null(var_cuant_y)) sym(var_cuant_y) else NULL
  
  # Asegurar que la variable cualitativa es un factor
  data[[var_cual_x]] <- as.factor(data[[var_cual_x]])
  
  # Variables locales para etiquetas
  label_x <- if (!is.null(var_cuant_y)) var_cuant_y else var_cual_x
  label_y <- var_cuant_x
  label_titulo <- paste(tipo_calculo, "de", var_cuant_x, "por", var_cual_x)
  
  # Generar una paleta de colores basada en la cantidad de niveles en var_cual_x
  niveles <- length(levels(data[[var_cual_x]]))
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
  data <- data %>%
    arrange(desc(Valor))
  
  # Crear el gráfico base
  p <- ggplot(data, aes(x = !!t, y = Valor, fill = !!t)) +
    scale_color_manual(values = colores) +
    scale_fill_manual(values = colores) +
    theme_minimal()
  
  # Añadir diferentes tipos de gráficos
  p <- switch(tipo_grafico,
              "caja" = ggplot(data, aes(x = !!t, y = !!y, fill = !!t)) +
                geom_boxplot() +
                labs(title = paste("Boxplot de", label_y, "por", label_x), x = label_x, y = label_y),
              "histograma" = ggplot(data, aes(x = !!y, fill = !!t)) +
                geom_histogram(position = "dodge", binwidth = 10) +
                labs(title = paste("Histograma de", label_y, "por", label_x), x = label_y, y = "Frecuencia"),
              "barra" = ggplot(data, aes(x = reorder(!!t, Valor), y = Valor, fill = !!t)) +
                geom_bar(stat = "identity") +
                geom_text(aes(label = scales::comma(Valor, big.mark = ".", decimal.mark = ",")), hjust = 1.2) +
                labs(title = paste("Barras de", label_y, "por", label_x), x = label_x, y = tipo_calculo) +
                theme(axis.text.y = element_text(hjust = 0.5, vjust = 0.5)) +
                coord_flip(),
              "columnas" = ggplot(data, aes(x = reorder(!!t, -Valor), y = Valor, fill = !!t)) +
                geom_col() +
                geom_text(aes(label = scales::comma(Valor, big.mark = ".", decimal.mark = ",")), vjust = -0.5) +
                labs(title = paste("Columnas de", label_y, "por", label_x), x = label_x, y = tipo_calculo) +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)),
              "dispercion" = ggplot(data, aes(x = !!t, y = !!y, color = !!t)) +
                geom_point() +
                labs(title = paste("Dispersión de", label_y, "por", label_x), x = label_x, y = label_y),
              "densidad" = ggplot(data, aes(x = !!y, fill = !!t)) +
                geom_density(alpha = 0.5) +
                labs(title = paste("Densidad de", label_y, "por", label_x), x = label_y, y = "Densidad"),
              "violin" = ggplot(data, aes(x = !!t, y = !!y, fill = !!t)) +
                geom_violin() +
                labs(title = paste("Violin de", label_y, "por", label_x), x = label_x, y = label_y),
              "lineas" = ggplot(data, aes(x = !!x, y = Valor, color = !!t, group = !!t)) +
                geom_line() +
                geom_point() +
                labs(title = paste("Líneas de", tipo_calculo, "de", label_y, "por", label_x, "de", label_titulo),
                     x = label_x, y = paste(tipo_calculo, "de", label_y)),
              "lineas acumuladas" = {
                data <- data %>%
                  group_by(!!t) %>%
                  arrange(!!x) %>%
                  mutate(Valor = cumsum(Valor))
                ggplot(data, aes(x = !!x, y = Valor, color = !!t, group = !!t)) +
                  geom_line() +
                  geom_point() +
                  scale_color_manual(values = colores) +
                  labs(title = paste("Líneas Acumuladas de", tipo_calculo, "de", label_y, "por", label_x, "de", label_titulo),
                       x = label_x, y = paste(tipo_calculo, "de", label_y))
              },
              "lineas multiplas" = ggplot(data, aes(x = !!x, y = !!y, color = !!t)) +
                geom_line() +
                labs(title = paste("Líneas Múltiples de", label_y, "por", label_x),
                     x = label_x, y = label_y) +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)),
              "puntos" = if (!is.null(var_cuant_y)) {
                x <- sym(var_cuant_y)
                ggplot(data, aes(x = !!x, y = !!y, color = !!t)) +
                  geom_point(size = 3) +
                  labs(title = paste(var_cuant_x, "vs", var_cuant_y, "por", var_cual_x), x = var_cuant_y, y = var_cuant_x, color = var_cual_x) +
                  theme_minimal() +
                  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
              } else {
                ggplot(data, aes(x = !!x, y = !!y, color = !!t)) +
                  geom_point(size = 3) +
                  labs(title = label_titulo, x = label_x, y = label_y, color = var_cual_x) +
                  theme_minimal() +
                  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
              },
              "burbujas" = ggplot(data, aes(x = !!t, y = !!y, size = !!y, color = !!t)) +
                geom_point(alpha = 0.5) +
                labs(title = paste("Burbujas de", label_y, "por", label_x), x = label_x, y = label_y),
              "torta" = {
                data_pie <- data %>% count(!!t) %>% mutate(percentage = n / sum(n))
                ggplot(data_pie, aes(x = "", y = percentage, fill = !!t)) +
                  geom_bar(stat = "identity", width = 1) +
                  coord_polar("y") +
                  geom_text(aes(label = scales::percent(percentage)), position = position_stack(vjust = 0.5)) +
                  labs(title = paste("Gráfico de Torta de", label_x), x = NULL, y = NULL) +
                  theme_void()
              },
              stop(paste(tipo_grafico, " Tipo de gráfico no soportado."))
  )
  
  # Aplicar tema minimalista y formato de etiquetas en eje y
  p <- p + theme_minimal() +
    scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
  # Mostrar el gráfico
  print(p)
}




# Definir la función para crear el mapa de calor de correlaciones
mapa_calor <- function(data) {
  
  # Seleccionar solo variables numéricas
  numeric_vars <- data %>% select_if(is.numeric)
  
  # Calcular matriz de correlación
  cor_matrix <- cor(numeric_vars, use = "complete.obs")
  
  # Derretir la matriz de correlación
  melted_cor_matrix <- melt(cor_matrix)
  
  # Obtener la paleta de colores
  colores <- paleta_colores()
  
  # Crear el mapa de calor
  p <- ggplot(melted_cor_matrix, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradientn(colors = colores, limits = c(-1, 1)) +
    labs(title = "Mapa de Calor de Correlaciones",
         x = "Variables",
         y = "Variables") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Mostrar el gráfico
  print(p)
}

