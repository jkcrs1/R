# Instalar y cargar las librerías necesarias
required_packages <- c("ggplot2", "gridExtra", "dplyr", "stringr", "paletteer", "scales","lubridate","prophet","reshape2","e1071","tibble")
installed_packages <- rownames(installed.packages())


for (pkg in required_packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
}

lapply(required_packages, library, character.only = TRUE)

# Función para determinar el tamaño de la muestra
tam.muestra <- function(alfa, epsilon, s, N = Inf) {
  za2 <- qnorm(1 - alfa / 2)
  n <- if (N == Inf) (s * za2 / epsilon)^2 else N * ((za2 * s)^2) / ((N - 1) * epsilon^2 + (za2 * s)^2)
  return(ceiling(n))
}

# Función para crear una paleta de colores dinámica
paleta_colores <- function(data, var_cual) {
  n <- length(unique(data[[var_cual]]))
  colores <- colorRampPalette(paletteer::paletteer_d("cartography::pastel.pal", n))
  return(colores(n))
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


