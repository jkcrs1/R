# Instalar y cargar las librerías necesarias
required_packages <- c("ggplot2", "gridExtra", "dplyr", "stringr", "paletteer", "scales","lubridate","prophet","reshape2")
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

# Función para crear gráficos que comparan variables cuantitativas y cualitativas
grafico_comparado <- function(data, var_cuant, var_cual, tipo_grafico = NULL) {
  colores <- paleta_colores(data, var_cual)
  
  p <- ggplot(data, aes(x = !!sym(var_cual), y = !!sym(var_cuant), fill = !!sym(var_cual))) +
    scale_fill_manual(values = colores) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = paste(tipo_grafico, "de", var_cuant, "por", var_cual), x = var_cual, y = var_cuant) +
    scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ","))
  
  p <- switch(tipo_grafico,
              "caja" = p + geom_boxplot(),
              "histograma" = p + geom_histogram(position = "dodge", binwidth = 10),
              "barra" = p + geom_bar() + geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5),
              "dispercion" = p + geom_point(aes(color = !!sym(var_cual))),
              "densidad" = p + geom_density(alpha = 0.5),
              "violin" = p + geom_violin(),
              "lineas" = p + geom_line(aes(group = !!sym(var_cual), color = !!sym(var_cual))),
              "puntos" = p + geom_jitter(aes(color = !!sym(var_cual))),
              "area" = p + geom_area(position = 'stack'),
              "burbujas" = p + geom_point(aes(size = !!sym(var_cuant)), alpha = 0.5),
              "area ampliada" = p + geom_area(position = 'stack'),
              "barras ampliadas" = p + geom_bar(position = 'fill'),
              "torta" = {
                data_pie <- data %>% count(!!sym(var_cual)) %>% mutate(percentage = n / sum(n))
                ggplot(data_pie, aes(x = "", y = percentage, fill = !!sym(var_cual))) +
                  geom_bar(stat = "identity", width = 1) +
                  coord_polar("y") +
                  scale_fill_manual(values = colores) +
                  geom_text(aes(label = scales::percent(percentage)), position = position_stack(vjust = 0.5)) +
                  labs(title = paste("Gráfico de Torta de", var_cual), x = NULL, y = NULL) +
                  theme_void()
              },
              stop("Tipo de gráfico no soportado.")
  )
  print(p)
}

# Instalar y cargar las librerías necesarias
required_packages <- c("ggplot2", "gridExtra", "dplyr", "stringr", "paletteer", "scales","lubridate","prophet","reshape2")
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

# Función para crear gráficos que comparan variables cuantitativas y cualitativas
grafico_comparado <- function(data, var_cuant, var_cual, tipo_grafico = NULL) {
  colores <- paleta_colores(data, var_cual)
  
  p <- ggplot(data, aes(x = !!sym(var_cual), y = !!sym(var_cuant), fill = !!sym(var_cual))) +
    scale_fill_manual(values = colores) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = paste(tipo_grafico, "de", var_cuant, "por", var_cual), x = var_cual, y = var_cuant) +
    scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ","))
  
  p <- switch(tipo_grafico,
              "caja" = p + geom_boxplot(),
              "histograma" = p + geom_histogram(position = "dodge", binwidth = 10),
              "barra" = p + geom_bar() + geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5),
              "dispercion" = p + geom_point(aes(color = !!sym(var_cual))),
              "densidad" = p + geom_density(alpha = 0.5),
              "violin" = p + geom_violin(),
              "lineas" = p + geom_line(aes(group = !!sym(var_cual), color = !!sym(var_cual))),
              "puntos" = p + geom_jitter(aes(color = !!sym(var_cual))),
              "area" = p + geom_area(position = 'stack'),
              "burbujas" = p + geom_point(aes(size = !!sym(var_cuant)), alpha = 0.5),
              "area ampliada" = p + geom_area(position = 'stack'),
              "barras ampliadas" = p + geom_bar(position = 'fill'),
              "torta" = {
                data_pie <- data %>% count(!!sym(var_cual)) %>% mutate(percentage = n / sum(n))
                ggplot(data_pie, aes(x = "", y = percentage, fill = !!sym(var_cual))) +
                  geom_bar(stat = "identity", width = 1) +
                  coord_polar("y") +
                  scale_fill_manual(values = colores) +
                  geom_text(aes(label = scales::percent(percentage)), position = position_stack(vjust = 0.5)) +
                  labs(title = paste("Gráfico de Torta de", var_cual), x = NULL, y = NULL) +
                  theme_void()
              },
              stop("Tipo de gráfico no soportado.")
  )
  print(p)
}

# Función para crear múltiples gráficos simultáneamente
graficos_multiple <- function(data, graficos) {
  plots <- lapply(graficos, function(grafico) {
    grafico_comparado(data, grafico$var_cuant, grafico$var_cual, grafico$tipo_grafico)
  })
  do.call("grid.arrange", c(plots, ncol = 2))
}

# Función para crear tablas con valores estadísticos y sus explicaciones
tabla_estadistica <- function(data, var_cuant, var_cual) {
  var_cuant_sym <- sym(var_cuant)
  calculos <- data %>%
    group_by(!!sym(var_cual)) %>%
    summarise(
      cantidad_registros = n(),
      media = mean(!!var_cuant_sym, na.rm = TRUE),
      mediana = median(!!var_cuant_sym, na.rm = TRUE),
      desviacion_estandar = sd(!!var_cuant_sym, na.rm = TRUE),
      varianza = var(!!var_cuant_sym, na.rm = TRUE),
      minimo = min(!!var_cuant_sym, na.rm = TRUE),
      maximo = max(!!var_cuant_sym, na.rm = TRUE),
      q1 = quantile(!!var_cuant_sym, 0.25, na.rm = TRUE),
      q3 = quantile(!!var_cuant_sym, 0.75, na.rm = TRUE),
      rango_intercuartil = IQR(!!var_cuant_sym, na.rm = TRUE),
      coeficiente_variacion = sd(!!var_cuant_sym, na.rm = TRUE) / mean(!!var_cuant_sym, na.rm = TRUE)
    ) %>%
    ungroup()
  
  explicaciones <- tibble::tibble(
    Variable = c("cantidad_registros", "media", "mediana", "desviacion_estandar", "varianza", "minimo", "maximo", "q1", "q3", "rango_intercuartil", "coeficiente_variacion"),
    Explicacion = c(
      "Número de registros en el grupo.",
      paste("Media de", var_cuant, "."),
      paste("Mediana de", var_cuant, "."),
      paste("Desviación estándar de", var_cuant, "."),
      paste("Varianza de", var_cuant, "."),
      paste("Valor mínimo de", var_cuant, "."),
      paste("Valor máximo de", var_cuant, "."),
      paste("Primer cuartil (25%) de", var_cuant, "."),
      paste("Tercer cuartil (75%) de", var_cuant, "."),
      paste("Rango intercuartil (Q3 - Q1) de", var_cuant, "."),
      paste("Coeficiente de variación (desviación estándar / media) de", var_cuant, ".")
    )
  )
  
  list(Calculos = calculos, Explicaciones = explicaciones)
}



