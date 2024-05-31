# Instalar y cargar las librerías necesarias
required_packages <- c("ggplot2", "gridExtra", "dplyr", "stringr", "paletteer", "scales")
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
crear_paleta_colores <- function(data, var_cual) {
  n <- length(unique(data[[var_cual]]))
  colores <- colorRampPalette(paletteer::paletteer_d("cartography::pastel.pal", n))
  return(colores(n))
}

# Función para crear gráficos que comparan variables cuantitativas y cualitativas
crear_grafico_comparado_cuali_cuanti <- function(data, var_cuant, var_cual, tipo_grafico = NULL) {
  colores <- crear_paleta_colores(data, var_cual)
  
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
crear_graficos_multiple <- function(data, graficos) {
  plots <- lapply(graficos, function(grafico) {
    crear_grafico_comparado_cuali_cuanti(data, grafico$var_cuant, grafico$var_cual, grafico$tipo_grafico)
  })
  do.call("grid.arrange", c(plots, ncol = 2))
}

# Función para crear tablas con valores estadísticos
crear_tabla_estadistica <- function(data, var_cuant, var_cual) {
  data %>%
    group_by(!!sym(var_cual)) %>%
    summarise(
      cantidad_registros = n(),
      media = mean(!!sym(var_cuant), na.rm = TRUE),
      mediana = median(!!sym(var_cuant), na.rm = TRUE),
      desviacion_estandar = sd(!!sym(var_cuant), na.rm = TRUE),
      varianza = var(!!sym(var_cuant), na.rm = TRUE),
      minimo = min(!!sym(var_cuant), na.rm = TRUE),
      maximo = max(!!sym(var_cuant), na.rm = TRUE),
      q1 = quantile(!!sym(var_cuant), 0.25, na.rm = TRUE),
      q3 = quantile(!!sym(var_cuant), 0.75, na.rm = TRUE),
      rango_intercuartil = IQR(!!sym(var_cuant), na.rm = TRUE),
      coeficiente_variacion = sd(!!sym(var_cuant), na.rm = TRUE) / mean(!!sym(var_cuant), na.rm = TRUE)
    ) %>%
    ungroup()
}




