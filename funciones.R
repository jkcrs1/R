# Instalar las librerías necesarias si no están instaladas
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
if (!require(gridExtra)) {
  install.packages("gridExtra")
}
if (!require(dplyr)) {
  install.packages("dplyr")
}
if (!require(stringr)) {
  install.packages("stringr")
}
if (!require(paletteer)) {
  install.packages("paletteer")
}
if (!require(scales)) {
  install.packages("scales")
}

# Cargar librerías necesarias
library(ggplot2)
library(gridExtra)
library(dplyr)
library(stringr)
library(paletteer)
library(scales)



# Función para determinar el tañano de la muestra
tam.muestra=function(alfa,epsilon,s,N=Inf)
{
  za2=qnorm(1-alfa/2)
  if (N==Inf) n=(s*za2/epsilon)^2
  else n=N*((za2*s)^2)/((N-1)*epsilon^2+(za2*s)^2)
  return(ceiling(n))
}


# Función para crear una paleta de colores dinámica
crear_paleta_colores <- function(data, var_cual) {
  n <- length(unique(data[[var_cual]]))
  colores <- colorRampPalette(paletteer_dynamic("cartography::pastel.pal", 20))(n)
  return(colores)
}

# Función para crear gráficos que comparan variables cuantitativas y cualitativas
crear_grafico_comparado_cuali_cuanti <- function(data, var_cuant, var_cual, tipo_grafico = NULL) {
  colores <- crear_paleta_colores(data, var_cual)
  
  p <- NULL
  if (tipo_grafico == "caja") {
    p <- ggplot(data, aes_string(x = var_cual, y = var_cuant)) +
      geom_boxplot(aes(fill = !!sym(var_cual))) +
      scale_fill_manual(values = colores) +
      labs(title = paste("Boxplot de", var_cuant, "por", var_cual),
           x = var_cual, y = var_cuant) +
      scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } 
  else if (tipo_grafico == "histograma") {
    p <- ggplot(data, aes_string(x = var_cuant, fill = var_cual)) +
      geom_histogram(position = "dodge", binwidth = 10) +
      scale_fill_manual(values = colores) +
      labs(title = paste("Histograma de", var_cuant, "por", var_cual),
           x = var_cuant, y = "Frecuencia") +
      scale_x_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } 
  else if (tipo_grafico == "barra") {
    p <- ggplot(data, aes_string(x = var_cual, fill = var_cual)) +
      geom_bar() +
      geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
      scale_fill_manual(values = colores) +
      labs(title = paste("Gráfico de Barras de", var_cual),
           x = var_cual, y = "Frecuencia") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } 
  else if (tipo_grafico == "dispercion") {
    p <- ggplot(data, aes_string(x = var_cual, y = var_cuant)) +
      geom_point(aes(color = !!sym(var_cual))) +
      scale_color_manual(values = colores) +
      labs(title = paste("Gráfico de Dispersión de", var_cuant, "por", var_cual),
           x = var_cual, y = var_cuant) +
      scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } 
  else if (tipo_grafico == "densidad") {
    p <- ggplot(data, aes_string(x = var_cuant, fill = var_cual)) +
      geom_density(alpha = 0.5) +
      scale_fill_manual(values = colores) +
      labs(title = paste("Gráfico de Densidad de", var_cuant, "por", var_cual),
           x = var_cuant, y = "Densidad") +
      scale_x_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } 
  else if (tipo_grafico == "violin") {
    p <- ggplot(data, aes_string(x = var_cual, y = var_cuant)) +
      geom_violin(aes(fill = !!sym(var_cual))) +
      scale_fill_manual(values = colores) +
      labs(title = paste("Gráfico de Violín de", var_cuant, "por", var_cual),
           x = var_cual, y = var_cuant) +
      scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } 
  else if (tipo_grafico == "lineas") {
    p <- ggplot(data, aes_string(x = var_cual, y = var_cuant, group = var_cual)) +
      geom_line(aes(color = !!sym(var_cual))) +
      scale_color_manual(values = colores) +
      labs(title = paste("Gráfico de Líneas de", var_cuant, "por", var_cual),
           x = var_cual, y = var_cuant) +
      scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } 
  else if (tipo_grafico == "puntos") {
    p <- ggplot(data, aes_string(x = var_cual, y = var_cuant)) +
      geom_jitter(aes(color = !!sym(var_cual))) +
      scale_color_manual(values = colores) +
      labs(title = paste("Gráfico de Puntos (Jitter) de", var_cuant, "por", var_cual),
           x = var_cual, y = var_cuant) +
      scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } 
  else if (tipo_grafico == "area") {
    p <- ggplot(data, aes_string(x = var_cual, y = var_cuant, group = var_cual)) +
      geom_area(aes(fill = !!sym(var_cual))) +
      scale_fill_manual(values = colores) +
      labs(title = paste("Gráfico de Área de", var_cuant, "por", var_cual),
           x = var_cual, y = var_cuant) +
      scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } 
  else if (tipo_grafico == "burbujas") {
    p <- ggplot(data, aes_string(x = var_cual, y = var_cuant, size = var_cuant)) +
      geom_point(aes(color = !!sym(var_cual)), alpha = 0.5) +
      scale_color_manual(values = colores) +
      labs(title = paste("Gráfico de Burbujas de", var_cuant, "por", var_cual),
           x = var_cual, y = var_cuant, size = var_cuant) +
      scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } 
  else if (tipo_grafico == "area ampliada") {
    p <- ggplot(data, aes_string(x = var_cual, y = var_cuant, fill = var_cual)) +
      geom_area(position = 'stack') +
      scale_fill_manual(values = colores) +
      labs(title = paste("Gráfico de Área Apilada de", var_cuant, "por", var_cual),
           x = var_cual, y = var_cuant) +
      scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
              
  } 
  else if (tipo_grafico == "barras ampliadas") {
    p <- ggplot(data, aes_string(x = var_cual, fill = var_cuant)) +
      geom_bar(position = 'fill') +
      scale_fill_manual(values = colores) +
      labs(title = paste("Gráfico de Barras Apiladas de", var_cual, "por", var_cuant),
           x = var_cual, y = "Proporción") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } 
  else if (tipo_grafico == "torta") {
    # Calcular los porcentajes
    data_pie <- data %>%
      count(!!sym(var_cual)) %>%
      mutate(percentage = n / sum(n))
    
    p <- ggplot(data_pie, aes(x = "", y = percentage, fill = !!sym(var_cual))) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      scale_fill_manual(values = colores) +
      geom_text(aes(label = scales::percent(percentage)), position = position_stack(vjust = 0.5)) +
      labs(title = paste("Gráfico de Torta de", var_cual),
           x = NULL, y = NULL) +
      theme_void()
  } else {
    stop("Tipo de gráfico no soportado. Use 'caja', 'histograma', 'barra', 'dispercion', 'densidad', 'violin', 'lineas', 'puntos', 'area', 'burbujas', 'area ampliada', 'densidad 2d', 'barras ampliadas' o 'torta'.")
  }
  print(p)
}                                         
                                            
                                            
                                            
                                            
                                            

# Función para crear múltiples gráficos simultáneamente
crear_graficos_multiple <- function(data, graficos) {
  plots <- list()
  
  for (i in seq_along(graficos)) {
    grafico <- graficos[[i]]
    p <- crear_grafico_comparado_cuali_cuanti(data, grafico$var_cuant, grafico$var_cual, grafico$tipo_grafico)
    plots[[i]] <- p
  }
  
  do.call("grid.arrange", c(plots, ncol = 2))
}


# Función para crear tablas con valores estadísticos
crear_tabla_estadistica <- function(data, var_cuant, var_cual) {
  tabla <- data %>%
    group_by_at(var_cual) %>%
    summarise(
      cantidad_registros = n(),
      media = mean(get(var_cuant), na.rm = TRUE),
      mediana = median(get(var_cuant), na.rm = TRUE),
      desviacion_estandar = sd(get(var_cuant), na.rm = TRUE),
      varianza = var(get(var_cuant), na.rm = TRUE),
      minimo = min(get(var_cuant), na.rm = TRUE),
      maximo = max(get(var_cuant), na.rm = TRUE),
      q1 = quantile(get(var_cuant), 0.25, na.rm = TRUE),
      q3 = quantile(get(var_cuant), 0.75, na.rm = TRUE),
      rango_intercuartil = IQR(get(var_cuant), na.rm = TRUE),
      coeficiente_variacion = sd(get(var_cuant), na.rm = TRUE) / mean(get(var_cuant), na.rm = TRUE)
    ) %>%
    ungroup()
  return(tabla)
}




