
# Instalar y cargar las librerías necesarias
required_packages <- c("ggplot2", "dplyr", "paletteer",
                       "ggrepel","RColorBrewer","ggcorrplot")

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


reemplazar_por_mediana <- function(x) {
  if (is.numeric(x) && any(is.na(x))) {
    x[is.na(x)] <- median(x, na.rm = TRUE)
  }
  return(x)
}



reemplazar_nulos <- function(x) {
  ifelse(x %in% c( "", "NULL", "Null", "null"), NA, x)
}



paleta_colores <- function() {
  colores <- paletteer::paletteer_d("ggthemes::Classic_10_Light")
  return(colores)
}







grafico_torta <- function(data, var_cual_x) {
  data[[var_cual_x]] <- as.factor(data[[var_cual_x]])
  num_categorias <- n_distinct(data[[var_cual_x]])
  if (num_categorias > 10) {
    colores <- colorRampPalette(brewer.pal(9, "Pastel1"))(num_categorias)
  } else {
    colores <- paleta_colores()
  }
  
  p <- ggplot(data, aes(x = "", fill = !!sym(var_cual_x))) +
    geom_bar(width = 1, stat = "count") +
    coord_polar("y") +
    geom_text(aes(label = scales::percent(after_stat(count / sum(count)), accuracy = 0.1)),
              stat = "count", position = position_stack(vjust = 0.5), color = "black") +
    labs(title = paste("Proporción de categorías de", var_cual_x), x = NULL, y = NULL)+ 
         #caption = paste("Este gráfico de torta muestra la proporción de cada categoría de", var_cual_x, ".")) +
    scale_fill_manual(values = colores) +
    theme_void() +
    theme(plot.title = element_text(size = 10))+
    theme(legend.background = element_blank())
  return(p)
}



grafico_anillo <- function(data, var_cual_x) {
  data[[var_cual_x]] <- as.factor(data[[var_cual_x]])
  num_categorias <- n_distinct(data[[var_cual_x]])
  if (num_categorias > 10) {
    colores <- colorRampPalette(brewer.pal(9, "Pastel1"))(num_categorias)
  } else {
    colores <- paleta_colores()
  }
  
  p <- ggplot(data, aes(x = 2, fill = !!sym(var_cual_x))) +
    geom_bar(width = 1, stat = "count") +
    coord_polar(theta = "y") +
    xlim(0.5, 2.5) +
    geom_text(aes(label = scales::percent(after_stat(count / sum(count)), accuracy = 0.1)),
              stat = "count", position = position_stack(vjust = 0.5), color = "black") +
    labs(title = paste("Proporción de categorías de", var_cual_x), x = NULL, y = NULL)+
    #caption = paste("Este gráfico de anillo muestra la proporción de cada categoría de", var_cual_x, ".")) +
    scale_fill_manual(values = colores) +
    theme_void() +
    theme(plot.title = element_text(size = 10))+
    theme(legend.position = "right", legend.background = element_blank())
  return(p)
  }




grafico_histograma <- function(data, var_cuant_x) {
  if (!is.numeric(data[[var_cuant_x]])) {
    data[[var_cuant_x]] <- as.factor(data[[var_cuant_x]])
  }
  colores <- paleta_colores()
  
  if (is.numeric(data[[var_cuant_x]])) {
    p <- ggplot(data, aes(x = !!sym(var_cuant_x))) +
      geom_histogram(binwidth = 1, fill = colores[1], color = "black") +
      labs(title = paste("Histograma de", var_cuant_x), x = var_cuant_x, y = "Frecuencia")+ 
           #caption = paste("Este histograma muestra la distribución de", var_cuant_x, ".")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(plot.title = element_text(size = 10))
  } else if (is.factor(data[[var_cuant_x]])) {
    p <- ggplot(data, aes(x = !!sym(var_cuant_x))) +
      geom_bar(fill = colores[1], color = "black") +
      labs(title = paste("Histograma de", var_cuant_x), x = var_cuant_x, y = "Frecuencia")+ 
           #caption = paste("Este histograma muestra la distribución de", var_cuant_x, ".")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      guides(fill = "none")+ 
      theme(plot.title = element_text(size = 10))
    
  }
  return(p)
}



  
  grafico_barras <- function(data, var_cual_x) {
    num_categorias <- n_distinct(data[[var_cual_x]])
    if (num_categorias > 10) {
      colores <- colorRampPalette(brewer.pal(12, "Set3"))(num_categorias)
    } else {
      colores <- paleta_colores()
    }
    
    p <- ggplot(data, aes(x = !!sym(var_cual_x))) +
      geom_bar(fill = colores) +
      geom_text(stat = 'count', aes(label = scales::number(after_stat(count), accuracy = 1, big.mark = ".", decimal.mark = ",")), angle = 90, vjust = 0.5,hjust = 0.8) +
      labs(title = paste("Frecuencia de", var_cual_x), x = var_cual_x, y = "Frecuencia")+ 
           #caption = paste("Este gráfico de barras muestra la frecuencia de", var_cual_x, ".")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust =0.5)) +
      theme(plot.title = element_text(size = 10))+
      scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) 

    return(p)
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
      labs(title = paste("Dispersión de", var_x, "vs", var_y),
           x = var_x,
           y = var_y,
           caption = paste("Este gráfico de dispersión muestra la relación entre", var_x, "y", var_y, "\nLa relaciòn es:", evaluacion)) +
      theme(plot.title = element_text(size = 10)) +  
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
    return(p)
  }
  
  
  grafico_boxplot <- function(data, var_cuant_x, titulo) {
    colores <- paleta_colores()
    
    x <- sym(var_cuant_x)
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
    
    p <- ggplot(data, aes(x = factor(1), y = !!x)) +
      geom_boxplot(alpha = 0.5, outlier.shape = NA, fill = colores[3]) + 
      geom_jitter(shape = 10, position = position_jitter(0.15), size = 1, alpha = 0.7, color = colores[6]) +
      labs(title = titulo, x = NULL, y = NULL)+ 
      #caption = paste("Este boxplot con puntos muestra la distribución de", var_cuant_x, ".")) +
      theme_minimal() +
      theme(plot.title = element_text(size = 10))+
      guides(fill = "none") +
      theme(
        legend.position = "none", 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) + 
      annotate("label", x = 1.3, y = stats$Min, label = paste("Min:", format(round(stats$Min, 0), big.mark = ".", decimal.mark = ",", nsmall = 0)), hjust = 0.5, size = 3, fill = "grey90") +
      annotate("label", x = 1.3, y = stats$Q1, label = paste("Q1:", format(round(stats$Q1, 0), big.mark = ".", decimal.mark = ",", nsmall = 0)), hjust = 0.5, size = 3, fill = "grey90") +
      annotate("label", x = 1.3, y = stats$Mediana, label = paste("Mediana:", format(round(stats$Mediana, 0), big.mark = ".", decimal.mark = ",", nsmall = 0)), hjust = 0.5, size = 3, fill = "grey90") +
      annotate("label", x = 1.3, y = stats$Q3, label = paste("Q3:", format(round(stats$Q3, 0), big.mark = ".", decimal.mark = ",", nsmall = 0)), hjust = 0.5, size = 3, fill = "grey90") +
      annotate("label", x = 1.3, y = bigote_superior, label = paste("Bigote Sup.:", format(round(bigote_superior, 0), big.mark = ".", decimal.mark = ",", nsmall = 0)), hjust = 0.5, size = 3, fill = "grey90") +
      annotate("label", x = 1.3, y = bigote_inferior, label = paste("Bigote Inf.:", format(round(bigote_inferior, 0), big.mark = ".", decimal.mark = ",", nsmall = 0)), hjust = 0.5, size = 3, fill = "grey90") +
      annotate("label", x = 1.3, y = stats$Max, label = paste("Max:", format(round(stats$Max, 0), big.mark = ".", decimal.mark = ",", nsmall = 0)), hjust = 0.5, size = 3, fill = "grey90")
    
    return(p)
  }
  
  
  
  grafico_area <- function(data, var_x, var_y, var_fill) {
    if (!is.factor(data[[var_fill]])) {
      data[[var_fill]] <- as.factor(data[[var_fill]])
    }
    
    num_categorias <- n_distinct(data[[var_fill]])
    
    if (num_categorias > 10) {
      colores <- colorRampPalette(brewer.pal(12, "Set3"))(num_categorias)
    } else {
      colores <- paleta_colores()
    }
    
    p <- ggplot(data, aes(x = !!sym(var_x), y = !!sym(var_y), fill = !!sym(var_fill), group = !!sym(var_fill))) +
      geom_area(alpha = 0.5, color = "grey", position = "identity") +
      labs(title = paste("Distribución por", var_x, "y", var_fill), 
           x = var_x, y = var_y, fill = var_fill)+ 
           #caption = paste("Este gráfico de área muestra la distribución de pagos por", var_x, "y", var_fill, ".")) +
      scale_fill_manual(values = colores) +
      theme_minimal() +
      theme(plot.title = element_text(size = 10))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
     
    
    return(p)
  }
  
  grafico_matriz_correlacion <- function(data) {
    numerical_vars <- data %>%
      select_if(is.numeric)
    
    cor_matrix <- cor(numerical_vars, use = "complete.obs")
    p <- ggcorrplot(cor_matrix,
                    title="Matriz de Correlación",
                    method = "circle", 
                    type = "lower", 
                    lab = TRUE,
                    lab_col = "black", 
                    lab_size = 2.5,
                    tl.col = "black", 
                    tl.srt = 45,
                    ggtheme = theme_minimal())
    return(p)
  }
  

  
  
mostrar_graficos <- function(..., ncol = 2) {
  # Recoger los gráficos pasados como argumentos
  graficos <- list(...)
  
  # Asegurarse de que no haya más de 4 gráficos
  if (length(graficos) > 4) {
    stop("Solo se pueden mostrar hasta 4 gráficos a la vez.")
  }
  
  # Mostrar los gráficos utilizando grid.arrange
  do.call(grid.arrange, c(graficos, ncol = ncol))
}




# Función para el boxplot con dos variables
grafico_boxplot_dos_var <- function(data, var_x, var_y) {
  colores <- paleta_colores()
  
  p <- ggplot(data, aes(x = !!sym(var_x), y = !!sym(var_y), fill = !!sym(var_x))) +
    geom_boxplot(alpha = 0.5, outlier.shape = NULL, fill = colores[3]) + 
    labs(title = paste("Distribución de", var_y, "por", var_x), x = var_x, y = var_y) + 
    theme_minimal() +
    guides(fill = "none") +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      plot.title = element_text(size = 10)
    ) +
    scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ","))
  
  return(p)
}
