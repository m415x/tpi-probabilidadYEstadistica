# ==============================================================================
# TRABAJO PRÁCTICO INTEGRADOR - CONSIGNAS 1 A 4
# Estadística Descriptiva
# Tecnicatura Universitaria en Programación a Distancia
# ==============================================================================

# Limpieza del espacio de trabajo
# ───────────────────────────────────
# 1. Borrar todos los objetos del Environment
rm(list = ls()) 

# 2. Cerrar todos los gráficos y dispositivos abiertos
graphics.off() 

# 3. Limpiar la Consola
cat("\014") 

# 4. Liberar memoria del sistema
gc()

# Instalar y cargar librerías
# ───────────────────────────────────
if (!require(readxl)) install.packages("readxl")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(scales)) install.packages("scales")
library(readxl)
library(ggplot2)
library(scales)


# ==============================================================================
# FUNCIONES AUXILIARES
# ==============================================================================


# ──────────────────────────────────────────────────────────────────────────────
# Calcular frecuencias (Retorna una lista nombrada)
# ──────────────────────────────────────────────────────────────────────────────
calculate_frequencies <- function(vector) {
  # table(): Toma un vector y cuenta cuántas veces aparece cada categoría
  fi <- table(vector)               # Frecuencia absoluta
  # cumsum(): Calcula la suma acumulativa de un vector
  Fi <- cumsum(fi)                  # Frecuencia acumulada
  # prop.table(): Toma una tabla de frecuencias y calcula proporciones
  fr <- prop.table(fi)              # Frecuencia relativa
  Fr <- cumsum(fr)                  # Frecuencia relativa acumulada
  
  # Devolver lista con nombres para facilitar el acceso
  return(list(fi=fi, Fi=Fi, fr=fr, Fr=Fr))
}


# ──────────────────────────────────────────────────────────────────────────────
# Crear dataframe de frecuencias para variable continua o discreta
# ──────────────────────────────────────────────────────────────────────────────
create_df <- function(freq_list, rnd = 4, class_intervals = NULL, breaks = NULL) {
    
  # ───────────────────────────────────
  # Construimos los vectores a partir de la lista de frecuencias
  # as.vector(): Convierte una tabla en un vector numérico.
  frec_abs <- as.vector(freq_list$fi)
  frec_acum <- as.vector(freq_list$Fi)
  # round(..., n): Redondea a n decimales
  frec_rel <- round(as.vector(freq_list$fr), rnd)
  frec_rel_acum <- round(as.vector(freq_list$Fr), rnd)
  
  # ───────────────────────────────────
  # Construimos el dataframe a partir de los vectores
  # data.frame(): Crea una tabla de datos
  if (!is.null(class_intervals) && !is.null(breaks)) {
    tabla_frec <- data.frame(
        # levels(): Extrae los nombres de los intervalos
        Intervalo = levels(class_intervals),
        # Calcula marcas de clase a partir de los cortes
        Marca_Clase = round(breaks, rnd),
        Frec_Abs = frec_abs,
        Frec_Acum = frec_acum,
        Frec_Rel = frec_rel,
        Frec_Rel_Acum = frec_rel_acum
    )
  } else {
    tabla_frec <- data.frame(
        Categoría = names(freq_list$fi),
        Frec_Abs = frec_abs,
        Frec_Acum = frec_acum,
        Frec_Rel = frec_rel,
        Frec_Rel_Acum = frec_rel_acum
    )
  }
  
  return(tabla_frec)
}


# ──────────────────────────────────────────────────────────────────────────────
# Cortes con el método de Sturges
# ──────────────────────────────────────────────────────────────────────────────
sturges_breaks <- function(vector) {
  # vector: vector numérico
  
  # ───────────────────────────────────
  # Calcular número de clases con Sturges
  # ceiling(): Redondea al alza al entero más cercano
  k <- ceiling(
    1 + 3.322 * log10(
      # length(): Obtiene el número de elementos de un objeto
      length(vector)
    )
  )
  
  # ───────────────────────────────────
  # Calcular rango
  # range(): Devuelve un vector con el número mínimo y máximo
  rango <- range(vector)
  
  # ───────────────────────────────────
  # Calcular amplitud
  amplitud <- ceiling((rango[2] - rango[1]) / k)
  
  # ───────────────────────────────────
  # Construir puntos de cortes
  # seq(): Genera secuencias de números con una progresión aritmética
  breaks <- seq(
    # Redondea hacia abajo el minimo
    # floor(): Redondea a la baja al entero más cercano
    from = floor(rango[1]),
    # Redondea hacia arriba el máximo y asegura ultimo valor
    to = ceiling(rango[2]) + amplitud,
    # Define ancho de clase
    by = amplitud,
    # Esto asegura que siempre haya k intervalos, 
    # aunque el último puede tener frcuencia absoluta igual a cero
    # length.out = k + 1
  )
  return(list(breaks = breaks, amplitude = amplitud))
}


# ──────────────────────────────────────────────────────────────────────────────
# Marcas de clase
# ──────────────────────────────────────────────────────────────────────────────

# ───────────────────────────────────
# VERSIÓN 1: Basada en cálculos numéricos (Vectorial)
# Se utiliza cuando se tiene el objeto 'breaks' generado por Sturges.
class_marks_with_breaks <- function(breaks) {
  # Toma los puntos de corte y promedia los vecinos
  xi <- (head(breaks, -1) + tail(breaks, -1)) / 2
  return(xi)
}

# ───────────────────────────────────
# VERSIÓN 2: Basada en etiquetas de texto (Parsing)
# Se utiliza cuando solo se tiene el factor 'clases' y no los cortes originales.
class_marks_with_intervals <- function(intervals) {
  # intervals: vector de intervalos tipo factor generado por cut()
  
  # Extraer nombres de intervalos
  # levels(): Extrae los nombres de los intervalos
  intervalos <- levels(intervals)
  
  # Limpiar símbolos
  # gsub(): Busca y reemplaza todas las ocurrencias de un patrón específico 
  # dentro de una o más cadenas de texto
  ext <- gsub("\\[|\\)|\\]", "", intervalos)
  
  # Separar por coma
  # strsplit(): Divide un vector de caracteres en subcadenas
  ext <- strsplit(ext, ",")
  
  # Promediar los extremos
  # sapply(): Aplica una función a cada elemento de un vector
  xi <- sapply(
    ext,
    # function(x): Define una función anónima que toma un argumento x
    # as.numeric(): Convierte en un vector numérico
    # mean(): Calcula la media aritmética
    function(v) mean(as.numeric(v))
  ) 
  return(xi)
}


# ──────────────────────────────────────────────────────────────────────────────
# Medidas de tendencia central para datos agrupados o discretos
# ──────────────────────────────────────────────────────────────────────────────
calculate_central_measures <- function(vector, freq_list, rnd = 4, class_marks = NULL, breaks = NULL, amplitude = NULL) {
  if (!is.null(class_marks) && !is.null(breaks) && !is.null(amplitude)) {
    frecuencias <- as.vector(freq_list$fi) 
  
    # ───────────────────────────────────
    # Media
    media <- sum(class_marks * frecuencias) / sum(frecuencias)
    
    # ───────────────────────────────────
    # Moda
    i_modal_class <- which.max(frecuencias) # Índice de la clase modal
    L_m <- breaks[i_modal_class]            # Límite inferior de la clase modal
    f0 <- ifelse(
        i_modal_class == 1, 0, frecuencias[i_modal_class - 1]
    )                                       # Frec del intervalo anterior
    f1 <- frecuencias[i_modal_class]        # Frec del intervalo modal
    f2 <- ifelse(
        i_modal_class == length(frecuencias), 0, frecuencias[i_modal_class + 1]
    )                                       # Frec del intervalo posterior
    
    moda <- L_m + ((f1 - f0) / ((f1 - f0) + (f1 - f2))) * amplitude
    
    # ───────────────────────────────────
    # Mediana
    mediana <- calculate_grouped_quantiles(0.50, freq_list$Fi, frecuencias, breaks, amplitude)

  } else {
    media <- mean(vector, na.rm = TRUE)
    mediana <- median(vector, na.rm = TRUE)
    frec <- table(vector)
    # paste(): Concatena elementos de texto con un separador
    moda <- paste(as.numeric(names(frec[frec == max(frec)])), collapse = ", ")
  }

  # Data Frame 
  return(
    central_measures <- data.frame(
        Media = round(media, rnd),
        Mediana = round(mediana, rnd),
        Moda = round(moda, rnd),
        Mínimo = min(vector),
        Máximo = max(vector)
    )
  )
}


# ──────────────────────────────────────────────────────────────────────────────
# Cuantiles agrupados
# ──────────────────────────────────────────────────────────────────────────────
calculate_grouped_quantiles <- function(p, Fi, frequencies, breaks, amplitude) {
  n_total <- sum(frequencies)
  n_p <- n_total * p
  i_class <- which(Fi >= n_p)[1]
  L <- breaks[i_class]
  F_ant <- ifelse(i_class == 1, 0, Fi[i_class - 1])
  f_i <- frequencies[i_class]
  return(L + ((n_p - F_ant) / f_i) * amplitude)
}


# ──────────────────────────────────────────────────────────────────────────────
# Medidas de posición
# ──────────────────────────────────────────────────────────────────────────────
calculate_position_measures <- function(vector, freq_list, rnd = 4, breaks = NULL, amplitude = NULL) {
  if (!is.null(breaks) && !is.null(amplitude)) {
    # Caso A: Variable Continua (Agrupada por intervalos)
    frecuencias <- as.vector(freq_list$fi) 
    
    # Cuartiles mediante interpolación lineal
    Q1 <- calculate_grouped_quantiles(0.25, freq_list$Fi, frecuencias, breaks, amplitude)
    Q2 <- calculate_grouped_quantiles(0.50, freq_list$Fi, frecuencias, breaks, amplitude) 
    Q3 <- calculate_grouped_quantiles(0.75, freq_list$Fi, frecuencias, breaks, amplitude)
    
    # Percentiles mediante interpolación (deciles 10% al 90%)
    probs <- seq(0.1, 0.9, by = 0.1)
    perc_valores <- sapply(probs, function(p) {
        calculate_grouped_quantiles(p, freq_list$Fi, frecuencias, breaks, amplitude)
    })
    perc_nombres <- paste0(probs * 100, "%")

  } else {
    # Caso B: Variable Discreta o Cruda (Uso de quantile() base)
    cuartiles <- quantile(vector, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
    Q1 <- cuartiles["25%"]
    Q2 <- cuartiles["50%"]
    Q3 <- cuartiles["75%"]
    
    percentiles_raw <- quantile(vector, seq(0.1, 0.9, by = 0.1), na.rm = TRUE)
    perc_valores <- as.vector(percentiles_raw)
    perc_nombres <- names(percentiles_raw)
  }

  # Construcción de DataFrames de salida
  cuartiles_df <- data.frame(
    Q1 = round(as.numeric(Q1), rnd),
    Q2 = round(as.numeric(Q2), rnd),
    Q3 = round(as.numeric(Q3), rnd),
    Rango_IC = round(as.numeric(Q3 - Q1), rnd)
  )
  
  percentiles_df <- data.frame(
    Percentil = perc_nombres,
    Valor = round(perc_valores, rnd)
  )

  return(
    list(
        cuartiles = cuartiles_df, 
        percentiles = percentiles_df
    )
  )
}


# ──────────────────────────────────────────────────────────────────────────────
# Medidas de dispersión
# ──────────────────────────────────────────────────────────────────────────────
calculate_dispersion_measures <- function(vector, freq_list, rnd = 4, mean, class_marks = NULL) {
  if (!is.null(class_marks)) {
    # Caso A: Variable Continua (Agrupada por intervalos)
    frecuencias <- as.vector(freq_list$fi) 

    # Varianza
    n_total <- sum(frecuencias)
    varianza <- sum(
        frecuencias * (class_marks - mean)^2
    ) / (n_total - 1)
    
    # Desvió Estándar
    desvio_estandar <- sqrt(varianza)
    
    # Coef de Variación
    coef_variacion <- (desvio_estandar / mean) * 100
    
    # Data Frame
    medidas_dispersion <- data.frame(
        Varianza = round(varianza, rnd),
        Desvio_Estandar = round(desvio_estandar, rnd),
        Coef_Variacion = percent(
        round(coef_variacion / 100, rnd), 
        accuracy = 0.01
        )
    )
  } else {
    # Caso B: Variable Discreta o Cruda
    varianza <- var(vector, na.rm = TRUE)
    desvio_estandar <- sd(vector, na.rm = TRUE)
    coef_variacion <- (desvio_estandar / mean) * 100
    
    # Data Frame
    medidas_dispersion <- data.frame(
        Varianza = round(varianza, rnd),
        Desvio_Estandar = round(desvio_estandar, rnd),
        Coef_Variacion = percent(
        round(coef_variacion / 100, rnd), 
        accuracy = 0.01
        )
    )
  }
  
  return(medidas_dispersion)
}


# ──────────────────────────────────────────────────────────────────────────────
# Calcular probabilidades Binomiales con redondeo simétrico
# ──────────────────────────────────────────────────────────────────────────────
calculate_binomial_probability <- function(x, size, prob, op = "eq", rnd = 4) {
  probability <- switch(op,
                        # X = x
                        "eq"  = dbinom(x, size = size, prob = prob),
                        # X < x  (menor estricto)
                        "lt"  = pbinom(x - 1, size = size, prob = prob, lower.tail = TRUE),
                        # X <= x (menor o igual)
                        "lte" = pbinom(x, size = size, prob = prob, lower.tail = TRUE),
                        # X > x  (mayor estricto)
                        "gt"  = pbinom(x, size = size, prob = prob, lower.tail = FALSE),
                        # X >= x (mayor o igual)
                        "gte" = pbinom(x - 1, size = size, prob = prob, lower.tail = FALSE)
  )
  
  return(round(probability, rnd))
}


# ──────────────────────────────────────────────────────────────────────────────
# Calcular probabilidades de Poisson con redondeo simétrico
# ──────────────────────────────────────────────────────────────────────────────
calculate_poisson_probability <- function(x, lambda, op = "eq", rnd = 4) {
  probability <- switch(op,
                        # X = x
                        "eq"  = dpois(x, lambda = lambda),
                        # X < x  (menor estricto)
                        "lt"  = ppois(x - 1, lambda = lambda, lower.tail = TRUE),
                        # X <= x (menor o igual)
                        "lte" = ppois(x, lambda = lambda, lower.tail = TRUE),
                        # X > x  (mayor estricto)
                        "gt"  = ppois(x, lambda = lambda, lower.tail = FALSE),
                        # X >= x (mayor o igual)
                        "gte" = ppois(x - 1, lambda = lambda, lower.tail = FALSE)
  )
  
  return(round(probability, rnd))
}


# ==============================================================================
# FUNCIONES PRINCIPALES
# ==============================================================================


# ──────────────────────────────────────────────────────────────────────────────
# Análisis de Variable Continua
# ──────────────────────────────────────────────────────────────────────────────
analyze_continuous_variable <- function(vector, var_name, rnd = 4) {
  
  # ───────────────────────────────────
  # Crear header
  message("\nANÁLISIS DE VARIABLE CONTINUA: ", var_name)
  
  # ───────────────────────────────────
  # 1. Tabla de frecuencias agrupadas
  
  # Llamar a la función sturges_breaks() para calcular los puntos de cortes
  cortes <- sturges_breaks(vector)$breaks
  
  # Calcular amplitud
  amplitud <- sturges_breaks(vector)$amplitude
  
  # Construir marcas de clase
  marcas <- class_marks_with_breaks(cortes)

  # Clasificar datos en intervalos
  # cut(): Divide un vector numérico en intervalos (clases)
  clases <- cut(
    # Vector a agrupar
    vector,
    # Vector con los intervalos de corte calculados
    breaks = cortes,
    # Intervalos cerrados a la izquierda y abiertos a la derecha
    right = FALSE,
    # Esto asegura que el valor mínimo se incluya en el primer intervalo
    include.lowest = TRUE
  )
  
  # Construir tabla de frecuencias
  frecs <- calculate_frequencies(clases)

  # Crear dataframe con resultados
  tabla_frec <- create_df(freq_list = frecs, rnd = rnd, class_intervals = clases, breaks = marcas)
  
  # ───────────────────────────────────
  # 2. Medidas de tendencia central
  medidas_centrales <- calculate_central_measures(vector, frecs, rnd, marcas, cortes, amplitud)
  
  # ───────────────────────────────────
  # 3. Medidas de posición
  medidas_posicion <- calculate_position_measures(vector, frecs, rnd, cortes, amplitud)
  
  # ───────────────────────────────────
  # 4. Medidas de dispersión
  medidas_dispersion <- calculate_dispersion_measures(vector, frecs, rnd, medidas_centrales$Media, marcas)
  
  # ───────────────────────────────────
  # 5. Resultados
  resultados <- list(
    medidas_centrales = medidas_centrales,
    medidas_posicion = medidas_posicion,
    medidas_dispersion = medidas_dispersion,
    tabla_frecuencias = tabla_frec,
    n = length(vector),
    cortes = cortes
  )

  return(resultados)
}


# ──────────────────────────────────────────────────────────────────────────────
# Análisis de variable discreta
# ──────────────────────────────────────────────────────────────────────────────
analyze_discrete_variable <- function(vector, var_name, rnd = 4) {
  
  # ───────────────────────────────────
  # Crear header
  message("\nANÁLISIS DE VARIABLE DISCRETA: ", var_name)

  # ───────────────────────────────────
  # 1. Tabla de frecuencias simple

  # Construir tabla de frecuencias
  frecs <- calculate_frequencies(vector)
  
  # Crear dataframe con resultados
  tabla_frec <- create_df(freq_list = frecs, rnd = rnd)
  
  # ───────────────────────────────────
  # 2. Medidas de tendencia central
  medidas_centrales <- calculate_central_measures(vector, frecs, rnd)
  
  # ───────────────────────────────────
  # 3. Medidas de posición

  medidas_posicion <- calculate_position_measures(vector, frecs, rnd)

  # ───────────────────────────────────
  # 4. Medidas de dispersión
  medidas_dispersion <- calculate_dispersion_measures(vector, frecs, rnd, medidas_centrales$Media)
  
  # ───────────────────────────────────
  # 5. Resultados
  
  resultados <- list(
    medidas_centrales = medidas_centrales,
    medidas_posicion = medidas_posicion,
    medidas_dispersion = medidas_dispersion,
    tabla_frecuencias = tabla_frec,
    n = length(vector)
  )
  
  return(resultados)
}


# ──────────────────────────────────────────────────────────────────────────────
# Análisis de variable categórica ordinal
# ──────────────────────────────────────────────────────────────────────────────
analyze_ordinal_variable <- function(
    vector, 
    var_name, 
    levels = NULL, 
    labels = NULL, 
    rnd = 4
) {
  
  # ───────────────────────────────────
  # Crear header
  message("\nANÁLISIS DE VARIABLE ORDINAL: ", var_name)
  
  # ───────────────────────────────────
  # 1. Tabla de frecuencias simple
  
  # Convertir a factor ordenado si es necesario
  if (!is.ordered(vector) && !is.null(levels) && !is.null(labels)) {
    vector <- factor(vector, levels = levels, labels = labels, ordered = TRUE)
  }
  
  # Construir tabla de frecuencias
  frecs <- calculate_frequencies(vector)
  
  # Crear dataframe con resultados
  tabla_frec <- create_df(freq_list = frecs, rnd = rnd)
  
  # ───────────────────────────────────
  # 2. Medidas descriptivas
  vector_num <- as.numeric(vector)
  
  moda <- names(frecs$fi)[which.max(frecs$fi)]
  q1 <- levels(vector)[quantile(vector_num, 0.25, na.rm = TRUE, type = 1)]
  mediana <- levels(vector)[quantile(vector_num, 0.5, na.rm = TRUE, type = 1)]
  q3 <- levels(vector)[quantile(vector_num, 0.75, na.rm = TRUE, type = 1)]
  
  medidas_df <- data.frame(
    Moda = moda,
    Q1 = q1,
    Mediana = mediana,
    Q3 = q3
  )
  
  # ───────────────────────────────────
  # 3. Resultados
  resultados <- list(
    tabla_frecuencias = tabla_frec,
    medidas_descriptivas = medidas_df,
    niveles = levels(vector),
    n = length(vector)
  )
  
  return(resultados)
}


# ──────────────────────────────────────────────────────────────────────────────
# Crear histograma
# ──────────────────────────────────────────────────────────────────────────────
render_histogram <- function(
    vector,
    breaks,
    var_name,
    frequency_table,
    show_values = FALSE,
    show_density = FALSE,
    descriptive_measures = NULL,
    position_measures = NULL,
    color = "steelblue"
) {
  
  # Crear dataframe con los datos de la tabla de frecuencias
  tipo_frec <- frequency_table$Frec_Abs
  
  df_barras <- data.frame(
    xmin = head(breaks, -1),    # Límites inferiores
    xmax = tail(breaks, -1),    # Límites superiores
    y = tipo_frec,
    marca_clase = frequency_table$Marca_Clase
  )

  p <- ggplot() +
    geom_rect(
      data = df_barras,
      aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = y),
      fill = color,
      color = "white",
      alpha = 0.8
    ) +
    labs(
      title = paste("Histograma de", var_name),
      x = var_name,
      y = "Frecuencia"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_text(face = "bold")
    ) 

  # Mostrar valores encima de las barras
  if (show_values) {
    p <- p + 
      geom_text(
        data = df_barras,
        aes(x = marca_clase, y = y, label = y),
        vjust = -0.5,
        color = "darkblue",
        fontface = "bold",
        linewidth = 3.2
      )
  }
  
  # Curva de densidad
  if (show_density) {
    df_densidad <- data.frame(x = vector)
    
    p <- p + 
      geom_density(
        data = df_densidad,
        aes(
          x = x, 
          y = after_stat(count) * diff(range(breaks)) / length(breaks)
        ),
        color = "darkred",
        linewidth = 0.5,
        fill = "red",
        alpha = 0.2,
        adjust = 0.8
      )
  }
  
  # Bloque de Medidas (Centrales y de Posición)
  y_max <- max(tipo_frec)
  label_y_start <- y_max * 0.95
  label_spacing <- y_max * 0.05
  x_range <- range(vector)
  label_x <- x_range[2] * 0.80 # Ajustado un poco más a la izquierda para que quepa todo

  # 1. Líneas de Tendencia Central (Si existen)
  if (!is.null(descriptive_measures)) {
    
    # Obtener la altura máxima para posicionamiento
    y_max <- max(tipo_frec)
    
    # Posicionamiento en esquina superior derecha
    x_range <- range(vector)
    label_x <- x_range[2] * 0.85
    label_y_start <- y_max * 0.95
    label_spacing <- y_max * 0.05
    
    p <- p + 
      geom_vline(
        xintercept = descriptive_measures$Media, 
        color = "blue", 
        linetype = "dashed", 
        linewidth = 0.8
      ) + 
      geom_vline(
        xintercept = descriptive_measures$Mediana, 
        color = "green", 
        linetype = "solid", 
        linewidth = 0.8
      ) + 
      geom_vline(
        xintercept = descriptive_measures$Moda, 
        color = "red", 
        linetype = "dotdash", 
        linewidth = 0.8
      ) +
      annotate(
        "text", 
        x = label_x, 
        y = label_y_start,
        label = paste("x̄:", round(descriptive_measures$Media, 2)),
        color = "blue", 
        hjust = 0,
        fontface = "bold",
        linewidth = 4
      ) +
      annotate(
        "text", 
        x = label_x, 
        y = label_y_start - label_spacing,
        label = paste("M:", round(descriptive_measures$Mediana, 2)),
        color = "green", 
        hjust = 0,
        fontface = "bold",
        linewidth = 4
      ) +
      annotate(
        "text", 
        x = label_x, 
        y = label_y_start - (2 * label_spacing),
        label = paste("Mo:", round(descriptive_measures$Moda, 2)),
        color = "red", 
        hjust = 0,
        fontface = "bold",
        linewidth = 4
      )
    }

    # 2. Líneas de Cuartiles (Si existen)
  if (!is.null(position_measures)) {
    p <- p + 
      geom_vline(
        xintercept = position_measures$Q1, 
        color = "violet", 
        linetype = "dotted", 
        linewidth = 0.9
        ) + 
      geom_vline(
        xintercept = position_measures$Q3, 
        color = "violet", 
        linetype = "dotted", 
        linewidth = 0.9
        ) +
      annotate(
        "text", 
        x = label_x, 
        y = label_y_start - (3 * label_spacing), 
        label = paste("Q1:", round(position_measures$Q1, 2)), 
        color = "violet", 
        hjust = 0, 
        fontface = "bold"
        ) +
      annotate(
        "text", 
        x = label_x, 
        y = label_y_start - (4 * label_spacing), 
        label = paste("Q3:", round(position_measures$Q3, 2)), 
        color = "violet", 
        hjust = 0, 
        fontface = "bold"
        )
  }

  return(p)
}


# ──────────────────────────────────────────────────────────────────────────────
# Crear diagrama circular
# ──────────────────────────────────────────────────────────────────────────────
render_pie_chart <- function(frequency_table, var_name, palette = "Set2") {
  
  porcentajes <- round(frequency_table$Frec_Rel * 100, 2)
  
  ggplot(frequency_table, aes(x = "", y = Frec_Rel, fill = Categoría)) +
    geom_bar(
      stat = "identity", 
      width = 1,
      color = "white",
      linewidth = 0.5
    ) +
    coord_polar("y", start = 0) +
    geom_text(
      aes(label = paste0(porcentajes, "%\n(", Frec_Abs, ")")),
      position = position_stack(vjust = 0.5),
      color = "black",
      linewidth = 3,
      fontface = "bold"
    ) +
    scale_fill_brewer(palette = palette) +
    labs(
      title = paste("Distribución de", var_name),
      fill = "Categorías"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.title = element_text(face = "bold")
    )
}


# ==============================================================================
# APLICACIÓN AL CASO DE ESTUDIO
# ==============================================================================

# 1. Cargar el archivo una sola vez y guardar la ruta
# ruta_archivo <- file.choose()
ruta_archivo <- "data/TUPAD-2026-EST-TPI-planilla3.xlsx"

# 2. Leer la primera hoja para los datos generales
datos <- read_excel(ruta_archivo)


# ------------------------------------------------------------------------------
# 2a) ANÁLISIS DE TIEMPO DE ESTUDIO SEMANAL (Variable continua)
# ------------------------------------------------------------------------------

# Variable continua
tiempo_estudio <- datos$`TIEMPO_SEMANAL_ESTUDIO_HS`

# Llamado a la función analyze_continuous_variable()
res_tiempo <- analyze_continuous_variable(
  tiempo_estudio, 
  "Tiempo de estudio semanal (horas)"
)

cat("\nTabla de frecuencias agrupadas:\n")
print(res_tiempo$tabla_frec, row.names = FALSE)
# View(res_tiempo$tabla_frec, title = "Tabla Frec. - Tiempo")

cat("\nMedidas de tendencia central:\n")
print(res_tiempo$medidas_centrales, row.names = FALSE)
# View(res_tiempo$medidas_centrales, title = "Medidas Centrales - Tiempo")

cat("\nMedidas de dispersión:\n")
print(res_tiempo$medidas_dispersion, row.names = FALSE)
# View(res_tiempo$medidas_dispersion, title = "Medidas Dispersión - Tiempo")

cat("\nMedidas de posición - Cuartiles: \n")
print(res_tiempo$medidas_posicion$cuartiles, row.names = FALSE)
# View(res_tiempo$medidas_posicion$cuartiles, title = "Medidas Posición - Tiempo")

cat("\nMedidas de posición - Percentiles: \n")
print(res_tiempo$medidas_posicion$percentiles, row.names = FALSE)

cat("\nRESUMEN ESTADÍSTICO\n")
cat("• Total de observaciones:", res_tiempo$n, "\n")
cat("• Número de intervalos:", length(res_tiempo$cortes) - 1, "\n")
cat("• Amplitud de clase:", diff(res_tiempo$cortes)[1], "\n")


# ------------------------------------------------------------------------------
# 2b) ANÁLISIS DE SATISFACCIÓN (Variable ordinal)
# ------------------------------------------------------------------------------

# Crear vector con todos los valores
satisfaccion <- datos$`SATISF_CON_CARRERA`

# Crear un dataframe con niveles de satisfacción 
# y extraer la segunda columna para forman el vector
df_nivel <- read_excel(ruta_archivo, sheet = 2, skip = 1, col_names = FALSE)
satisfaccion_niveles <- df_nivel$...1
satisfaccion_etiquetas <- df_nivel$...2

res_satisfaccion <- analyze_ordinal_variable(
  satisfaccion,
  "Satisfacción con la carrera",
  levels = satisfaccion_niveles,
  labels = satisfaccion_etiquetas
)

cat("\nTabla de frecuencias:\n")
print(res_satisfaccion$tabla_frecuencias, row.names = FALSE)
# View(res_satisfaccion$tabla_frecuencias, title = "Tabla Frec. - Satisfacción")

cat("\nMedidas descriptivas:\n")
print(res_satisfaccion$medidas_descriptivas, row.names = FALSE)
# View(res_satisfaccion$medidas_descriptivas, title = "Medidas Descriptivas - Satisfacción")


# ------------------------------------------------------------------------------
# 3) MEDIDAS DESCRIPTIVAS (Consigna 3)
# ------------------------------------------------------------------------------

# ───────────────────────────────────
# Crear header
message("\nINTERPRETACIÓN DE MEDIDAS DESCRIPTIVAS\n")

# Para tiempo de estudio
cat("\nTIEMPO DE ESTUDIO SEMANAL:\n")
cat(
  "• Los estudiantes dedican en promedio", 
  res_tiempo$medidas_centrales$Media, 
  "horas semanales al estudio\n"
)
cat(
  "• El 50% estudia menos de", 
  res_tiempo$medidas_centrales$Mediana, 
  "horas semanales\n"
)
cat(
  "• La dispersión es de ±", 
  res_tiempo$medidas_dispersion$Desvio_Estandar, 
  "horas alrededor de la media\n"
)
cat(
  "• El coeficiente de variación es del", 
  res_tiempo$medidas_dispersion$Coef_Variacion,
  "\n"
)

# Para satisfacción
cat("\n\nSATISFACCIÓN CON LA CARRERA:\n")
cat(
  "• El nivel más frecuente es:", 
  res_satisfaccion$medidas_descriptivas$Moda, 
  "\n"
)
cat(
  "• El", 
  round(max(res_satisfaccion$tabla_frecuencias$Frec_Rel) * 100, 2), 
  "% de los estudiantes está",
  res_satisfaccion$medidas_descriptivas$Moda, 
  "\n"
)

# ------------------------------------------------------------------------------
# 4) REPRESENTACIÓN GRÁFICA (Consigna 4)
# ------------------------------------------------------------------------------

# ───────────────────────────────────
# 4a) Histograma para tiempo de estudio
histograma <- render_histogram(
  vector = tiempo_estudio,
  breaks = res_tiempo$cortes,
  var_name = "Tiempo de estudio semanal (horas)",
  frequency_table = res_tiempo$tabla_frecuencias,
  show_values = TRUE,
  show_density = TRUE,
  descriptive_measures = res_tiempo$medidas_centrales,
  position_measures = res_tiempo$medidas_posicion$cuartiles
)
print(histograma)

# ───────────────────────────────────
# 4b) Diagrama circular para satisfacción
diagrama <- render_pie_chart(
  res_satisfaccion$tabla_frecuencias,
  "Satisfacción con la carrera"
)
print(diagrama)

# ───────────────────────────────────
# 4c) Análisis de gráficos

message("\nANÁLISIS DE GRÁFICOS\n")

cat("\nHISTOGRAMA:\n")
if(
  res_tiempo$medidas_centrales$Media 
  > res_tiempo$medidas_centrales$Mediana
) {
  distribucion <- "es Asimétrica positiva"
} else if(
  res_tiempo$medidas_centrales$Media 
  < res_tiempo$medidas_centrales$Mediana
) {
  distribucion <- "es Asimétrica negativa"
} else {distribucion <- "no presenta asimetría"}

cat("• La distribución", distribucion, "\n")
cat(
  "• Intervalo modal:", 
  res_tiempo$tabla_frecuencias$Intervalo[
    which.max(res_tiempo$tabla_frecuencias$Frec_Abs)
  ], 
  "\n")

cat("\n\nDIAGRAMA CIRCULAR:\n")
cat(
  "• Satisfacción positiva:", 
  round(sum(res_satisfaccion$tabla_frecuencias$Frec_Rel[1:2]) * 100, 2), 
  "%\n"
)
cat(
  "• Satisfacción negativa:", 
  round(sum(res_satisfaccion$tabla_frecuencias$Frec_Rel[3:4]) * 100, 2), 
  "%\n"
)


# ------------------------------------------------------------------------------
# 5) MODELO BINOMIAL (Consigna 5)
# ------------------------------------------------------------------------------

# Extraemos las probabilidades calculadas
p_muy_satisfecho   <- res_satisfaccion$tabla_frecuencias$Frec_Rel[1]
p_satisfecho       <- res_satisfaccion$tabla_frecuencias$Frec_Rel[2]
p_insatisfecho     <- res_satisfaccion$tabla_frecuencias$Frec_Rel[3]
p_muy_insatisfecho <- res_satisfaccion$tabla_frecuencias$Frec_Rel[4]

# Muestra
n <- 16

# ───────────────────────────────────
# 5a) Más de 9 muy satisfechos: P(X > 9)
prob_5a <- calculate_binomial_probability(x = 9, size = n, prob = p_muy_satisfecho, op = "gt")

# ───────────────────────────────────
# 5b) Entre 4 y 8 satisfechos: P(4 <= X <= 8) -> P(X <= 8) - P(X <= 3)
prob_5b <- calculate_binomial_probability(x = 8, size = n, prob = p_satisfecho, op = "lte") - 
  calculate_binomial_probability(x = 3, size = n, prob = p_satisfecho, op = "lte")

# ───────────────────────────────────
# 5c) Menos de 5 insatisfechos: P(X < 5)
prob_5c <- calculate_binomial_probability(x = 5, size = n, prob = p_insatisfecho, op = "lt")

# ───────────────────────────────────
# 5d) Exactamente 10 muy insatisfechos: P(X = 10)
prob_5d <- calculate_binomial_probability(x = 10, size = n, prob = p_muy_insatisfecho)

message("\nANÁLISIS DEL MODELO BINOMIAL\n")

cat(
  "• La probabilidad de que más de 9 estudiantes estén muy satisfechos con la carrera es:", 
  prob_5a, "\n"
)
cat(
  "• La probabilidad de que entre 4 y 8 estudiantes estén satisfechos con la carrera es:", 
  prob_5b, "\n"
)
cat(
  "• La probabilidad de que menos de 5 estudiantes estén insatisfechos con la carrera es:", 
  prob_5c, "\n"
)
cat(
  "• La probabilidad de que exactamente 10 estudiantes estén muy insatisfechos con la carrera es:", 
  prob_5d, "\n"
)


# ------------------------------------------------------------------------------
# 6) MODELO DE POISSON (Consigna 6)
# ------------------------------------------------------------------------------

#Parámetro base
base <- 15/30

# ───────────────────────────────────
# 6a) Por lo menos 6 consultas en 20 minutos
prob_6a <- calculate_poisson_probability(x = 6, lambda = base * 20, op = "gte")

# ───────────────────────────────────
# 6b) A lo sumo 12 consultas en 40 minutos
prob_6b <- calculate_poisson_probability(x = 12, lambda = base * 40, op = "lte")

# ───────────────────────────────────
# 6c) Más de 7 y menos de 10 consultas en 30 minutos: P(7 < X < 10) -> P(X = 8) - P(X = 9)
prob_6c <- calculate_poisson_probability(x = 8, lambda = base * 30) + 
  calculate_poisson_probability(x = 9, lambda = base * 30)


message("\nANÁLISIS DEL MODELO DE POISSON\n")

cat(
  "• La probabilidad de que lleguen por lo menos 6 consultas en 20 minutos es:", 
  prob_6a, "\n"
)
cat(
  "• La probabilidad de que lleguen a lo sumo 12 consultas en 40 minutos es:", 
  prob_6b, "\n"
)
cat(
  "• La probabilidad de que lleguen más de 7 y menos de 10 consultas en 30 minutos es:", 
  prob_6c, "\n"
)


# ------------------------------------------------------------------------------
# GUARDAR RESULTADOS
# ------------------------------------------------------------------------------
guardar <- FALSE

if(guardar) {
  # Guardar gráficos
  ggsave("output/histograma_tiempo_estudio.png", histograma, width = 10, height = 6, dpi = 300)
  ggsave("output/diagrama_satisfaccion.png", diagrama, width = 8, height = 6, dpi = 300)
  
  # Guardar resultados en CSV
  write.csv(res_tiempo$tabla_frecuencias, "output/tabla_frecuencias_tiempo_estudio.csv", row.names = FALSE)
  write.csv(res_satisfaccion$tabla_frecuencias, "output/tabla_frecuencias_satisfaccion.csv", row.names = FALSE)
  
  cat("\n", "=", "ARCHIVOS GUARDADOS", "=", "\n")
  cat("• histograma_tiempo_estudio.png\n")
  cat("• diagrama_satisfaccion.png\n")
  cat("• tabla_frecuencias_tiempo_estudio.csv\n")
  cat("• tabla_frecuencias_satisfaccion.csv\n")
}

# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================