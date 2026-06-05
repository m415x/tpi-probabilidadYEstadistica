# ==============================================================================
# TRABAJO PRÁCTICO INTEGRADOR - CONSIGNAS 1 A 8
# Estadística Descriptiva y Probabilidad
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
if (!require(readxl)) {
  install.packages("readxl")
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
if (!require(scales)) {
  install.packages("scales")
}
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
  fi <- table(vector) # Frecuencia absoluta
  # cumsum(): Calcula la suma acumulativa de un vector
  Fi <- cumsum(fi) # Frecuencia acumulada
  # prop.table(): Toma una tabla de frecuencias y calcula proporciones
  fr <- prop.table(fi) # Frecuencia relativa
  Fr <- cumsum(fr) # Frecuencia relativa acumulada

  # Devolver lista con nombres para facilitar el acceso
  return(list(
    fi = fi,
    Fi = Fi,
    fr = fr,
    Fr = Fr
  ))
}


# ──────────────────────────────────────────────────────────────────────────────
# Crear dataframe de frecuencias para variable continua o discreta
# ──────────────────────────────────────────────────────────────────────────────
create_df <- function(freq_list,
                      rnd = 4,
                      class_intervals = NULL,
                      breaks = NULL) {
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
  k <- ceiling(1 + 3.322 * log10(
    # length(): Obtiene el número de elementos de un objeto
    length(vector)
  ))

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
    function(v) {
      mean(as.numeric(v))
    }
  )

  return(xi)
}


# ──────────────────────────────────────────────────────────────────────────────
# Medidas de tendencia central para datos agrupados o discretos
# ──────────────────────────────────────────────────────────────────────────────
calculate_central_measures <- function(vector,
                                       freq_list,
                                       rnd = 4,
                                       class_marks = NULL,
                                       breaks = NULL,
                                       amplitude = NULL) {
  if (!is.null(class_marks) &&
    !is.null(breaks) && !is.null(amplitude)) {
    frecuencias <- as.vector(freq_list$fi)

    # ───────────────────────────────────
    # Media
    media <- sum(class_marks * frecuencias) / sum(frecuencias)

    # ───────────────────────────────────
    # Moda
    i_modal_class <- which.max(frecuencias) # Índice de la clase modal
    L_m <- breaks[i_modal_class] # Límite inferior de la clase modal
    f0 <- ifelse(i_modal_class == 1, 0, frecuencias[i_modal_class - 1]) # Frec del intervalo anterior
    f1 <- frecuencias[i_modal_class] # Frec del intervalo modal
    f2 <- ifelse(i_modal_class == length(frecuencias), 0, frecuencias[i_modal_class + 1]) # Frec del intervalo posterior

    moda <- L_m + ((f1 - f0) / ((f1 - f0) + (f1 - f2))) * amplitude

    # ───────────────────────────────────
    # Mediana
    mediana <- calculate_grouped_quantiles(0.50, freq_list$Fi, frecuencias, breaks, amplitude)
  } else {
    media <- mean(vector, na.rm = T)
    mediana <- median(vector, na.rm = T)
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
calculate_position_measures <- function(vector,
                                        freq_list,
                                        rnd = 4,
                                        breaks = NULL,
                                        amplitude = NULL) {
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
    cuartiles <- quantile(vector,
      probs = c(0.25, 0.5, 0.75),
      na.rm = T
    )
    Q1 <- cuartiles["25%"]
    Q2 <- cuartiles["50%"]
    Q3 <- cuartiles["75%"]

    percentiles_raw <- quantile(vector, seq(0.1, 0.9, by = 0.1), na.rm = T)
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

  percentiles_df <- data.frame(Percentil = perc_nombres, Valor = round(perc_valores, rnd))

  return(list(cuartiles = cuartiles_df, percentiles = percentiles_df))
}


# ──────────────────────────────────────────────────────────────────────────────
# Medidas de dispersión
# ──────────────────────────────────────────────────────────────────────────────
calculate_dispersion_measures <- function(vector,
                                          freq_list,
                                          rnd = 4,
                                          mean,
                                          class_marks = NULL) {
  if (!is.null(class_marks)) {
    # Caso A: Variable Continua (Agrupada por intervalos)
    frecuencias <- as.vector(freq_list$fi)

    # Varianza
    n_total <- sum(frecuencias)
    varianza <- sum(frecuencias * (class_marks - mean)^2) / (n_total - 1)

    # Desvió Estándar
    desvio_estandar <- sqrt(varianza)

    # Coef de Variación
    coef_variacion <- (desvio_estandar / mean) * 100

    # Data Frame
    medidas_dispersion <- data.frame(
      Varianza = round(varianza, rnd),
      Desvio_Estandar = round(desvio_estandar, rnd),
      Coef_Variacion = percent(round(coef_variacion / 100, rnd), accuracy = 0.01)
    )
  } else {
    # Caso B: Variable Discreta o Cruda
    varianza <- var(vector, na.rm = T)
    desvio_estandar <- sd(vector, na.rm = T)
    coef_variacion <- (desvio_estandar / mean) * 100

    # Data Frame
    medidas_dispersion <- data.frame(
      Varianza = round(varianza, rnd),
      Desvio_Estandar = round(desvio_estandar, rnd),
      Coef_Variacion = percent(round(coef_variacion / 100, rnd), accuracy = 0.01)
    )
  }

  return(medidas_dispersion)
}


# ──────────────────────────────────────────────────────────────────────────────
# Generador de cortes basados en desviaciones estándar
# ──────────────────────────────────────────────────────────────────────────────
sd_vline <- function(mean, sd, sd_multiplier) {
  return(mean + sd * sd_multiplier)
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
    right = F,
    # Esto asegura que el valor mínimo se incluya en el primer intervalo
    include.lowest = T
  )

  # Construir tabla de frecuencias
  frecs <- calculate_frequencies(clases)

  # Crear dataframe con resultados
  tabla_frec <- create_df(
    freq_list = frecs,
    rnd = rnd,
    class_intervals = clases,
    breaks = marcas
  )

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
analyze_ordinal_variable <- function(vector,
                                     var_name,
                                     levels = NULL,
                                     labels = NULL,
                                     rnd = 4) {
  # ───────────────────────────────────
  # Crear header
  message("\nANÁLISIS DE VARIABLE ORDINAL: ", var_name)

  # ───────────────────────────────────
  # 1. Tabla de frecuencias simple

  # Convertir a factor ordenado si es necesario
  if (!is.ordered(vector) && !is.null(levels) && !is.null(labels)) {
    vector <- factor(vector,
      levels = levels,
      labels = labels,
      ordered = T
    )
  }

  # Construir tabla de frecuencias
  frecs <- calculate_frequencies(vector)

  # Crear dataframe con resultados
  tabla_frec <- create_df(freq_list = frecs, rnd = rnd)

  # ───────────────────────────────────
  # 2. Medidas descriptivas
  vector_num <- as.numeric(vector)

  moda <- names(frecs$fi)[which.max(frecs$fi)]
  q1 <- levels(vector)[quantile(vector_num, 0.25, na.rm = T, type = 1)]
  mediana <- levels(vector)[quantile(vector_num, 0.5, na.rm = T, type = 1)]
  q3 <- levels(vector)[quantile(vector_num, 0.75, na.rm = T, type = 1)]

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
# Calcular probabilidades Binomiales con redondeo simétrico
# ──────────────────────────────────────────────────────────────────────────────
calculate_binomial_probability <- function(x,
                                           size,
                                           prob,
                                           op = "eq",
                                           rnd = 4) {
  probabilidad <- switch(op,
    # X =  x (soporta escalares o vectores)
    "eq" = sum(dbinom(x, size = size, prob = prob)),
    # X <  x (menor estricto)
    "lt" = pbinom(x - 1, size = size, prob = prob),
    # X <= x (menor o igual)
    "lte" = pbinom(x, size = size, prob = prob),
    # X >  x (mayor estricto)
    "gt" = pbinom(
      x,
      size = size,
      prob = prob,
      lower.tail = F
    ),
    # X >= x (mayor o igual)
    "gte" = pbinom(
      x - 1,
      size = size,
      prob = prob,
      lower.tail = F
    )
  )

  return(round(probabilidad, rnd))
}


# ──────────────────────────────────────────────────────────────────────────────
# Calcular probabilidades de Poisson con redondeo simétrico
# ──────────────────────────────────────────────────────────────────────────────
calculate_poisson_probability <- function(x, lambda, op = "eq", rnd = 4) {
  probabilidad <- switch(op,
    # X =  x (soporta escalares o vectores)
    "eq"  = sum(dpois(x, lambda = lambda)),
    # X <  x (menor estricto)
    "lt"  = ppois(x - 1, lambda = lambda),
    # X <= x (menor o igual)
    "lte" = ppois(x, lambda = lambda),
    # X >  x (mayor estricto)
    "gt"  = ppois(x, lambda = lambda, lower.tail = F),
    # X >= x (mayor o igual)
    "gte" = ppois(x - 1, lambda = lambda, lower.tail = F)
  )

  return(round(probabilidad, rnd))
}


# ──────────────────────────────────────────────────────────────────────────────
# Calcular probabilidades Normales con redondeo simétrico
# ──────────────────────────────────────────────────────────────────────────────
calculate_normal_probability <- function(mean,
                                         sd,
                                         critical_x,
                                         op = "lte",
                                         rnd = 4) {
  probabilidad <- switch(op,
    # X <= x (menor o igual)
    "lte" = pnorm(critical_x[1], mean = mean, sd = sd),
    # X >= x (mayor o igual)
    "gte" = pnorm(
      critical_x[1],
      mean = mean,
      sd = sd,
      lower.tail = F
    ),
    # a <= X <= b (Comprendido entre dos valores) -> P(b) - P(a)
    "bet" = pnorm(critical_x[2], mean = mean, sd = sd) - pnorm(critical_x[1], mean = mean, sd = sd)
  )

  return(round(probabilidad, rnd))
}


# ──────────────────────────────────────────────────────────────────────────────
# Calcular cuantiles inversos de la Distribución Normal
# ──────────────────────────────────────────────────────────────────────────────
calculate_normal_quantile <- function(mean,
                                      sd,
                                      prob,
                                      op = "lte",
                                      rnd = 4) {
  # qnorm() calcula el valor de la variable dado un nivel de probabilidad acumulada
  valor_x <- switch(op,
    "lte" = qnorm(p = prob, mean = mean, sd = sd),
    "gte" = qnorm(
      p = prob,
      mean = mean,
      sd = sd,
      lower.tail = F
    )
  )

  return(round(valor_x, rnd))
}


# ──────────────────────────────────────────────────────────────────────────────
# Crear histograma
# ──────────────────────────────────────────────────────────────────────────────
render_histogram <- function(vector,
                             breaks,
                             var_name,
                             frequency_table,
                             show_values = F,
                             show_density = F,
                             descriptive_measures = NULL,
                             position_measures = NULL,
                             color = "steelblue") {
  # Crear dataframe con los datos de la tabla de frecuencias
  tipo_frec <- frequency_table$Frec_Abs

  df_barras <- data.frame(
    xmin = head(breaks, -1),
    # Límites inferiores
    xmax = tail(breaks, -1),
    # Límites superiores
    y = tipo_frec,
    marca_clase = frequency_table$Marca_Clase
  )

  p <- ggplot() +
    geom_rect(
      data = df_barras,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = 0,
        ymax = y
      ),
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
        color = "blue",
        fontface = "bold",
        size = 3.2
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
        size = 4
      ) +
      annotate(
        "text",
        x = label_x,
        y = label_y_start - label_spacing,
        label = paste("M:", round(descriptive_measures$Mediana, 2)),
        color = "green",
        hjust = 0,
        fontface = "bold",
        size = 4
      ) +
      annotate(
        "text",
        x = label_x,
        y = label_y_start - (2 * label_spacing),
        label = paste("Mo:", round(descriptive_measures$Moda, 2)),
        color = "red",
        hjust = 0,
        fontface = "bold",
        size = 4
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
      size = 3,
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


# ──────────────────────────────────────────────────────────────────────────────
# Graficar la Distribución Binomial Teórica
# ──────────────────────────────────────────────────────────────────────────────
render_binomial_dist <- function(size,
                                 prob,
                                 critical_x,
                                 op = "eq",
                                 var_name,
                                 x_label = "Número de Éxitos",
                                 bar_color = "steelblue",
                                 rnd = 4) {
  # 1. Parámetros teóricos centrales
  esperanza <- size * prob
  varianza <- esperanza * (1 - prob)
  desvio <- sqrt(varianza)
  limite_inf <- esperanza - desvio
  limite_sup <- esperanza + desvio

  # 2. Generar el espacio muestral completo (0 a n)
  df_bin <- data.frame(
    Exitos = 0:size,
    Probabilidad = dbinom(0:size, size = size, prob = prob)
  )

  # 3. LÓGICA DINÁMICA DE RESALTADO: Evalúa qué barras cumplen la condición
  df_bin$Cumple <- switch(op,
    "eq"  = (df_bin$Exitos %in% critical_x),
    "lt"  = (df_bin$Exitos < critical_x),
    "lte" = (df_bin$Exitos <= critical_x),
    "gt"  = (df_bin$Exitos > critical_x),
    "gte" = (df_bin$Exitos >= critical_x)
  )

  # 4. Posicionamiento de leyendas
  label_x <- size * 0.75
  label_y_start <- max(df_bin$Probabilidad) * 0.95
  label_spacing <- max(df_bin$Probabilidad) * 0.1

  # 5. Gráfico con ggplot2
  p <- ggplot(df_bin, aes(x = Exitos, y = Probabilidad, fill = Cumple)) +

    # Franja de dispersión típica (Esperanza ± 1 Desvío)
    geom_rect(
      aes(
        xmin = limite_inf,
        xmax = limite_sup,
        ymin = 0,
        ymax = max(Probabilidad) * 1.05
      ),
      fill = "orange",
      alpha = 0.05
    ) +

    # Barras de probabilidad con colores condicionales
    geom_bar(
      stat = "identity",
      color = "white",
      alpha = 0.85,
      width = 0.8
    ) +

    # Mapeo manual de colores: TRUE (Azul de éxito) / FALSE (Gris pasivo)
    scale_fill_manual(values = c("FALSE" = "gray85", "TRUE" = bar_color)) +

    # Líneas de parámetros teóricos
    geom_vline(
      xintercept = esperanza,
      color = "blue",
      linetype = "dashed",
      linewidth = 0.5
    ) +
    geom_vline(
      xintercept = limite_inf,
      color = "darkorange",
      linetype = "dotted",
      linewidth = 0.8
    ) +
    geom_vline(
      xintercept = limite_sup,
      color = "darkorange",
      linetype = "dotted",
      linewidth = 0.8
    ) +

    # Etiquetas de probabilidad inclinadas a 45° con margen superior ampliado
    geom_text(
      aes(label = sprintf("%.4f", Probabilidad), color = Cumple),
      vjust = -0.4,
      hjust = -0.1,
      angle = 45,
      size = 2.5,
      fontface = "bold"
    ) +

    # Mapeo manual para los TEXTOS (TRUE = Mismo color de barra, FALSE = Gris oscuro legible)
    scale_color_manual(values = c("FALSE" = "gray40", "TRUE" = bar_color)) +

    # Textos e indicadores con el color idéntico a su línea correspondiente
    annotate(
      "label",
      x = limite_inf,
      y = max(df_bin$Probabilidad) * 0.85,
      label = paste(" -σ:", round(limite_inf, rnd)),
      color = "darkorange",
      hjust = 1.1,
      fontface = "bold",
      fill = "white"
    ) +
    annotate(
      "label",
      x = limite_sup,
      y = max(df_bin$Probabilidad) * 0.85,
      label = paste(" +σ:", round(limite_sup, rnd)),
      color = "darkorange",
      hjust = -0.1,
      fontface = "bold",
      fill = "white"
    ) +

    # Leyenda con los parámetros teóricos
    annotate(
      "label",
      x = label_x,
      y = label_y_start,
      label = paste0("Esperanza (μ): ", round(esperanza, rnd)),
      color = "blue",
      hjust = 0,
      fontface = "bold",
      size = 4,
      fill = "white",
      label.size = NA
    ) +
    annotate(
      "label",
      x = label_x,
      y = label_y_start - label_spacing,
      label = paste("Varianza (σ²): ", round(varianza, rnd)),
      color = "green",
      hjust = 0,
      fontface = "bold",
      size = 4,
      fill = "white",
      label.size = NA
    ) +
    annotate(
      "label",
      x = label_x,
      y = label_y_start - (2 * label_spacing),
      label = paste("Desvío (σ): ±", round(desvio, rnd)),
      color = "darkorange",
      hjust = 0,
      fontface = "bold",
      size = 4,
      fill = "white",
      label.size = NA
    ) +
    labs(
      title = paste("Distribución Binomial:", var_name),
      subtitle = paste0(
        "(n = ", size, " | p = ", round(prob, rnd), " | Condición: ", op, " ", critical_x, ")"
      ),
      x = x_label,
      y = "Probabilidad Teórica"
    ) +
    scale_x_continuous(breaks = 0:size) +

    # Ampliamos el límite de Y un 20% para que las etiquetas a 45° no se corten arriba
    scale_y_continuous(limits = c(0, max(df_bin$Probabilidad) * 1.20)) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(
        hjust = 0.5,
        face = "italic",
        color = "gray40"
      ),
      axis.title = element_text(face = "bold"),
      # Ocultamos la leyenda automática de TRUE/FALSE
      legend.position = "none",
      panel.grid.minor = element_blank()
    )

  return(p)
}


# ──────────────────────────────────────────────────────────────────────────────
# Graficar la Distribución de Poisson Teórica
# ──────────────────────────────────────────────────────────────────────────────
render_poisson_dist <- function(lambda,
                                critical_x,
                                op = "eq",
                                var_name,
                                x_label = "Número de Eventos",
                                bar_color = "cadetblue",
                                rnd = 4) {
  # 1. Parámetros teóricos centrales (μ = σ²)
  esperanza <- lambda
  varianza <- esperanza
  desvio <- sqrt(varianza)
  limite_inf <- esperanza - desvio
  limite_sup <- esperanza + desvio

  # Limitar el eje X para que no grafique barras invisibles infinitas
  x_max <- ceiling(lambda + (3 * desvio))

  # 2. Generar el espacio muestral acotado (0 a n)
  df_pois <- data.frame(
    Eventos = 0:x_max,
    Probabilidad = dpois(0:x_max, lambda = lambda)
  )

  # 3. LÓGICA DINÁMICA DE RESALTADO: Evalúa qué barras cumplen la condición
  df_pois$Cumple <- switch(op,
    "eq"  = (df_pois$Eventos %in% critical_x),
    "lt"  = (df_pois$Eventos < critical_x),
    "lte" = (df_pois$Eventos <= critical_x),
    "gt"  = (df_pois$Eventos > critical_x),
    "gte" = (df_pois$Eventos >= critical_x)
  )

  # 4. Posicionamiento de leyendas
  label_x <- x_max * 0.8
  label_y_start <- max(df_pois$Probabilidad) * 1.1
  label_spacing <- max(df_pois$Probabilidad) * 0.1

  # 5. Gráfico con ggplot2
  p <- ggplot(df_pois, aes(x = Eventos, y = Probabilidad, fill = Cumple)) +

    # Franja de dispersión típica (Esperanza ± 1 Desvío)
    geom_rect(
      aes(
        xmin = limite_inf,
        xmax = limite_sup,
        ymin = 0,
        ymax = max(Probabilidad) * 1.05
      ),
      fill = "orange",
      alpha = 0.05
    ) +

    # Barras de probabilidad con colores condicionales
    geom_bar(
      stat = "identity",
      color = "white",
      alpha = 0.85,
      width = 0.8
    ) +

    # Mapeo manual de colores: TRUE (Azul de éxito) / FALSE (Gris pasivo)
    scale_fill_manual(values = c("FALSE" = "gray85", "TRUE" = bar_color)) +

    # Líneas de parámetros teóricos
    geom_vline(
      xintercept = esperanza,
      color = "blue",
      linetype = "dashed",
      linewidth = 0.5
    ) +
    geom_vline(
      xintercept = limite_inf,
      color = "darkorange",
      linetype = "dotted",
      linewidth = 0.8
    ) +
    geom_vline(
      xintercept = limite_sup,
      color = "darkorange",
      linetype = "dotted",
      linewidth = 0.8
    ) +

    # Etiquetas de probabilidad inclinadas a 45° con margen superior ampliado
    geom_text(
      aes(label = sprintf("%.4f", Probabilidad), color = Cumple),
      vjust = -0.4,
      hjust = -0.1,
      angle = 45,
      size = 2.5,
      fontface = "bold"
    ) +
    scale_color_manual(values = c("FALSE" = "gray40", "TRUE" = bar_color)) +

    # Textos e indicadores con el color idéntico a su línea correspondiente
    annotate(
      "label",
      x = limite_inf,
      y = max(df_pois$Probabilidad) * 0.85,
      label = paste(" -σ:", round(limite_inf, rnd)),
      color = "darkorange",
      fontface = "bold",
      hjust = 1.1,
      fill = "white"
    ) +
    annotate(
      "label",
      x = limite_sup,
      y = max(df_pois$Probabilidad) * 0.85,
      label = paste(" +σ:", round(limite_sup, rnd)),
      color = "darkorange",
      hjust = -0.1,
      fontface = "bold",
      fill = "white"
    ) +

    # Leyenda con los parámetros teóricos
    annotate(
      "label",
      x = label_x,
      y = label_y_start,
      label = paste0("λ (μ = σ²): ", round(esperanza, rnd)),
      color = "blue",
      hjust = 0,
      fontface = "bold",
      size = 4,
      fill = "white",
      label.size = NA
    ) +
    annotate(
      "label",
      x = label_x,
      y = label_y_start - label_spacing,
      label = paste("Desvío (σ): ±", round(desvio, rnd)),
      color = "darkorange",
      hjust = 0,
      fontface = "bold",
      size = 4,
      fill = "white",
      label.size = NA
    ) +
    labs(
      title = paste("Distribución de Poisson:", var_name),
      x = x_label,
      y = "Probabilidad Teórica"
    ) +
    scale_x_continuous(breaks = seq(0, x_max, by = ifelse(x_max > 15, 2, 1))) +

    # Ampliamos el límite de Y un 20% para que las etiquetas a 45° no se corten arriba
    scale_y_continuous(limits = c(0, max(df_pois$Probabilidad) * 1.20)) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_text(face = "bold"),
      # Ocultamos la leyenda automática de TRUE/FALSE
      legend.position = "none",
      panel.grid.minor = element_blank()
    )

  return(p)
}


# ──────────────────────────────────────────────────────────────────────────────
# Graficar la Distribución Normal Teórica con la Regla Empírica (68-95-99.7)
# ──────────────────────────────────────────────────────────────────────────────
render_normal_dist <- function(mean,
                               sd,
                               critical_x = NULL,
                               op = "lte",
                               prob = NULL,
                               var_name,
                               x_label = "Valor de la Variable (X)",
                               fill_color = "lightblue",
                               rnd = 4) {
  # 1. Generar secuencia de datos que cubra prácticamente toda la campana (μ ± 4σ)
  x_seq <- sd_vline(mean, sd, c(-4, 4))

  # seq() crea el vector continuo para lograr una curva suave y sin dientes
  eje_x <- seq(x_seq[1], x_seq[2], length.out = 300)

  df_norm <- data.frame(
    VariableX = eje_x,
    Densidad = dnorm(eje_x, mean = mean, sd = sd) # Altura de la curva
  )

  # Cortes para la regla empírica
  cortes_1sd <- sd_vline(mean, sd, c(-1, 1))
  cortes_2sd <- sd_vline(mean, sd, c(-2, 2))
  cortes_3sd <- sd_vline(mean, sd, c(-3, 3))

  # 2. Construcción de la base gráfica
  p <- ggplot(df_norm, aes(x = VariableX, y = Densidad))

  # 3. CAPAS DE SOMBREADO DINÁMICO

  # Caso A: Sombreado basado en límites físicos
  if (!is.null(critical_x)) {
    p <- p + geom_area(
      data = subset(df_norm, switch(op,
        "gte" = (VariableX >= critical_x[1]),
        "lte" = (VariableX <= critical_x[1]),
        "bet" = (VariableX >= critical_x[1] &
          VariableX <= critical_x[2])
      )),
      aes(y = Densidad),
      fill = fill_color,
      alpha = 0.4
    )
  }

  # Caso B: Sombreado basado en cuantiles probabilísticos
  if (!is.null(prob)) {
    xi <- calculate_normal_quantile(
      mean = mean,
      sd = sd,
      prob = prob,
      op = op
    )

    p <- p + geom_area(
      data = subset(df_norm, switch(op,
        "lte" = (VariableX <= xi),
        "gte" = (VariableX >= xi)
      )),
      aes(y = Densidad),
      fill = fill_color,
      alpha = 0.4
    )
  }

  # 4. CAPAS DE ELEMENTOS GEOMÉTRICOS Y TEXTOS (Líneas verticales y etiquetas por color)
  p <- p + geom_line(color = "black", linewidth = 1) + # Línea de la campana

    # Regla del 68% (± 1 Desvío)
    geom_vline(
      xintercept = cortes_1sd,
      color = "darkgreen",
      linetype = "dashed",
      alpha = 0.7
    ) +
    annotate(
      "text",
      x = cortes_1sd[1],
      y = max(df_norm$Densidad) * 0.60,
      label = paste(" -1σ:", round(cortes_1sd[1], rnd / 2)),
      color = "darkgreen",
      fontface = "bold",
      hjust = 1.1
    ) +
    annotate(
      "text",
      x = cortes_1sd[2],
      y = max(df_norm$Densidad) * 0.60,
      label = paste(" +1σ:", round(cortes_1sd[2], rnd / 2)),
      color = "darkgreen",
      fontface = "bold",
      hjust = -0.1
    ) +

    # Regla del 95% (± 2 Desvíos)
    geom_vline(
      xintercept = cortes_2sd,
      color = "darkorange",
      linetype = "dotted",
      alpha = 0.9
    ) +
    annotate(
      "text",
      x = cortes_2sd[1],
      y = max(df_norm$Densidad) * 0.40,
      label = paste(" -2σ:", round(cortes_2sd[1], rnd / 2)),
      color = "darkorange",
      fontface = "bold",
      hjust = 1.1
    ) +
    annotate(
      "text",
      x = cortes_2sd[2],
      y = max(df_norm$Densidad) * 0.40,
      label = paste(" +2σ:", round(cortes_2sd[2], rnd / 2)),
      color = "darkorange",
      fontface = "bold",
      hjust = -0.1
    ) +

    # Regla del 99.7% (± 3 Desvíos)
    geom_vline(
      xintercept = cortes_3sd,
      color = "red",
      linetype = "dotdash",
      alpha = 0.5
    ) +
    annotate(
      "text",
      x = cortes_3sd[1],
      y = max(df_norm$Densidad) * 0.20,
      label = paste(" -3σ:", round(cortes_3sd[1], rnd / 2)),
      color = "red",
      fontface = "bold",
      hjust = 1.1
    ) +
    annotate(
      "text",
      x = cortes_3sd[2],
      y = max(df_norm$Densidad) * 0.20,
      label = paste(" +3σ:", round(cortes_3sd[2], rnd / 2)),
      color = "red",
      fontface = "bold",
      hjust = -0.1
    ) +

    # Centro de la distribución (μ)
    geom_vline(
      xintercept = mean,
      color = "blue",
      linetype = "solid",
      linewidth = 0.5
    ) +
    annotate(
      "text",
      x = mean,
      y = max(df_norm$Densidad) * 0.95,
      label = " μ",
      color = "blue",
      fontface = "bold",
      hjust = -0.2
    ) +

    labs(
      title = paste("Distribución Normal:", var_name),
      subtitle = bquote(paste(
        "(μ = ", .(round(mean, rnd)), " cm | σ = ", .(round(sd, rnd)), " cm)"
      )),
      x = x_label,
      y = "Densidad de Probabilidad"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(
        hjust = 0.5,
        face = "italic",
        color = "gray40"
      ),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  if (!is.null(prob)) {
    p <- p +
      geom_vline(
        xintercept = xi,
        color = "purple",
        linetype = "longdash",
        linewidth = 0.8
      ) +
      annotate(
        "text",
        x = xi,
        y = max(df_norm$Densidad) * 0.80,
        label = paste(" xi:", round(xi, rnd / 2)),
        color = "purple",
        fontface = "bold",
        hjust = -0.2
      )
  }

  return(p)
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
res_tiempo <- analyze_continuous_variable(tiempo_estudio, "Tiempo de estudio semanal (horas)")

mostrar_consigna_2a <- function() {
  cat("\014") # Limpiar pantalla

  cat("\nTabla de frecuencias agrupadas:\n")
  print(res_tiempo$tabla_frec, row.names = F)
  # View(res_tiempo$tabla_frec, title = "Tabla Frec. - Tiempo")

  cat("\nMedidas de tendencia central:\n")
  print(res_tiempo$medidas_centrales, row.names = F)
  # View(res_tiempo$medidas_centrales, title = "Medidas Centrales - Tiempo")

  cat("\nMedidas de dispersión:\n")
  print(res_tiempo$medidas_dispersion, row.names = F)
  # View(res_tiempo$medidas_dispersion, title = "Medidas Dispersión - Tiempo")

  cat("\nMedidas de posición - Cuartiles: \n")
  print(res_tiempo$medidas_posicion$cuartiles, row.names = F)
  # View(res_tiempo$medidas_posicion$cuartiles, title = "Medidas Posición - Tiempo")

  cat("\nMedidas de posición - Percentiles: \n")
  print(res_tiempo$medidas_posicion$percentiles, row.names = F)

  cat("\nRESUMEN ESTADÍSTICO\n")
  cat("• Total de observaciones:", res_tiempo$n, "\n")
  cat("• Número de intervalos:", length(res_tiempo$cortes) - 1, "\n")
  cat("• Amplitud de clase:", diff(res_tiempo$cortes)[1], "\n")

  write.csv(
    res_tiempo$tabla_frecuencias,
    "output/tabla_frecuencias_tiempo_estudio.csv",
    row.names = F
  )
  cat("\n-> Tabla de frecuencias exportada a /output.\n")
}


# ------------------------------------------------------------------------------
# 2b) ANÁLISIS DE SATISFACCIÓN (Variable ordinal)
# ------------------------------------------------------------------------------

# Crear vector con todos los valores
satisfaccion <- datos$`SATISF_CON_CARRERA`

# Crear un dataframe con niveles de satisfacción
# y extraer la segunda columna para forman el vector
df_nivel <- read_excel(ruta_archivo,
  sheet = 2,
  skip = 1,
  col_names = F
)
satisfaccion_niveles <- df_nivel$...1
satisfaccion_etiquetas <- df_nivel$...2

res_satisfaccion <- analyze_ordinal_variable(satisfaccion,
  "Satisfacción con la carrera",
  levels = satisfaccion_niveles,
  labels = satisfaccion_etiquetas
)

mostrar_consigna_2b <- function() {
  cat("\014") # Limpiar pantalla

  cat("\nTabla de frecuencias:\n")
  print(res_satisfaccion$tabla_frecuencias, row.names = F)
  # View(res_satisfaccion$tabla_frecuencias, title = "Tabla Frec. - Satisfacción")

  cat("\nMedidas descriptivas:\n")
  print(res_satisfaccion$medidas_descriptivas, row.names = F)
  # View(res_satisfaccion$medidas_descriptivas, title = "Medidas Descriptivas - Satisfacción")

  write.csv(
    res_satisfaccion$tabla_frecuencias,
    "output/tabla_frecuencias_satisfaccion.csv",
    row.names = F
  )
  cat("\n-> Tabla de frecuencias exportada a /output.\n")
}


# ------------------------------------------------------------------------------
# 3) MEDIDAS DESCRIPTIVAS (Consigna 3)
# ------------------------------------------------------------------------------

mostrar_consigna_3 <- function() {
  cat("\014") # Limpiar pantalla

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
    round(max(
      res_satisfaccion$tabla_frecuencias$Frec_Rel
    ) * 100, 2),
    "% de los estudiantes está",
    res_satisfaccion$medidas_descriptivas$Moda,
    "\n"
  )
}


# ------------------------------------------------------------------------------
# 4) REPRESENTACIÓN GRÁFICA (Consigna 4)
# ------------------------------------------------------------------------------

mostrar_consigna_4 <- function() {
  cat("\014") # Limpiar pantalla

  # ───────────────────────────────────
  # 4a) Histograma para tiempo de estudio
  histograma <- render_histogram(
    vector = tiempo_estudio,
    breaks = res_tiempo$cortes,
    var_name = "Tiempo de estudio semanal (horas)",
    frequency_table = res_tiempo$tabla_frecuencias,
    show_values = T,
    show_density = T,
    descriptive_measures = res_tiempo$medidas_centrales,
    position_measures = res_tiempo$medidas_posicion$cuartiles
  )
  print(histograma)

  # ───────────────────────────────────
  # 4b) Diagrama circular para satisfacción
  diagrama_circular <- render_pie_chart(
    res_satisfaccion$tabla_frecuencias,
    "Satisfacción con la carrera"
  )
  print(diagrama_circular)

  # ───────────────────────────────────
  # 4c) Análisis de gráficos

  message("\nANÁLISIS DE GRÁFICOS\n")

  cat("\nHISTOGRAMA:\n")
  if (res_tiempo$medidas_centrales$Media
  > res_tiempo$medidas_centrales$Mediana) {
    distribucion <- "es Asimétrica positiva"
  } else if (res_tiempo$medidas_centrales$Media
  < res_tiempo$medidas_centrales$Mediana) {
    distribucion <- "es Asimétrica negativa"
  } else {
    distribucion <- "no presenta asimetría"
  }

  cat("• La distribución", distribucion, "\n")
  cat(
    "• Intervalo modal:",
    res_tiempo$tabla_frecuencias$Intervalo[which.max(res_tiempo$tabla_frecuencias$Frec_Abs)],
    "\n"
  )

  cat("\n\nDIAGRAMA CIRCULAR:\n")
  cat("• Satisfacción positiva:", round(sum(
    res_satisfaccion$tabla_frecuencias$Frec_Rel[1:2]
  ) * 100, 2), "%\n")
  cat("• Satisfacción negativa:", round(sum(
    res_satisfaccion$tabla_frecuencias$Frec_Rel[3:4]
  ) * 100, 2), "%\n")

  ggsave(
    "output/histograma_tiempo_estudio_semanal.jpg",
    histograma,
    width = 9,
    height = 5,
    dpi = 300
  )
  ggsave(
    "output/diagrama_circular_satisfaccion_con_carrera.jpg",
    diagrama_circular,
    width = 9,
    height = 5,
    dpi = 300
  )
  cat("\n-> Gráficos exportados con éxito a /output.\n")
}

# ------------------------------------------------------------------------------
# 5) MODELO BINOMIAL (Consigna 5)
# ------------------------------------------------------------------------------

mostrar_consigna_5 <- function() {
  cat("\014") # Limpiar pantalla

  # Extraemos las probabilidades calculadas
  p_muy_satisfecho <- res_satisfaccion$tabla_frecuencias$Frec_Rel[1]
  p_satisfecho <- res_satisfaccion$tabla_frecuencias$Frec_Rel[2]
  p_insatisfecho <- res_satisfaccion$tabla_frecuencias$Frec_Rel[3]
  p_muy_insatisfecho <- res_satisfaccion$tabla_frecuencias$Frec_Rel[4]

  # Muestra
  n_muestra <- 16

  # Leyenda eje x
  x_label_binom <- "Número de Alumnos en la Muestra"

  # ───────────────────────────────────
  # 5a) Más de 9 muy satisfechos: P(X > 9)
  prob_5a <- calculate_binomial_probability(
    x = 9,
    size = n_muestra,
    prob = p_muy_satisfecho,
    op = "gt"
  )

  g_binom_5a <- render_binomial_dist(
    size = n_muestra,
    prob = p_muy_satisfecho,
    critical_x = 9,
    op = "gt",
    var_name = "Más de 9 alumnos 'Muy Satisfechos'",
    x_label = x_label_binom
  )
  ggsave(
    "output/binomial_gt9_muy_satisfechos.jpg",
    g_binom_5a,
    width = 9,
    height = 5,
    dpi = 300
  )
  print(g_binom_5a)

  # ───────────────────────────────────
  # 5b) Entre 4 y 8 satisfechos: P(4 <= X <= 8)
  prob_5b <- calculate_binomial_probability(x = 4:8, size = n_muestra, prob = p_satisfecho)

  g_binom_5b <- render_binomial_dist(
    size = n_muestra,
    prob = p_satisfecho,
    critical_x = 4:8,
    var_name = "Entre 4 y 8 alumnos 'Satisfechos'",
    x_label = x_label_binom
  )
  ggsave(
    "output/binomial_lte4_lte8_satisfechos.jpg",
    g_binom_5b,
    width = 9,
    height = 5,
    dpi = 300
  )
  print(g_binom_5b)

  # ───────────────────────────────────
  # 5c) Menos de 5 insatisfechos: P(X < 5)
  prob_5c <- calculate_binomial_probability(
    x = 5,
    size = n_muestra,
    prob = p_insatisfecho,
    op = "lt"
  )

  g_binom_5c <- render_binomial_dist(
    size = n_muestra,
    prob = p_insatisfecho,
    critical_x = 5,
    op = "lt",
    var_name = "Menos de 5 alumnos 'Insatisfechos'",
    x_label = x_label_binom
  )
  ggsave(
    "output/binomial_lt5_insatisfechos.jpg",
    g_binom_5c,
    width = 9,
    height = 5,
    dpi = 300
  )
  print(g_binom_5c)

  # ───────────────────────────────────
  # 5d) Exactamente 10 muy insatisfechos: P(X = 10)
  prob_5d <- calculate_binomial_probability(x = 10, size = n_muestra, prob = p_muy_insatisfecho)

  g_binom_5d <- render_binomial_dist(
    size = n_muestra,
    prob = p_muy_insatisfecho,
    critical_x = 10,
    var_name = "Exactamente 10 alumnos 'Muy Insatisfechos'",
    x_label = x_label_binom
  )
  ggsave(
    "output/binomial_eq10_muy_insatisfechos.jpg",
    g_binom_5d,
    width = 9,
    height = 5,
    dpi = 300
  )
  print(g_binom_5d)

  # Impresión limpia en consola sin eco de sentencias
  withAutoprint(
    {
      message("\nANÁLISIS DEL MODELO BINOMIAL\n")
      cat(
        "========================================================================\n"
      )
      cat(
        " INFORME TÉCNICO: Nivel de Satisfacción de los Estudiantes ( n =",
        n_muestra,
        ")\n"
      )
      cat(
        "========================================================================\n"
      )
      cat(sprintf(" • Parámetros Base de la Población:\n"))
      cat(
        sprintf(
          "     P(Muy Satisfecho):   %.4f  |  P(Satisfecho):       %.4f\n",
          p_muy_satisfecho,
          p_satisfecho
        )
      )
      cat(
        sprintf(
          "     P(Insatisfecho):     %.4f  |  P(Muy Insatisfecho): %.4f\n\n",
          p_insatisfecho,
          p_muy_insatisfecho
        )
      )
      cat(" • Resultados del Análisis de Probabilidad:\n")
      cat(sprintf(
        "     [a] P(X > 9)  Muy Satisfechos:   =>  %s (%.2f%%)\n",
        format(prob_5a, nsmall = 4),
        prob_5a * 100
      ))
      cat(sprintf(
        "     [b] P(4<=X<=8) Satisfechos:      =>  %s (%.2f%%)\n",
        format(prob_5b, nsmall = 4),
        prob_5b * 100
      ))
      cat(sprintf(
        "     [c] P(X < 5)  Insatisfechos:     =>  %s (%.2f%%)\n",
        format(prob_5c, nsmall = 4),
        prob_5c * 100
      ))
      cat(sprintf(
        "     [d] P(X = 10) Muy Insatisfechos: =>  %s (%.2f%%)\n",
        format(prob_5d, nsmall = 4),
        prob_5d * 100
      ))
      cat(
        "────────────────────────────────────────────────────────────────────────\n"
      )
      cat("-> Gráficos exportados con éxito en la carpeta /output.\n")
      cat(
        "========================================================================\n\n"
      )
    },
    echo = FALSE
  )
}


# ------------------------------------------------------------------------------
# 6) MODELO DE POISSON (Consigna 6)
# ------------------------------------------------------------------------------

mostrar_consigna_6 <- function() {
  cat("\014") # Limpiar pantalla

  # Parámetro
  base <- 15 / 30
  lambda_20min <- base * 20
  lambda_40min <- base * 40
  lambda_30min <- base * 30

  x_label_pois <- "Cantidad de Consultas"


  # ───────────────────────────────────
  # 6a) Por lo menos 6 consultas en 20 minutos: P(X >= 6)
  prob_6a <- calculate_poisson_probability(
    x = 6,
    lambda = base * 20,
    op = "gte"
  )

  g_pois_6a <- render_poisson_dist(
    lambda = lambda_20min,
    critical_x = 6,
    op = "gte",
    var_name = "Por lo menos 6 consultas en 20 minutos",
    x_label = x_label_pois
  )
  ggsave(
    "output/poisson_gte6_20minutos.jpg",
    g_pois_6a,
    width = 9,
    height = 5,
    dpi = 300
  )
  print(g_pois_6a)

  # ───────────────────────────────────
  # 6b) A lo sumo 12 consultas en 40 minutos: P(X <= 12)
  prob_6b <- calculate_poisson_probability(
    x = 12,
    lambda = base * 40,
    op = "lte"
  )

  g_pois_6b <- render_poisson_dist(
    lambda = lambda_40min,
    critical_x = 12,
    op = "lte",
    var_name = "A lo sumo 12 consultas en 40 minutos",
    x_label = x_label_pois
  )
  ggsave(
    "output/poisson_lte12_40minutos.jpg",
    g_pois_6b,
    width = 9,
    height = 5,
    dpi = 300
  )
  print(g_pois_6b)

  # ───────────────────────────────────
  # 6c) Más de 7 y menos de 10 consultas en 30 minutos: P(7 < X < 10)
  prob_6c <- calculate_poisson_probability(x = 8:9, lambda = base * 30)

  g_pois_6c <- render_poisson_dist(
    lambda = lambda_30min,
    critical_x = 8:9,
    var_name = "Más de 7 y menos de 10 consultas en 30 minutos",
    x_label = x_label_pois
  )
  ggsave(
    "output/poisson_gt7_lt10_30minutos.jpg",
    g_pois_6c,
    width = 9,
    height = 5,
    dpi = 300
  )
  print(g_pois_6c)

  withAutoprint(
    {
      message("\nANÁLISIS DEL MODELO DE POISSON\n")
      cat(
        "========================================================================\n"
      )
      cat(" INFORME TÉCNICO: Análisis de Consultas Recibidas por Docentes  \n")
      cat(
        "========================================================================\n"
      )
      cat(" • Evaluaciones de Procesos de Tiempo de Tutoría:\n")
      cat(
        sprintf(
          "     [a] Por lo menos 6 consultas en 20 min  (λ = %s) =>  %s (%.2f%%)\n",
          lambda_20min,
          format(prob_6a, nsmall = 4),
          prob_6a * 100
        )
      )
      cat(
        sprintf(
          "     [b] A lo sumo 12 consultas en 40 min    (λ = %s) =>  %s (%.2f%%)\n",
          lambda_40min,
          format(prob_6b, nsmall = 4),
          prob_6b * 100
        )
      )
      cat(
        sprintf(
          "     [c] Entre 8 y 9 consultas en 30 min     (λ = %s) =>  %s (%.2f%%)\n",
          lambda_30min,
          format(prob_6c, nsmall = 4),
          prob_6c * 100
        )
      )
      cat(
        "────────────────────────────────────────────────────────────────────────\n"
      )
      cat("-> Gráficos de distribución de eventos guardados en /output.\n")
      cat(
        "========================================================================\n\n"
      )
    },
    echo = FALSE
  )
}


# ------------------------------------------------------------------------------
# 7) MODELO NORMAL (Consigna 7)
# ------------------------------------------------------------------------------

mostrar_consigna_7 <- function() {
  cat("\014") # Limpiar pantalla

  # Parámetros
  estat_data <- datos$ESTATURA_CM.
  estat_media <- mean(estat_data, na.rm = TRUE)
  estat_desvio <- sd(estat_data, na.rm = TRUE)

  x_label_norm <- "Estatura (cm)"

  # ───────────────────────────────────
  # 7a) Mayor o igual que 179 cm: P(X >= 179)
  prob_7a <- calculate_normal_probability(
    mean = estat_media,
    sd = estat_desvio,
    critical_x = 179,
    op = "gte"
  )

  g_norm_7a <- render_normal_dist(
    mean = estat_media,
    sd = estat_desvio,
    critical_x = 179,
    op = "gte",
    var_name = "Estatura mayor o igual a 179 cm",
    x_label = x_label_norm
  )
  ggsave(
    "output/normal_gte179_estatura.jpg",
    g_norm_7a,
    width = 9,
    height = 5,
    dpi = 300
  )
  print(g_norm_7a)

  # ───────────────────────────────────
  # 7b) Comprendida entre 147 y 172 cm: P(147 <= X <= 172)
  prob_7b <- calculate_normal_probability(
    mean = estat_media,
    sd = estat_desvio,
    critical_x = c(147, 172),
    op = "bet"
  )

  g_norm_7b <- render_normal_dist(
    mean = estat_media,
    sd = estat_desvio,
    critical_x = c(147, 172),
    op = "bet",
    var_name = "Estatura entre 147 y 172 cm",
    x_label = x_label_norm
  )
  ggsave(
    "output/normal_gte147_lte172_estatura.jpg",
    g_norm_7b,
    width = 9,
    height = 5,
    dpi = 300
  )
  print(g_norm_7b)

  # ───────────────────────────────────
  # 7c) Hallar el valor que excede al 97.5% de la población (Cuantil inverso)
  valor_7c <- calculate_normal_quantile(mean = estat_media, sd = estat_desvio, prob = 0.975)

  g_norm_7c <- render_normal_dist(
    mean = estat_media,
    sd = estat_desvio,
    prob = 0.975,
    var_name = "Valor que excede al 97.5% de la población",
    x_label = x_label_norm
  )
  ggsave(
    "output/normal_cuantil_gte97.5_poblacion.jpg",
    g_norm_7c,
    width = 9,
    height = 5,
    dpi = 300
  )
  print(g_norm_7c)

  withAutoprint(
    {
      message("\nANÁLISIS DEL MODELO NORMAL\n")
      cat(
        "========================================================================\n"
      )
      cat(" INFORME TÉCNICO: Estatura de Estudiantes y Parámetros Muestrales\n")
      cat(
        "========================================================================\n"
      )
      cat(sprintf(" • Parámetros Empíricos Calculados de la Muestra Actual:\n"))
      cat(sprintf("     Media Muestral (μ):       %.2f cm\n", estat_media))
      cat(sprintf("     Desviación Estándar (σ):  %.2f cm\n\n", estat_desvio))
      cat(" • Análisis Probabilístico Inferencial:\n")
      cat(sprintf(
        "     [a] P(X >= 179 cm)    Estatura Alta        =>  %s (%.2f%%)\n",
        format(prob_7a, nsmall = 4),
        prob_7a * 100
      ))
      cat(
        sprintf(
          "     [b] P(147<=X<=172)    Estatura Promedio    =>  %s (%.2f%%)\n",
          format(prob_7b, nsmall = 4),
          prob_7b * 100
        )
      )
      cat(sprintf(
        "     [c] Percentil 97.5%%   Umbral de Excedencia =>  %.2f cm\n",
        valor_7c
      ))
      cat(
        "────────────────────────────────────────────────────────────────────────\n"
      )
      cat("-> Curva de Gauss y áreas de probabilidad exportadas a /output.\n")
      cat(
        "========================================================================\n"
      )
    },
    echo = FALSE
  )
}


# ------------------------------------------------------------------------------
# 8) INFERENCIA ESTADÍSTICA (Consigna 8)
# ------------------------------------------------------------------------------

mostrar_consigna_8 <- function() {
  cat("\014") # Limpiar pantalla
  
  # 1. Extraer variable continua (Peso) y limpiar posibles NAs
  peso <- datos$`PESO_KG.`
  
  # 2. Configurar la semilla analítica
  set.seed(415)
  
  # 3. Parámetro poblacional real (Universo completo de estudiantes)
  media_poblacional <- mean(peso, na.rm = T)
  
  # 4. Parámetros del diseño muestral (MAS)
  cantidad_muestras <- 6
  tamaño_muestras <- 20
  
  # Contenedor vectorizado para las medias calculadas
  medias_muestrales <- numeric(cantidad_muestras)
  
  # 5. Iteración automatizada para las 6 extracciones sin reposición
  for (i in 1:cantidad_muestras) {
    # Muestreo Aleatorio Simple (replace = FALSE)
    muestra_i <- sample(peso, tamaño_muestras, replace = F)
    medias_muestrales[i] <- mean(muestra_i)
  }
  
  # 6. Consolidación de métricas en el Dataframe de Control
  tabla_resumen <- data.frame(
    Muestra = paste0("Muestra_", 1:cantidad_muestras),
    Media_Muestral = round(medias_muestrales, 2)
  )
  
  # Cálculo analítico del error de estimación puntual
  tabla_resumen$Diferencia_Respecto_Poblacion <- round(tabla_resumen$Media_Muestral - media_poblacional, 2)
  
  # 7. IMPRESIÓN DEL INFORME TÉCNICO FORMATEADO EN CONSOLA
  withAutoprint({
    message("\n[ANÁLISIS DE DISTRIBUCIONES MUESTRALES Y T.L.C.]")
    cat("========================================================================\n")
    cat(" 📊 INFORME TÉCNICO: Simulación de Muestreo Aleatorio Simple (n = 20)   \n")
    cat("========================================================================\n")
    cat(sprintf(" • Parámetro Poblacional Real (Universo Completo):\n"))
    cat(sprintf("   - Media Poblacional Real (μ): %.4f kg\n\n", media_poblacional))
    cat(" • Resultados de las Medias Muestrales Obtenidas:\n")
    
    # Imprime la tabla resumen de forma limpia en el reporte
    print(tabla_resumen, row.names = FALSE)
    
    cat("────────────────────────────────────────────────────────────────────────\n")
    cat(" 💡 RESPUESTAS EXPLICATIVAS PARA LA CÁTEDRA (ANÁLISIS INFERENCIAL):\n\n")
    cat(" 1) ¿Coinciden los promedios de las muestras con el parámetro?\n")
    cat(sprintf("    No, las medias muestrales de forma individual NO coinciden exactamente\n"))
    cat(sprintf("    con la media poblacional (μ = %.2f kg). Esto se debe a la variabilidad\n", media_poblacional))
    cat(sprintf("    muestral inherente al azar. Sin embargo, se observa que fluctúan con\n"))
    cat(sprintf("    proximidad en torno a ella, actuando como estimadores insesgados.\n\n"))
    
    cat(" 2) ¿Cómo son los promedios muestrales entre sí?\n")
    cat("    Los promedios muestrales son diferentes entre sí, mostrando una dispersión\n")
    cat(sprintf("    que va desde los %.2f kg (Muestra 5) hasta los %.2f kg (Muestra 4).\n", min(tabla_resumen$Media_Muestral), max(tabla_resumen$Media_Muestral)))
    cat("    Esta variabilidad se conoce estadísticamente como Error Estándar de la Media.\n\n")
    
    cat(" 3) CONCLUSIÓN GENERAL EN EL CONTEXTO DEL PROBLEMA:\n")
    cat(sprintf("    Al promediar las 6 medias obtenidas, el resultado es %.2f kg, valor que\n", mean(medias_muestrales)))
    cat(sprintf("    se aproxima notablemente a los %.2f kg poblacionales. Esto demuestra de\n", media_poblacional))
    cat("    manera empírica el Teorema del Límite Central: a pesar de que cada muestra\n")
    cat("    posee un error individual por el azar, la distribución de las medias tiende\n")
    cat("    a concentrarse con simetría en torno al parámetro real de la población.\n")
    cat("========================================================================\n")
  }, echo = FALSE)
}


# ==============================================================================
# MENÚ INTERACTIVO
# ==============================================================================
ejecutar_menu <- TRUE

while (ejecutar_menu) {
  cat("\014") # Limpiar consola al regresar al menú principal
  cat("========================================================================\n")
  cat("  UTN - TPI PROBABILIDAD Y ESTADÍSTICA - Panel de Control Interactivo\n")
  cat("========================================================================\n")
  cat("  [1] Mostrar Consigna 2a: Análisis de Variable Continua (Tiempo de Estudio)\n")
  cat("  [2] Mostrar Consigna 2b: Análisis de Variable Ordinal (Satisfacción con la Carrera)\n")
  cat("  [3] Mostrar Consigna 3:  Medidas Descriptivas de las Variables\n")
  cat("  [4] Mostrar Consigna 4:  Representación Gráfica de las Variables\n")
  cat("  [5] Mostrar Consigna 5:  Distribución Binomial (Satisfacción con la Carrera)\n")
  cat("  [6] Mostrar Consigna 6:  Distribución de Poisson (Consultas Recibidas)\n")
  cat("  [7] Mostrar Consigna 7:  Distribución Normal (Estaturas de Estudiantes)\n")
  cat("  [8] Mostrar Consigna 8:  Inferencia Estadística (Peso de Estudiantes)\n")
  cat("  [0] Salir del Programa\n")
  cat("========================================================================\n")

  # Captura la entrada del usuario desde la consola
  opcion <- readline(prompt = " Seleccione una opción para visualizar el reporte -> ")

  # Evaluación del flujo con estructura switch
  switch(opcion,
    "1" = {
      graphics.off()
      mostrar_consigna_2a()
      readline(prompt = "Presione [ENTER] para regresar al menú principal...")
    },
    "2" = {
      graphics.off()
      mostrar_consigna_2b()
      readline(prompt = "Presione [ENTER] para regresar al menú principal...")
    },
    "3" = {
      graphics.off()
      mostrar_consigna_3()
      readline(prompt = "Presione [ENTER] para regresar al menú principal...")
    },
    "4" = {
      graphics.off()
      mostrar_consigna_4()
      readline(prompt = "Presione [ENTER] para regresar al menú principal...")
    },
    "5" = {
      graphics.off()
      mostrar_consigna_5()
      readline(prompt = "Presione [ENTER] para regresar al menú principal...")
    },
    "6" = {
      graphics.off()
      mostrar_consigna_6()
      readline(prompt = "Presione [ENTER] para regresar al menú principal...")
    },
    "7" = {
      graphics.off()
      mostrar_consigna_7()
      readline(prompt = "Presione [ENTER] para regresar al menú principal...")
    },
    "8" = {
      graphics.off()
      mostrar_consigna_8()
      readline(prompt = "Presione [ENTER] para regresar al menú principal...")
    },
    "0" = {
      ejecutar_menu <- FALSE
      cat("\014")
      cat(
        "========================================================================\n"
      )
      cat(" ¡Programa finalizado con éxito!\n")
      cat(
        "========================================================================\n\n"
      )
    },
    {
      cat("\n Opción inválida. Intente nuevamente.\n")
      Sys.sleep(1.5) # Pausa antes de refrescar el menú
    }
  )
}


# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================
