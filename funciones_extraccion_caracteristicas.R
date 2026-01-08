################################################################################
# FUNCIONES PARA LA EXTRACCIÓN DE CARACTERÍSTICAS DE IMÁGENES
################################################################################

library(jpeg)
library(grDevices)
library(imager)
library(entropy)

# Lectura de los valores de los tres canales RGB de una imagen

# parámetros de entrada: 
#   - dataframe con las rutas locales a las imágenes en la columna "ruta"
# parámetros de salida:
#   - lista que tiene como índices las rutas a las imágenes y contiene las
#     imágenes en RGB

leer_imagenes <- function(df){
  
  # creamos una lista vacía dónde guardaremos los valores de cada imagen
  lista_imagenes <- list()
  
  cat("Leyendo", nrow(df), "imágenes...\n")
  
  for (j in 1:nrow(df)) {
    tryCatch({
      
      # leemos la imagen
      img <- readJPEG(df$ruta[j])
      
      # guardamos la imagen   
      lista_imagenes[[df$ruta[j]]] <- img
      
    }, error = function(e) {
      cat("Error leyendo:", df$ruta[j], "\n")
    })
  }
  
  # lista con los valores de los 3 canales de cada imagen
  return(lista_imagenes)
}

# Extracción de los valores medianos de los 3 canales RGB

# parámetros de entrada: 
#   - dataframe con las rutas locales a las imágenes en la columna "ruta"
#   - lista con la info de los 3 canales de cada imagen obtenida con la función 
#     read_images
# parámetros de salida:
#   - dataframe original con la adición de las columnas R_mediana, G_mediana, 
#     B_mediana

extraer_rgb <- function(df, lista) {
  
  # matriz vacía para guardar los valores medianos de R, G y B
  features <- matrix(0, nrow = nrow(df), ncol = 3)
  colnames(features) <- c("R_mediana", "G_mediana", "B_mediana")
  
  cat("Extrayendo RGB de", nrow(df), "imágenes...\n")
  
  # calculamos las medianas de los 3 canales de cada imagen
  j <- 1
  for (nombre in df$ruta) {
      features[j, 1] <- median(lista[[nombre]][,,1])
      features[j, 2] <- median(lista[[nombre]][,,2])
      features[j, 3] <- median(lista[[nombre]][,,3])
      j <- j + 1
  }
  
  # unimos la nueva info al dataframe y nos aseguramos de que la etiqueta es 
  # de tipo factor
  df_final <- cbind(df, features)
  df_final$etiqueta <- as.factor(df_final$etiqueta)
  
  # devolvemos el dataframe original con los nuevos datos añadidos
  return(df_final)
}

# Cambio de variables de RGB a HSV

# parámetros de entrada: 
#   - los tres canales R, G y B
# parámetros de salida:
#   - matriz con los valores de H, S y V 

rgb_a_hsv <- function(r, g, b) {
  
  # formamos una matriz con los valores RGB
  rgb_matrix <- matrix(c(r, g, b), ncol = 3)
  
  # calculamos los valores HSV
  hsv_matrix <- rgb2hsv(t(rgb_matrix), maxColorValue = 1)
  
  # devolvemos la matriz con los valores HSV
  return(t(hsv_matrix))
}

# Extracción de los valores medianos de HSV

# parámetros de entrada: 
#   - dataframe con las rutas locales a las imágenes en la columna "ruta"
#   - lista con la info de los 3 canales de cada imagen obtenida con la función 
#     read_images
# parámetros de salida:
#   - dataframe original con la adición de las columnas H_cos_mediana, 
#     H_sin_mediana, S_mediana, V_mediana

extraer_hsv <- function(df, lista) {
  
  # matriz vacía para guardar los valores medianos de H, S y V
  features <- matrix(0, nrow = nrow(df), ncol = 4)
  colnames(features) <- c("H_cos_mediana", "H_sin_mediana", "S_mediana", "V_mediana")
  
  cat("Extrayendo HSV de", nrow(df), "imágenes...\n")
  
  # calculamos las medianas de los 3 canales de cada imagen
  j <- 1
  for (nombre in df$ruta) {

      hsv_vals <- rgb_a_hsv(
        as.vector(lista[[nombre]][,,1]), # R
        as.vector(lista[[nombre]][,,2]), # G
        as.vector(lista[[nombre]][,,3])  # B
        )
      
      features[j, 1] = median(cos(hsv_vals[, 1]*2*pi)) # cos(H)
      features[j, 2] = median(sin(hsv_vals[, 1]*2*pi)) # sin(H)
      features[j, 3] = median(hsv_vals[, 2])           # S
      features[j, 4] = median(hsv_vals[, 3])           # V
      
      j <- j+1
      
  }
  
  # unimos la nueva info al dataframe y nos aseguramos de que la etiqueta es 
  # de tipo factor
  df_final <- cbind(df, features)
  df_final$etiqueta <- as.factor(df_final$etiqueta)
  
  # devolvemos el dataframe original con los nuevos datos añadidos
  return(df_final)
}

# Extracción de la ratio de pixeles azules en la imagen

# parámetros de entrada: 
#   - dataframe con las rutas locales a las imágenes en la columna "ruta"
#   - lista con la info de los 3 canales de cada imagen obtenida con la función 
#     read_images
# parámetros de salida:
#   - dataframe original con la adición de las columna Ratio_Azul

extraer_ratio_azul <- function(df, lista) {
  
  # vector vacío para guardar las ratios
  features <- numeric(nrow(df))
  
  cat("Extrayendo ratios de", nrow(df), "imágenes...\n")
  
  j <- 1
  for (nombre in df$ruta) {
        
      # Un pixel es "Azul cielo" si el canal Azul es mayor que el Rojo y el Verde
      # y además tiene cierto brillo (para no confundir con objetos oscuros azules)
      pixeles_azules <- sum(lista[[nombre]][,,3] > lista[[nombre]][,,1] & lista[[nombre]][,,3] > lista[[nombre]][,,2] & lista[[nombre]][,,3] > 0.4) 
      features[j] <- (pixeles_azules / length(lista[[nombre]][,,3])) * 100
      
      j <- j+1
      
      }
    
  # unimos la nueva info al dataframe y nos aseguramos de que la etiqueta es 
  # de tipo factor
  df_final <- cbind(df, Ratio_Azul = features)
  df_final$etiqueta <- as.factor(df_final$etiqueta)
  
  # devolvemos el dataframe original con los nuevos datos añadidos
  return(df_final)
}

# Extracción del brillo y contraste de la imagen

# parámetros de entrada: 
#   - dataframe con las rutas locales a las imágenes en la columna "ruta"
#   - lista con la info de los 3 canales de cada imagen obtenida con la función 
#     read_images
# parámetros de salida:
#   - dataframe original con la adición de las columna Brillo y Contraste

extraer_brillo_contraste <- function(df, lista) {
  
  # inicializamos vectores para las dos nuevas características
  features_brillo <- numeric(nrow(df))
  features_contraste <- numeric(nrow(df))
  
  cat("Extrayendo Brillo y Contraste (Gris) de", nrow(df), "imágenes...\n")
  
  j <- 1
  for (nombre in df$ruta) {
    
      # matriz de grises
      gris_matrix <- 0.299 * lista[[nombre]][,,1] + 0.587 * lista[[nombre]][,,2] + 0.114 * lista[[nombre]][,,3]
      
      # Convertimos la matriz a un vector para los cálculos estadísticos
      gris_vector <- as.vector(gris_matrix)
      
      # 2. CÁLCULO DE BRILLO (Media)
      # Un valor alto (cercano a 1) significa una imagen muy clara (Soleado/Día)
      # Un valor bajo (cercano a 0) significa una imagen oscura (Noche)
      features_brillo[j] <- mean(gris_vector, na.rm = TRUE)
      
      # 3. CÁLCULO DE CONTRASTE (Desviación Estándar)
      # Un valor alto significa mucha diferencia entre luces y sombras (Soleado duro)
      # Un valor bajo significa que todo es de un gris similar (Nublado plano o Noche cerrada)
      features_contraste[j] <- sd(gris_vector, na.rm = TRUE)
      
      j <- j+1
  }
  
  # Unir las nuevas columnas al dataframe original
  df_final <- cbind(df, 
                    Brillo = features_brillo, 
                    Contraste = features_contraste)
  
  return(df_final)
}

# Extracción de la desviación estándar del canal V

# parámetros de entrada: 
#   - dataframe con las rutas locales a las imágenes en la columna "ruta"
#   - lista con la info de los 3 canales de cada imagen obtenida con la función 
#     leer_imagenes
# parámetros de salida:
#   - dataframe original con la adición de la columna sd_V

extraer_sd_V <- function(df, lista){
  # vector vacío para guardar los valores de la desviación estándar de V
  features <- numeric(nrow(df))
  
  cat("Extrayendo desviación estándar de V de", nrow(df), "imágenes...\n")
  
  j <- 1
  for (nombre in df$ruta){
    # extraemos los canales HSV
    hsv_vals <- rgb_a_hsv(
      as.vector(lista[[nombre]][,,1]), 
      as.vector(lista[[nombre]][,,2]), 
      as.vector(lista[[nombre]][,,3])
    )
    
    # Cálculo de la desviación estándar de V
    features[j] <- sd(hsv_vals[, 3], na.rm = TRUE)
    j <- j + 1
  }
  
  # unimos la nueva info al dataframe y nos aseguramos de que la etiqueta es 
  # de tipo factor
  df_final <- cbind(df, sd_V = features)
  df_final$etiqueta <- as.factor(df_final$etiqueta)
  
  # devolvemos el dataframe original con los nuevos datos añadidos
  return(df_final)
}

# Extracción de porcentaje de tonos cálidos

# parámetros de entrada: 
#   - dataframe con las rutas locales a las imágenes en la columna "ruta"
#   - lista con la info de los 3 canales de cada imagen obtenida con la función 
#     leer_imagenes
# parámetros de salida:
#   - dataframe original con la adición de la columna Porcentaje_Calidos

extraer_tonos_calidos <- function(df, lista){
  # vector vacío para guardar los valores de los porcentajes 
  features <- numeric(nrow(df))
  
  cat("Extrayendo porcentaje de tonos cálidos de", nrow(df), "imágenes...\n")
  
  j <- 1
  for (nombre in df$ruta){
    # extraemos los canales HSV
    hsv_vals <- rgb_a_hsv(
      as.vector(lista[[nombre]][,,1]), 
      as.vector(lista[[nombre]][,,2]), 
      as.vector(lista[[nombre]][,,3])
    )
    
    # obtenemos el canal H en grados
    H_grados <- hsv_vals[, 1]*360
    
    # número total de píxeles
    total_pixeles <- length(H_grados)
    
    # número de píxeles donde H está entre 0 y 60 grados
    tonos_calidos <- sum(H_grados >= 0 & H_grados <= 60)
    
    # porcentaje de tonos cálidos
    features[j] <- (tonos_calidos/total_pixeles)*100
    
    j <- j + 1
  }
  
  # unimos la nueva info al dataframe y nos aseguramos de que la etiqueta es 
  # de tipo factor
  df_final <- cbind(df, Porcentaje_Calidos = features)
  df_final$etiqueta <- as.factor(df_final$etiqueta)
  
  # devolvemos el dataframe original con los nuevos datos añadidos
  return(df_final)
}


# Extracción de los gradientes verticales medios de H y V

# parámetros de entrada: 
#   - dataframe con las rutas locales a las imágenes en la columna "ruta"
#   - lista con la info de los 3 canales de cada imagen obtenida con la función 
#     leer_imagenes
# parámetros de salida:
#   - dataframe original con la adición de la columna Gradiente_H y Gradiente_V

extraer_gradientes_hv <- function(df, lista){
  # vectores vacíos para guardar los gradientes
  features_H <- numeric(nrow(df))
  features_V <- numeric(nrow(df))
  
  cat("Extrayendo gradientes verticales medios de", nrow(df), "imágenes...\n")
  
  j <- 1
  for (nombre in df$ruta){
    # dimensiones originales para construir las matrices
    filas <- nrow(lista[[nombre]][,,1])
    columnas <- ncol(lista[[nombre]][,,1])
    
    # extraemos los canales HSV
    hsv_vals <- rgb_a_hsv(
      as.vector(lista[[nombre]][,,1]), 
      as.vector(lista[[nombre]][,,2]), 
      as.vector(lista[[nombre]][,,3])
    )
    
    # matrices de H y V
    H_matrix <- matrix(hsv_vals[, 1], nrow = filas, ncol = columnas)
    V_matrix <- matrix(hsv_vals[, 3], nrow = filas, ncol = columnas)
    
    # cálculo del gradiente vertical (diferencia entre filas adyacentes)
    gradiente_H <- abs(diff(H_matrix))
    gradiente_V <- abs(diff(V_matrix))
    
    # guardamos la media del gradiente vertical
    features_H[j] <- mean(gradiente_H, na.rm = TRUE)
    features_V[j] <- mean(gradiente_V, na.rm = TRUE)
    
    j <- j + 1
  }
  
  # unimos la nueva info al dataframe y nos aseguramos de que la etiqueta es 
  # de tipo factor
  df_final <- cbind(df, Gradiente_H = features_H, Gradiente_V = features_V)
  df_final$etiqueta <- as.factor(df_final$etiqueta)
  
  # devolvemos el dataframe original con los nuevos datos añadidos
  return(df_final)
}

# Extracción de gradientes verticales por bandas horizontales

# parámetros de entrada: 
#   - dataframe con las rutas locales a las imágenes en la columna "ruta"
#   - lista con la info de los 3 canales de cada imagen obtenida con la función 
#     leer_imagenes
# parámetros de salida:
#   - dataframe original con la adición de la columna Gradiente_V_Bandas

extraer_gradientes_horizontales <- function(df, lista){
  # vector vacío para guardar los valores
  features <- numeric(nrow(df))
  
  cat("Extrayendo gradientes por bandas de", nrow(df), "imágenes...\n")
  
  j <- 1
  for (nombre in df$ruta){
    # dimensiones originales para construir las matrices
    filas <- nrow(lista[[nombre]][,,1])
    columnas <- ncol(lista[[nombre]][,,1])
    
    # extraemos los canales HSV
    hsv_vals <- rgb_a_hsv(
      as.vector(lista[[nombre]][,,1]), 
      as.vector(lista[[nombre]][,,2]), 
      as.vector(lista[[nombre]][,,3])
    )
    
    # matriz de V
    V_matrix <- matrix(hsv_vals[, 3], nrow = filas, ncol = columnas)
    
    # dividir en 3 bandas horizontales
    altura <- nrow(V_matrix)
    banda1 <- V_matrix[1:floor(altura/3), ]
    banda2 <- V_matrix[(floor(altura/3)+1):floor(2*altura/3), ]
    banda3 <- V_matrix[(floor(2*altura/3)+1):altura, ]
    
    # cálculo de los gradientes verticales para cada banda
    gradiente_banda1 <- abs(diff(banda1))
    gradiente_banda2 <- abs(diff(banda2))
    gradiente_banda3 <- abs(diff(banda3))
    
    # promedio de los gradientes
    mean_gradiente <- mean(c(mean(gradiente_banda1, na.rm = TRUE),
                             mean(gradiente_banda2, na.rm = TRUE),
                             mean(gradiente_banda3, na.rm = TRUE)))
    
    # guardamos la nueva característica
    features[j] <- mean_gradiente
    
    j <- j + 1
  }
  
  # unimos la nueva info al dataframe y nos aseguramos de que la etiqueta es 
  # de tipo factor
  df_final <- cbind(df, Gradiente_V_Bandas = features)
  df_final$etiqueta <- as.factor(df_final$etiqueta)
  
  # devolvemos el dataframe original con los nuevos datos añadidos
  return(df_final)
}

# Extracción de características por bloques (cuadrícula)

# parámetros de entrada: 
#   - dataframe con las rutas locales a las imágenes en la columna "ruta"
#   - lista con la info de los 3 canales de cada imagen obtenida con la función 
#     leer_imagenes
#   - num_bloques: número de divisiones por eje 
# parámetros de salida:
#   - dataframe original con características de mediana_cos_H, mediana_sin_H, sd_V y Porcentaje_Brillantes por bloque

extraer_caracteristicas_bloques <- function(df, lista, num_bloques = 4) {
  
  # lista para guardar los vectores de características de cada imagen
  features_list <- list()
  
  cat("Extrayendo características por bloques de", nrow(df), "imágenes...\n")
  
  for (j in 1:nrow(df)) {
    nombre <- df$ruta[j]
    
    # dimensiones originales
    altura <- nrow(lista[[nombre]][,,1])
    ancho <- ncol(lista[[nombre]][,,1])
    
    # extraer canales HSV
    hsv_vals <- rgb_a_hsv(
      as.vector(lista[[nombre]][,,1]), 
      as.vector(lista[[nombre]][,,2]), 
      as.vector(lista[[nombre]][,,3])
    )
    
    # reconstruimos matrices H y V
    H_matrix <- matrix(hsv_vals[, 1], nrow = altura, ncol = ancho)
    V_matrix <- matrix(hsv_vals[, 3], nrow = altura, ncol = ancho)
    
    # matrices de seno y coseno para H
    H_rad <- H_matrix * 2 * pi
    cos_matrix <- cos(H_rad)
    sin_matrix <- sin(H_rad)
    
    # calculamos el tamaño de cada bloque
    bloque_altura <- floor(altura / num_bloques)
    bloque_ancho <- floor(ancho / num_bloques)
    
    # vector para las características de la imagen actual
    caracteristicas_imagen <- c()
    
    # doble bucle para recorrer la cuadrícula
    for (i in 0:(num_bloques - 1)) {
      for (k in 0:(num_bloques - 1)) {
        
        # índices de inicio y fin del bloque
        fila_inicio <- i * bloque_altura + 1
        fila_fin <- ifelse(i == num_bloques - 1, altura, (i + 1) * bloque_altura)
        col_inicio <- k * bloque_ancho + 1
        col_fin <- ifelse(k == num_bloques - 1, ancho, (k + 1) * bloque_ancho)
        
        # extraemos cada bloques
        bloque_cos <- cos_matrix[fila_inicio:fila_fin, col_inicio:col_fin]
        bloque_sin <- sin_matrix[fila_inicio:fila_fin, col_inicio:col_fin]
        bloque_V <- V_matrix[fila_inicio:fila_fin, col_inicio:col_fin]
        
        # cálculo de la mediana de H y la desviación estándar de V
        mediana_cos_H <- median(bloque_cos, na.rm = TRUE)
        mediana_sin_H <- median(bloque_sin, na.rm = TRUE)
        sd_V <- sd(as.vector(bloque_V), na.rm = TRUE)
        
        # consideramos que un pixel es brillante si su valor V > 0.9
        porcentaje_brillantes <- (sum(bloque_V > 0.9) / length(bloque_V)) * 100
        
        # acumulamos en el vector de la imagen
        caracteristicas_imagen <- c(caracteristicas_imagen, mediana_cos_H, mediana_sin_H, sd_V, porcentaje_brillantes)
      }
    }
    features_list[[j]] <- caracteristicas_imagen
  }
  
  # convertimos la lista a matriz y asignamos nombres a las columnas
  features_matrix <- do.call(rbind, features_list)
  
  colnames(features_matrix) <- paste0("Bloque_", rep(1:(num_bloques^2), each = 4), 
                                      c("mediana_cos_H", "mediana_sin_H", "_sd_V", "_Porcentaje_Brillantes"))
  
  # unimos la nueva info al dataframe y nos aseguramos de que la etiqueta es 
  # de tipo factor
  df_final <- cbind(df, features_matrix)
  
  # devolvemos el dataframe original con los nuevos datos añadidos
  df_final$etiqueta <- as.factor(df_final$etiqueta)
  
  return(df_final)
}

# Extracción de proporción de píxeles extremos por bloque

# parámetros de entrada: 
#   - dataframe con las rutas locales a las imágenes en la columna "ruta"
#   - lista con la info de los 3 canales de cada imagen obtenida con la función 
#     leer_imagenes
#   - num_bloques: número de divisiones por eje 
# parámetros de salida:
#   - dataframe original con las columnas de proporción de oscuros y brillantes por bloque

extraer_pixeles_extremos_bloques <- function(df, lista, num_bloques = 4) {
  
  # lista para guardar los vectores de características
  features_list <- list()
  
  cat("Extrayendo píxeles extremos por bloques de", nrow(df), "imágenes...\n")
  
  for (j in 1:nrow(df)) {
    nombre <- df$ruta[j]
    
    # dimensiones originales
    altura <- nrow(lista[[nombre]][,,1])
    ancho <- ncol(lista[[nombre]][,,1])
    
    # extraemos canales HSV 
    hsv_vals <- rgb_a_hsv(
      as.vector(lista[[nombre]][,,1]), 
      as.vector(lista[[nombre]][,,2]), 
      as.vector(lista[[nombre]][,,3])
    )
    
    # reconstruimos la matriz V
    V_matrix <- matrix(hsv_vals[, 3], nrow = altura, ncol = ancho)
    
    # calculamos el tamaño de cada bloque
    bloque_altura <- floor(altura / num_bloques)
    bloque_ancho <- floor(ancho / num_bloques)
    
    # vector para las características 
    caracteristicas_imagen <- c()
    
    # doble bucle para recorrer la cuadrícula
    for (i in 0:(num_bloques - 1)) {
      for (k in 0:(num_bloques - 1)) {
        
        # índices de inicio y fin del bloque
        fila_inicio <- i * bloque_altura + 1
        fila_fin <- ifelse(i == num_bloques - 1, altura, (i + 1) * bloque_altura)
        col_inicio <- k * bloque_ancho + 1
        col_fin <- ifelse(k == num_bloques - 1, ancho, (k + 1) * bloque_ancho)
        
        # extraemos el bloque del canal V
        bloque_V <- V_matrix[fila_inicio:fila_fin, col_inicio:col_fin]
        total_pixeles_bloque <- length(bloque_V)
        
        # calculamos píxeles extremos
        # oscuros (V < 0.1) y brillantes (V > 0.9)
        pixeles_oscuros <- sum(bloque_V < 0.1)
        pixeles_brillantes <- sum(bloque_V > 0.9)
        
        proporcion_oscuros <- (pixeles_oscuros / total_pixeles_bloque) * 100
        proporcion_brillantes <- (pixeles_brillantes / total_pixeles_bloque) * 100
        
        # acumulamos los dos valores por bloque
        caracteristicas_imagen <- c(caracteristicas_imagen, proporcion_oscuros, proporcion_brillantes)
      }
    }
    features_list[[j]] <- caracteristicas_imagen
  }
  
  # convertimos la lista a matriz
  features_matrix <- do.call(rbind, features_list)
  
  # asignamos nombres a las columnas 
  colnames(features_matrix) <- paste0("Bloque_", rep(1:(num_bloques^2), each = 2),
                                      c("_Proporcion_Oscuros", "_Proporcion_Brillantes"))
  
  # unimos la nueva info al dataframe y nos aseguramos de que la etiqueta es 
  # de tipo factor
  df_final <- cbind(df, features_matrix)
  # devolvemos el dataframe original con los nuevos datos añadidos
  df_final$etiqueta <- as.factor(df_final$etiqueta)
  
  return(df_final)
}

# Extracción de la detección de bordes

# parámetros de entrada: 
#   - dataframe con las rutas locales a las imágenes en la columna "ruta"
#   - lista con la info de los 3 canales de cada imagen obtenida con la función 
#     leer_imagenes
# parámetros de salida:
#   - dataframe original con la columna Sobel_mean

extraer_sobel <- function(df, lista) {
  features <- numeric(nrow(df))
  cat("Extrayendo Sobel de", nrow(df), "imágenes...\n")
  
  j <- 1
  for (nombre in df$ruta) {
    img_array <- lista[[nombre]]
    
    # conversión a formato cimg
    img_cimg <- suppressWarnings(as.cimg(img_array))
    # escala de grises
    img_gray <- grayscale(img_cimg)
    # cálculo de gradientes
    gradientes <- imgradient(img_gray, "xy")
    # combinamos ambos gradientes
    magnitud <- sqrt(gradientes$x^2 + gradientes$y^2)
    
    features[j] <- mean(magnitud, na.rm = TRUE)
    j <- j + 1
  }
  
  df_final <- cbind(df, Sobel_mean = features)
  return(df_final)
}

# Extracción de cantidad de píxeles de alta luminosidad (fuentes de Luz)

# parámetros de entrada: 
#   - df: dataframe con las rutas locales a las imágenes en la columna "ruta"
#   - lista: lista con la info de los 3 canales de cada imagen (procedente de leer_imagenes)
# parámetros de salida:
#   - dataframe original con la columna Fuentes_luz

extraer_fuentes_luz <- function(df, lista) {
  
  # Vector para guardar los resultados
  features <- numeric(nrow(df))
  
  cat("Extrayendo características de Fuentes de Luz de", nrow(df), "imágenes...\n")
  
  for (j in 1:nrow(df)) {
    nombre <- df$ruta[j]
    
    tryCatch({
      # Extraemos los canales directamente de nuestra lista
      r <- lista[[nombre]][,,1]
      g <- lista[[nombre]][,,2]
      b <- lista[[nombre]][,,3]
      
      # Calcular luminancia (conversión a escala de grises ponderada)
      # Esta fórmula da más peso al verde porque el ojo humano es más sensible a él
      img_gray <- 0.299 * r + 0.587 * g + 0.114 * b
      
      # Definir umbral de "luz brillante" (cercano al blanco puro)
      umbral <- 0.9
      
      # Contar cuántos píxeles superan el umbral
      # En una imagen nocturna, estos suelen ser farolas o focos
      features[j] <- sum(img_gray > umbral)
      
    }, error = function(e) {
      cat("Error procesando:", nombre, "\n")
    })
  }
  
  # Unimos la nueva columna al dataframe original
  df_final <- cbind(df, Fuentes_luz = features)
  
  # Aseguramos que la etiqueta sea factor
  df_final$etiqueta <- as.factor(df_final$etiqueta)
  
  return(df_final)
}

# Extracción de la entropía 

# parámetros de entrada: 
#   - df: dataframe con las rutas locales a las imágenes en la columna "ruta"
#   - lista: lista con la info de los 3 canales de cada imagen (procedente de leer_imagenes)
# parámetros de salida:
#   - dataframe original con la columna Entropia

extraer_entropia <- function(df, lista) {
  
  # Inicializamos el vector para la entropía
  features <- numeric(nrow(df))
  
  cat("Extrayendo características de Entropía de", nrow(df), "imágenes...\n")
  
  j <- 1
  for (nombre in df$ruta) {
    img_array <- lista[[nombre]]
    
    # conversión a grises
    gris_matrix <- 0.299 * img_array[,,1] + 0.587 * img_array[,,2] + 0.114 * img_array[,,3]
    tabla_frecuencias <- table(as.vector(round(gris_matrix, 3))) 
    # cálculo de la entropía
    features[j] <- entropy(tabla_frecuencias)
    
    j <- j + 1
  }
  
  df_final <- cbind(df, Entropia = features)
  
  if("etiqueta" %in% names(df_final)) {
    df_final$etiqueta <- as.factor(df_final$etiqueta)
  }
  return(df_final)
}

# Extracción de porcentaje de píxeles de baja luminosidad (píxeles oscuros)

# parámetros de entrada: 
#   - df: dataframe con las rutas locales a las imágenes en la columna "ruta"
#   - lista: lista con la info de los 3 canales de cada imagen (procedente de leer_imagenes)
# parámetros de salida:
#   - dataframe original con la columna Pixeles_oscuros

extraer_pixeles_oscuros <- function(df, lista) {
  
  # Vector para guardar los resultados
  features <- numeric(nrow(df))
  
  cat("Extrayendo características de Píxeles Oscuros de", nrow(df), "imágenes...\n")
  
  for (j in 1:nrow(df)) {
    nombre <- df$ruta[j]
    
    tryCatch({
      # Extraemos los canales directamente de nuestra lista en memoria
      r <- lista[[nombre]][,,1]
      g <- lista[[nombre]][,,2]
      b <- lista[[nombre]][,,3]
      
      # Calcular luminancia (conversión a escala de grises)
      img_gray <- 0.299 * r + 0.587 * g + 0.114 * b
      
      # Cálculo de proporciones
      total_pixeles <- length(img_gray)
      # Definimos el umbral de oscuridad (píxeles casi negros)
      pixeles_oscuros <- sum(img_gray < 0.1)
      
      # Guardamos el resultado como porcentaje (%) de la imagen
      features[j] <- (pixeles_oscuros / total_pixeles) * 100
      
    }, error = function(e) {
      cat("Error procesando:", nombre, "\n")
    })
  }
  
  # Unimos la nueva columna al dataframe original
  df_final <- cbind(df, Pixeles_oscuros = features)
  
  # Aseguramos que la etiqueta sea factor
  df_final$etiqueta <- as.factor(df_final$etiqueta)
  
  return(df_final)
}

# Extracción del rango del canal V

# parámetros de entrada: 
#   - df: dataframe con las rutas locales a las imágenes en la columna "ruta"
#   - lista: lista con la info de los 3 canales de cada imagen (procedente de leer_imagenes)
# parámetros de salida:
#   - dataframe original con la columna Rango_dinamico_V

extraer_rango_dinamico_V <- function(df, lista) {
  
  # Vector para guardar los resultados
  features <- numeric(nrow(df))
  
  cat("Extrayendo Rango Dinámico de", nrow(df), "imágenes...\n")
  
  for (j in 1:nrow(df)) {
    nombre <- df$ruta[j]
    
    tryCatch({
      # Extraemos canales de la lista
      r <- lista[[nombre]][,,1]
      g <- lista[[nombre]][,,2]
      b <- lista[[nombre]][,,3]
      
      # Convertimos a HSV 
      # Solo nos interesa la tercera columna (Value)
      hsv_vals <- rgb_a_hsv(as.vector(r), as.vector(g), as.vector(b))
      v_channel <- hsv_vals[, 3]
      
      # El rango dinámico es la diferencia entre el máximo y el mínimo
      # Mide la amplitud total de luminosidad presente en la escena
      features[j] <- max(v_channel) - min(v_channel)
      
    }, error = function(e) {
      cat("Error procesando:", nombre, "\n")
    })
  }
  
  # Unimos la nueva columna al dataframe
  df_final <- cbind(df, Rango_dinamico_V = features)
  
  # Aseguramos factor para la etiqueta
  df_final$etiqueta <- as.factor(df_final$etiqueta)
  
  return(df_final)
}

# Extracción de la intensidad media del canal V tras aplicar un filtro pasobajo (Blur)

# parámetros de entrada: 
#   - df: dataframe con las rutas locales a las imágenes en la columna "ruta"
#   - lista: lista con la info de los 3 canales de cada imagen (procedente de leer_imagenes)
# parámetros de salida:
#   - dataframe original con la columna V_blurred_mean

extraer_blur_V <- function(df, lista) {
  
  # Vector para guardar los resultados
  features <- numeric(nrow(df))
  
  cat("Extrayendo media de canal V con suavizado de", nrow(df), "imágenes...\n")
  
  for (j in 1:nrow(df)) {
    nombre <- df$ruta[j]
    
    tryCatch({
      # Extraemos los canales directamente de nuestra lista
      r <- lista[[nombre]][,,1]
      g <- lista[[nombre]][,,2]
      b <- lista[[nombre]][,,3]
      
      # Convertimos a HSV y aislamos el canal Value (V)
      hsv_vals <- rgb_a_hsv(as.vector(r), as.vector(g), as.vector(b))
      # Reconstruimos la matriz para poder aplicar el filtro espacial de imager
      V_matrix <- matrix(hsv_vals[, 3], nrow = nrow(r), ncol = ncol(r))
      
      # Convertimos a objeto cimg y aplicamos filtro gaussiano (paso bajo)
      # sigma = 2 es el parámetro de suavizado
      V_blurred <- imager::isoblur(imager::as.cimg(V_matrix), sigma = 2)
      
      # La característica es la media de la imagen suavizada
      features[j] <- mean(V_blurred)
      
    }, error = function(e) {
      cat("Error procesando:", nombre, "\n")
    })
  }
  
  # Unimos la nueva columna al dataframe
  df_final <- cbind(df, V_blurred_mean = features)
  
  # Aseguramos que la etiqueta sea factor
  df_final$etiqueta <- as.factor(df_final$etiqueta)
  
  return(df_final)
}
