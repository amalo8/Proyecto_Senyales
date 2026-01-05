################################################################################
# FUNCIONES PARA LA EXTRACCIÓN DE CARACTERÍSTICAS DE IMÁGENES
################################################################################

library(jpeg)
library(grDevices)

# Lectura de los valores de los tres canales RGB de una imagen

# parámetros de entrada: 
#   - dataframe con las rutas locales a las imágenes en la columna "ruta"
# parámetros de salida:
#   - lista que tiene como índices las rutas a las imágenes y contiene listas 
#     con los canales RGB (índices r, g, b)

leer_imagenes <- function(df){
  
  # creamos una lista vacía dónde guardaremos los valores de cada imagen
  lista_imagenes <- list()
  
  at("Leyendo", nrow(df), "imágenes...\n")
  
  for (j in 1:nrow(df)) {
    tryCatch({
      
      # leemos la imagen
      img <- readJPEG(df$ruta[j])
      
      # creamos lista vacía para guardar después los canales
      lista_canales <- list()
      
      # Si la imagen es escala de grises (matriz 2D), replicar canales
      if (length(dim(img)) == 2) {
        r <- img; g <- img; b <- img
        cat("Warning: imagen en escala de grises, ruta: ",df$ruta[j], "\n")
      } else {
        # Si es color (matriz 3D)
        r <- img[,,1]
        g <- img[,,2]
        b <- img[,,3]
      }
      
      # guardamos los canales
      lista_canales["r"] <- r
      lista_canales["g"] <- g
      lista_canales["b"] <- b
      
      # guardamos la lista   
      lista_imagenes[df$ruta[j]] <- lista_canales
      
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
      features[j, 1] = median(lista[nombre]$r)
      features[j, 2] = median(lista[nombre]$g)
      features[j, 3] = median(lista[nombre]$b)
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
#   - dataframe original con la adición de las columnas H_mediana, S_mediana, 
#     V_mediana

extraer_hsv <- function(df, lista) {
  
  # matriz vacía para guardar los valores medianos de H, S y V
  features <- matrix(0, nrow = nrow(df), ncol = 3)
  colnames(features) <- c("H_mediana", "S_mediana", "V_mediana")
  
  cat("Extrayendo HSV de", nrow(df), "imágenes...\n")
  
  # calculamos las medianas de los 3 canales de cada imagen
  j <- 1
  for (nombre in df$ruta) {

      hsv_vals <- rgb_a_hsv(
        as.vector(lista[nombre]$r), 
        as.vector(lista[nombre]$g), 
        as.vector(lista[nombre]$b)
        )
      
      features[j, 1] = median(hsv_vals[, 1])
      features[j, 2] = median(hsv_vals[, 2])
      features[j, 3] = median(hsv_vals[, 3])
      
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
      pixeles_azules <- sum(lista[nombre]$b > lista[nombre]$r & lista[nombre]$b > lista[nombre]$g & lista[nombre]$b > 0.4) 
      features[j] <- (pixeles_azules / length(lista[nombre]$b)) * 100
      
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
      gris_matrix <- 0.299 * lista[nombre]$r + 0.587 * lista[nombre]$g + 0.114 * lista[nombre]$b
      
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

