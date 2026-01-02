################################################################################
# FUNCIONES PARA LA EXTRACCIÓN DE CARACTERÍSTICAS DE IMÁGENES
################################################################################

# Extracción de los valores de los tres canales RGB de una imagen

# parámetros de entrada: 
#   - dataframe con las rutas locales a las imágenes en la columna "ruta"
# parámetros de salida:
#   - lista que tiene como índices las rutas a las imágenes y contiene listas 
#     con los canales RGB (índices r, g, b)

read_images <- function(df){
  
  for (j in 1:nrow(df)) {
    tryCatch({
      img <- readJPEG(df$ruta[j])
      
      # Si la imagen es escala de grises (matriz 2D), replicar canales
      if (length(dim(img)) == 2) {
        r <- img; g <- img; b <- img
        cat("Warning: imagen en escala de grises, ruta: ",df$ruta[j])
      } else {
        # Si es color (matriz 3D)
        r <- img[,,1]
        g <- img[,,2]
        b <- img[,,3]
      }
      
      features[j, 1] = median(r)
      features[j, 2] = median(g)
      features[j, 3] = median(b)
      
      
    }, error = function(e) {
      cat("Error leyendo:", df_datos$ruta[j], "\n")
    })
  }
}