######################## 1. Librerias ##########################################
install.packages("pacman")
library(pacman)
pacman::p_load(pdftools,     # Leer archivos PDF
               dplyr,        # Manipular datos
               stopwords,    # Selección de Oraciones en solitario
               ggplot2,      # Graficar
               magrittr,     # Concatenar operaciones con pipe %>%
               wordcloud,    # Generar la nube de Oraciones
               RColorBrewer, # Color Oraciones  
               tidytext,     # Dividir una columna en tokens
               stringr,      # Detecta la presencia o ausencia de un patrón en una cadena.
               ggraph,
               stringi,
               scales,
               tidyr,
               widyr,
               igraph,
               quanteda,
               topicmodels,
               cvTools)

####################### 2. Importe de archivo #################################
#Algunos parámetros básicos de la mineria de texto
# Import pdf file, formato y dimensiones.
texto_minado <- pdftools::pdf_text("input/Texto-Definitivo-CPR-2022-Tapas.pdf")

options(encoding = "utf-8")

length(texto_minado)

# Limpieza general
texto_minado <- gsub("\\r", " ", texto_minado)  
texto_minado <- gsub("\\n", "", texto_minado)
texto_minado <- gsub("\\d\\K\\.(?=\\d)", "", texto_minado, perl = TRUE)

# Colapsar (length = 1)
texto_minado <- paste(texto_minado, collapse = '')

# Limpiar algunos carácteres específicos. 

texto_minado <- gsub('"', "", texto_minado)
texto_minado <- gsub(',', " ", texto_minado)


# Crear un generador de frases
vector = c()
for(i in 1:length(texto_minado)){
  temp <- (strsplit(texto_minado[[i]], "\\.")[[1]])
  print(temp)
  vector <- c(vector, temp)
}

# Hasta el momento todo bien, llegamos a nuestro primer objetivo que es visualizar
# como data frame el texto minado.
texto_minado <- as.data.frame(vector)
View(texto_minado)

######################## 3. Limpieza de datos ##################################
# Definir título de columna.
colnames(texto_minado)[1] <- "Oraciones"

# Quitar detalles fráficos (pie de página, encabezados)
texto_minado$Oraciones <- trimws(texto_minado$Oraciones, "left", whitespace = "[\r]")

# Convertir a 'chr'
texto_minado$Oraciones <- as.character(texto_minado$Oraciones)

# Estructurar y limpiar
texto_minado$Oraciones <- gsub("1", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("2", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("3", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("4", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("5", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("6", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("7", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("8", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("9", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("0", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub(")", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("– ", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("Artículo", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("CAPÍTULO", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("las ", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("Las", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("la ", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("La ", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("del ", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("El ", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("el ", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("Los ", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("ley ", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("Se ", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("ser ", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("En ", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("CONSTITUCIÓN POLÍTICA DE LA REPÚBLICA", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("de ", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("que ", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("y ", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("se ", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("sus ", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("I ", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("podrán", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("deberán", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("podrá", "", texto_minado$Oraciones)
texto_minado$Oraciones <- gsub("deberá", "", texto_minado$Oraciones)



# Crear stopwords ('es') y conversion a df.
texto_minado_sw <- stopwords("es")
texto_minado_sw <- as.data.frame(texto_minado_sw)

# Definir nomobre columna y convertir a 'chr'
colnames(texto_minado_sw) <- "Concepto"
texto_minado_sw$Concepto <- as.character(texto_minado_sw$Concepto)

# Generar ID para cada concepto e la variable 'texto_minado'
texto_minado_ID <- tibble::rowid_to_column(texto_minado, "ID")

# Estructurar repetición de conceptos
texto_minado_end <- texto_minado_ID %>% 
  distinct(Oraciones, .keep_all = TRUE) %>% # Para eliminar filas duplicadas basadas en 'Oraciones'
  unnest_tokens(Concepto, Oraciones, drop = FALSE) %>% 
  distinct(ID, Concepto, .keep_all = TRUE) %>% 
  anti_join(texto_minado_sw) %>% # devuelve todas las filas de x donde no hay valores coincidentes en y, manteniendo solo columnas de x
  filter(str_detect(Concepto, "[^\\d]")) %>% 
  group_by(Concepto) %>% 
  dplyr::mutate(Concepto_total = n()) %>% # Esta funcións nos muestra el N de repeticiones
  ungroup()

# Anaalizamos el resultado, y a este lo graficamos de múltiples maneras.
texto_minado_N_total <- texto_minado_end %>% 
  dplyr::count(Concepto, sort = T)

View(texto_minado_N_total)

# Gráfico de barras con ggplot.

texto_minado_N_total %>%
  head(38) %>% # Visualizar los 38 primeros conceptos
  mutate(Concepto = reorder(Concepto, n)) %>% # Ordenar de mayor a menor
  ggplot(aes(Concepto, n)) + geom_col(fill = "red") + scale_y_continuous(labels = comma_format()) + 
  coord_flip() +  labs(title = paste0("Presencia de conceptos "), subtitle = "Stopwords extraidas", x = "Concepto", y = "N de veces presente")

# Tabla de frecuencia
texto_minado_frq <- texto_minado_end %>%
  group_by(Concepto) %>% 
  count(Concepto) %>%  
  group_by(Concepto) %>%
  mutate(frecuencia = n/dim(texto_minado_N_total)[1])

# Nube de palabras
wordcloud(words = texto_minado_frq$Concepto, freq = texto_minado_frq$frecuencia,
          max.words = 100, 
          random.order = FALSE, 
          rot.per = 0.10,
          colors = brewer.pal(6, "Paired"))
# Para personalizar
display.brewer.all(colorblindFriendly = T)

######################## 6. Guardar datos ######################################
saveRDS(texto_minado, file = "R/texto_minado.rds")
