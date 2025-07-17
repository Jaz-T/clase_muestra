#Actividad Clase Muestra 

# Librarías
library(rvest) #Raspar archivos html
library(dplyr) #Manejar datos
library(tidytext) #Manejar datos de texto
library(pdftools) #Manejar archivos pdf
library(stringr) #Manejar y limpiar caracteres 
library(readr) #Leer archivos
library(lubridate) #Manejo de fechas
library(glue) #Colapsar caracteres
library(tokenizers) #Separar en oraciones/palabras/párrafos
library(stringi) #Manipulación de caracteres 
library(ggplot2) #Graficos en R
library(wordcloud2) #Nube de palabras
library(tm)

######################################################################
#####################OBTENER DATOS####################################
######################################################################

# Web Scraping comunicado último anuncio polmon ----
## Página de referencia a los anuncios de polmon
base <- read_html("https://www.banxico.org.mx/publicaciones-y-prensa/anuncios-de-las-decisiones-de-politica-monetaria/anuncios-politica-monetaria-t.html")

## Obtenemos el complemento de los links para los comunicados que queremos descargar
url_com <- base %>%
  html_nodes("a[href]") %>%
  html_attr("href")
### Obtenemos el complemento del último comunicado
url_comp <- url_com[2] #último comunicado

## Url del comunicado con el que queremos trabajar
url <- paste0("https://www.banxico.org.mx", url_comp)

## Scraping del comunicado 
pdf <- list(pdf_text(url))
rm(base, url_com, url_comp, url)
pdf

######################################################################
#####################ESTRUCTURAR DATOS################################
######################################################################

# Obtener una base de datos estructurada a partir del texto obtenido ----

## Extraer fecha del comunicado
comunicado_fecha <- trim(substring(pdf[[1]][[1]],60,120)) #La función trim quita los espacions innecesarios
                                                          #La función substring extrae los carácteres 60 a 120 de 
## Dar formato de fecha al comunicado
### Quitar "de" de la fecha y convertir a minúsculas
comunicado_fecha <- str_remove_all(tolower(comunicado_fecha), "de") 
### Convertir texto a fecha
fecha_data <- as.Date(comunicado_fecha,format="%d %B %Y")

## Nombramos con la fecha del comunicado
names(pdf) <- fecha_data 
### Convertimos la lista en un dataframe 
comunicado_frame <- as.data.frame(unlist(pdf),stringsAsFactors=FALSE) 
### Poner fechas en el data frame
comunicado_frame <- as.data.frame(cbind(row.names(comunicado_frame), comunicado_frame))
### Nombres filas y columnas
rownames(comunicado_frame) <- NULL #Quita nombres de filas
colnames(comunicado_frame) <- c("fecha", "texto") #Asignar nombre a las columnas para identificarlas
### Arreglar fechas 
comunicado_frame$fecha <- as.Date(substring(comunicado_frame$fecha,1,10))

rm(pdf, comunicado_fecha, fecha_data) #Eliminar objetos que no utilizaremos

# Base estructurada ----
comunicado_frame

######################################################################
#####################LIMPIEZA DATOS###################################
######################################################################

# Limpieza texto ----

## Separar oraciones y convertir a minúsculas
comunicado_oraciones <- as_tibble(comunicado_frame) %>%
  unnest_tokens(output= texto, input = texto, token = "sentences",
                to_lower = TRUE) #Crear un renglón por cada oración

## Quitar acentos y homologar encoding del texto 
comunicado_oraciones$texto <- stri_trans_general(str = comunicado_oraciones$texto, id ='latin-ascii')

## Borrar tablas de pronóstico
comunicado_oraciones_sin_tabla <- comunicado_oraciones %>%
  group_by(fecha) %>% #Agrupar comunicados por fecha
  filter(
    row_number() < stringr::str_which(texto, "pronosticos de la inflacion general y subyacente")[1] %>% 
      (function(x){if_else(is.na(x), 100L, x)})
  ) %>% #Filtrar las tablas
  ungroup #Desagrupar

### Borrar menciones relacionadas a ver la tabla, paginación y otros
comunicado_oraciones_sin_tabla$texto <- str_remove_all(comunicado_oraciones_sin_tabla$texto, "[(]ver cuadro[)]") #Eliminar texto que hace referencia a la tabla
comunicado_oraciones_sin_tabla$texto <- str_remove_all(comunicado_oraciones_sin_tabla$texto, "     1") #Eliminar texto página 1
comunicado_oraciones_sin_tabla$texto <- str_remove_all(comunicado_oraciones_sin_tabla$texto, "26 de junio de 2025                                        comunicado de prensa  anuncio de politica monetaria   ") #Eliminar texto página 1

# Tokenización ----

comunicados_palabras <- comunicado_oraciones_sin_tabla %>%
 unnest_tokens(output= palabras, input = texto, 
              token = "words", drop = FALSE) 

# Quitar artículos y preposiciones ----

## Escencialmente se eliminan los términos sin valor analítico, existen diccionarios para los textos en inglés 
## pero no son comunes los de términos en español.

## Creamos un vector con dichos términos
conectores <- c("de", "la", "el", "a", "que", "y", "los", "las", "en", "se", "ellas", "ello", "ellos",
                "del", "por", "un", "una", "al", "con", "lo", "para", "sus", "su", "ella", 
                "este", "esta", "estas", "esto", "como", "asi", "le", "que")

## Eliminar palabras conectoras 

comunicado_limpio <- comunicados_palabras %>%
  dplyr::filter(!(palabras %in% conectores)) #Se filtran dichos términos

# Lematización ----

##Existen palabras que pueden estar conjugadas o pertenecer a la misma raíz,
##se ocupa este método para poder tomarlas como una sóla palabra
# 
# library(udpipe)
# #Modelo en español para lematizar las palabras
# model <- udpipe_load_model(file = "spanish-gsd-ud-2.5-191206.udpipe") 


######################################################################
#####################EXPLORACIÓN DATOS################################
######################################################################

# Conteo de palabras ----
analisis_palabras <- comunicado_limpio %>%
  group_by(palabras) %>%
  summarise(n = n()) %>% #Conteo por palabra
  mutate(percent = n/sum(n)*100) %>% #porcentaje de las entradas con las que contamos 
  arrange(desc(n))

analisis_palabras %>%
  filter(percent>=1) %>%
  ggplot(aes(x=palabras, y=percent)) +
  geom_bar(stat = "identity")

# Nube de palabras ----
data_nube <- analisis_palabras %>% 
  reframe(word = palabras,
          freq = n)

wordcloud2(data_nube, size = 0.7, color = rep_len(c("white", "darkblue"), nrow(data_nube)), backgroundColor = "black", shape = "circle")

######################################################################
#####################ANÁLISIS SENTIMIENTO#############################
######################################################################

# Diccionario sentimiento polmon ----

### La mayor parte de los diccionarios de sentimiento se encuentran en inglés. Por lo que si se desean utilizar es necesario
### traducir el texto. Algunos ejemplos son: Minqing and Bing (2004) (hereinafter referred to as BG), Loughran and McDonald (2011) (LM), Apel and Grimaldi (2014) (ABG), and Bennani and Neuenkirch (2017) (BN).
### Una paquetería también es SentimentAnalysis, pero nuevamente sólo está disponible en inglés.

### Para este ejercicio lo que haremos será crear un diccionario sencillo en español para ver como funciona este análisis

## Definimos términos hawk 

hawk <- c("alza", "presiones", "tensiones", "incertidumbre", "agravamiento", "aumento", "aumentaron", "persistencia", "holgura")
dove <- c("reducir", "redujeron", "recortes", "reducción", "desaceleracion", "apretamiento")


sentimiento_texto <- analisis_palabras %>% 
  mutate(sentimiento = case_when(palabras %in% hawk ~ 1, #Si las palabras son hawk le asignamos un número positivo 
                                 palabras %in% dove ~ -1,#Si las palabras son dove le asignamos un número negativo 
                                 !(palabras %in% c(hawk, dove)) ~ 0)) #Si las palabras no son hawk o dove, entendemos que el texto es neutral y le asignamos cero

### Dado que el texto no aparece el mismo número de veces, se puede ponderar el número por el porcentaje de apariciones 

sentimiento_texto_ponderado <- sentimiento_texto %>% 
  mutate(sentimiento_ponderado = sentimiento*percent)

# Índice restrictividad ----

## Con esto podemos calcular un índice de restrictividad 

indice_restrictividad <- sum(sentimiento_texto_ponderado$sentimiento_ponderado)/length(sentimiento_texto_ponderado)

indice_restrictividad

### Dado que es positivo podemos inferir que el comunicado tiene un tono hawk y esto nos permitería comparar el sentimiento
### con otros comunicados de prensa 

######################################################################
#####################DISCUSIÓN #######################################
######################################################################
