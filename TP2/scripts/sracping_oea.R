# Carga de librerías vistas en clase
library(rvest)      # Para el scraping
library(tidyverse)  # Para manejar los datos (dplyr, ggplot2, etc.)
library(tidytext)   # Para el procesamiento de texto (NLP)
paquetes <- c(
  "tidyverse", # dplyr, ggplot2, readr, etc.
  "rvest",     # Web scraping
  "here",      # Gestión de rutas (Tutorial 04)
  "xml2",      # Para guardar los archivos .html
  "tidytext",  # Procesamiento de texto (NLP)
  "udpipe",    # Lematización (Tutorial 06)
  "tidylo",    # Log Odds Ratio (Tutorial 06)
  "scales",    # Formato de gráficos
  "tm"         # Matrices de términos (DTM)
)

# Instalación forzada para evitar errores de compilación en Windows
install.packages(paquetes, type = "win.binary")

# =================================================================
# SCRIPT: scripts/scraping_oea.R
# Objetivo: Web scraping de comunicados OEA y guardado de crudos
# Basado en: Tutorial 04 (Estructura) y Tutorial 05 (Scraping)
# =================================================================

# 1. Carga de librerías vistas en clase
library(tidyverse)
library(rvest)
library(here)
library(xml2)

# 2. Creación del directorio /data si no existe (Tutorial 04)
# Usamos message() como pide la consigna
data_dir <- here("data")
if (!dir.exists(data_dir)) {
  message("Creando el directorio: ", data_dir)
  dir.create(data_dir, recursive = TRUE)
}

# 3. Configuración inicial (Consigna)
meses <- 1:4
anio <- 2026
base_comunicados <- data.frame()

message("Iniciando el proceso de scraping para el año ", anio)

# 4. Fase 1: Obtención de la lista de links (Tutorial 05)
for (m in meses) {
  url_indice <- paste0("https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=", m, "&nAnio=", anio)
  
  message(">>> Procesando índice del Mes: ", m)
  Sys.sleep(3) # Respeto al Crawl-delay del robots.txt
  
  pagina_indice <- tryCatch(read_html(url_indice), error = function(e) return(NULL))
  
  if (!is.null(pagina_indice)) {
    # Selector que identificamos en el script de referencia
    nodos <- pagina_indice %>% html_nodes(".headlinelink")
    
    if (length(nodos) > 0) {
      temp_df <- data.frame(
        mes = m,
        titulo = nodos %>% html_text(trim = TRUE),
        url_nota = paste0("https://www.oas.org", nodos %>% html_attr("href")),
        stringsAsFactors = FALSE
      )
      base_comunicados <- bind_rows(base_comunicados, temp_df)
    }
  }
}

# 5. Fase 2: Descarga de cuerpos y guardado de HTMLs (Consigna)
message("Descargando contenidos individuales y guardando archivos .html...")

# Creamos el ID y la columna vacía
base_comunicados <- base_comunicados %>% mutate(id = row_number())
base_comunicados$cuerpo <- NA

for (i in 1:nrow(base_comunicados)) {
  
  url_nota <- base_comunicados$url_nota[i]
  id_actual <- base_comunicados$id[i]
  
  Sys.sleep(1) # Delay corto entre notas
  
  html_nota <- tryCatch(read_html(url_nota), error = function(e) return(NULL))
  
  if (!is.null(html_nota)) {
    # Guardamos el HTML original con registro de fecha (Requisito de consigna)
    fecha_hoy <- format(Sys.time(), "%Y-%m-%d")
    nombre_archivo <- paste0("oea_nota_", id_actual, "_", fecha_hoy, ".html")
    write_xml(html_nota, here("data", nombre_archivo))
    
    # Extraemos el cuerpo usando el selector 'p' visto en clase
    texto_cuerpo <- html_nota %>% 
      html_nodes("p") %>% 
      html_text(trim = TRUE) %>% 
      paste(collapse = " ")
    
    base_comunicados$cuerpo[i] <- texto_cuerpo
  }
  
  if (i %% 10 == 0) message("Progreso: ", i, " notas de ", nrow(base_comunicados), " descargadas.")
}

# 6. Guardado de la tabla final en formato .rds (Consigna)
# Seleccionamos solo las 3 variables pedidas: id, titulo, cuerpo
tabla_final <- base_comunicados %>% 
  select(id, titulo, cuerpo)

saveRDS(tabla_final, file = here("data", "oea_raw.rds"))

message("¡Proceso finalizado! Se ha creado el archivo /data/oea_raw.rds")