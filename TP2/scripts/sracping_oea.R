library(tidyverse)
library(rvest)
library(here)
library(xml2)

# 1. Mensajes y creación del directorio 
data_dir <- here("TP2", "data")

if (!dir.exists(data_dir)) {
  message("Creando el directorio: ", data_dir)
  dir.create(data_dir, recursive = TRUE)
}

# 2. Configuración
meses <- 1:4
anio <- 2026
base_comunicados <- data.frame()

message("Iniciando el proceso de scraping para el año ", anio)

# 3. Scraping y links
for (m in meses) {
  url_indice <- paste0("https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=", m, "&nAnio=", anio)
  message(">>> Procesando índice del Mes: ", m)
  Sys.sleep(2) 
  
  pagina_indice <- tryCatch(read_html(url_indice), error = function(e) return(NULL))
  
  if (!is.null(pagina_indice)) {
    nodos <- pagina_indice %>% html_nodes(".headlinelink")
    if (length(nodos) > 0) {
      temp_df <- data.frame(
        titulo = nodos %>% html_text(trim = TRUE),
        url_nota = paste0("https://www.oas.org", nodos %>% html_attr("href")),
        stringsAsFactors = FALSE
      )
      base_comunicados <- bind_rows(base_comunicados, temp_df)
    }
  }
}

# 4. Scraping y guardado de HTMLs
message("Descargando contenidos y guardando archivos .html en /data...")

base_comunicados <- base_comunicados %>% mutate(id = row_number())
base_comunicados$cuerpo <- NA

for (i in 1:nrow(base_comunicados)) {
  url_nota <- base_comunicados$url_nota[i]
  id_actual <- base_comunicados$id[i]
  
  Sys.sleep(1) 
  html_nota <- tryCatch(read_html(url_nota), error = function(e) return(NULL))
  
  if (!is.null(html_nota)) {
    # Guardo el HTML 
    nombre_html <- paste0("nota_", id_actual, ".html")
    write_xml(html_nota, here("TP2", "data", nombre_html))
    
    # Extraigo cuerpo
    base_comunicados$cuerpo[i] <- html_nota %>% 
      html_nodes("p") %>% 
      html_text(trim = TRUE) %>% 
      paste(collapse = " ")
  }
  if (i %% 10 == 0) message("Progreso: ", i, " notas procesadas.")
}

# 5. Guardo de tabla final ( id, titulo, cuerpo)
tabla_final <- base_comunicados %>% select(id, titulo, cuerpo)

saveRDS(tabla_final, file = here("TP2", "data", "oea_raw.rds"))

message("¡Proceso finalizado! Archivos guardados en TP2/data")