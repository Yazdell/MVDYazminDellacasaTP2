

# 1. Carga de librerías (Tutorial 06)
library(tidyverse)
library(tidytext)
library(udpipe)
library(here)


# 1. Rutas (Tutorial 04)
output_dir <- here("TP2", "output")
if (!dir.exists(output_dir)) {
  message("Creando el directorio: ", output_dir)
  dir.create(output_dir, recursive = TRUE)
}

# 2. Carga de datos
message("Cargando datos desde TP2/data/oea_raw.rds")
oea_raw <- readRDS(here("TP2", "data", "oea_raw.rds"))

# --- VERIFICACIÓN DE SEGURIDAD ---
# Si la columna se llama 'texto' en lugar de 'cuerpo', esto lo arregla:
if(!"cuerpo" %in% colnames(oea_raw) & "texto" %in% colnames(oea_raw)){
  oea_raw <- oea_raw %>% rename(cuerpo = texto)
}

# 3. Preparación (Tutorial 06)
# Quitamos el filtro de NA un segundo para ver si ese era el problema
oea_prep <- oea_raw %>%
  mutate(cuerpo = as.character(cuerpo),
         id = as.character(id))

# 4. Lematización
message("Lematizando... (Verificá que oea_prep tenga filas en el Environment)")
m_spa <- udpipe_download_model(language = "spanish")
m_spa <- udpipe_load_model(m_spa$file_model)

texto_anotado <- udpipe_annotate(m_spa, x = oea_prep$cuerpo, doc_id = oea_prep$id) %>%
  as.data.frame()

# 5. Filtrado (Puntos a, b y c)
message("Filtrando categorías gramaticales y removiendo stopwords...")

processed_text <- texto_anotado %>%
  # Punto b: Quedarse con sustantivos (NOUN), verbos (VERB) y adjetivos (ADJ)
  filter(upos %in% c("NOUN", "VERB", "ADJ")) %>%
  
  # Punto b: Convertir lemas a minúscula
  mutate(lemma = str_to_lower(lemma)) %>%
  
  # Punto c: Remover stopwords usando tidytext (Tutorial 06)
  anti_join(get_stopwords("es"), by = c("lemma" = "word")) %>% 
  
  # Seleccionamos las columnas pedidas para el análisis
  select(doc_id, lemma, upos)

# 6. Guardado (CONSIGNA)
message("Guardando processed_text.rds en TP2/output")
saveRDS(processed_text, here("TP2", "output", "processed_text.rds"))

message("¡Script finalizado! Revisá si processed_text tiene filas en el Environment")

