### Paquetes ----
library(pacman)
devtools::install_github("tidyverse/readxl") # Descargar la versión en desarrollo de tidyverse. Esto es necesario porque para usar readxl, los archivos .xls de algunas entidades no pueden ser leídos con la versión que está en el CRAN
p_load(animation, cowplot, curl, extrafont, forcats, gganimate, ggforce, 
       ggmap, ggraph, ggrepel, ggridges, janitor, lubridate, mapdata, 
       maps, maptools, purrr, readxl, rgdal, rgeos, scales, sp,
       stringi, stringr, stringdist, tweenr, tidyr, tidygraph,
       tidyverse, treemapify, zoo)

### Setup general ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen = 9999)

### Definir tema de gráficas ----
tema <-  theme_minimal() +
  theme(text = element_text(family="Didact Gothic Regular", color = "grey35"),
        plot.title = element_text(size = 24, face = "bold", margin = margin(10,0,20,0), family="Trebuchet MS Bold", color = "grey25"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666", margin = margin(0, 0, 20, 0), family="Didact Gothic Regular"),
        plot.caption = element_text(hjust = 0, size = 15),
        panel.grid = element_line(linetype = 2), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 16, face = "bold", family="Trebuchet MS Bold"),
        legend.text = element_text(size = 14, family="Didact Gothic Regular"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 18, hjust = 1, face = "bold", margin = margin(0,0,0,0), family="Didact Gothic Regular"),
        axis.text = element_text(size = 16, face = "bold", family="Didact Gothic Regular"))

### Descarga archivos de la Encuesta intercensal 2015 del INEGI con información del total de la población municipal ----

# Definir sufijos con acronimos de entidades
edos <- c("ags", "bc", "bcs", "cam",
          "coah", "col", "chis", "chih",
          "cdmx", "dgo", "gto", "gro",
          "hgo", "jal", "mex", "mich",
          "mor", "nay", "nl", "oax",
          "pue", "qro", "qroo", "slp",
          "sin", "son", "tab", "tamps", 
          "tlax", "ver", "yuc", "zac")

# Definir comienzo del nombre de las variables a descargar 
variables <- c("01_poblacion_")

# Descargar datos al folder 01_datos/inegi/encuesta_intercensal_2015/ 
for (i in seq_along(edos)) {
  for (j in seq_along(variables)) {
    curl_download(paste("http://www.beta.inegi.org.mx/contenidos/Proyectos/enchogares/especiales/intercensal/2015/tabulados/", variables[j], edos[i], ".xls", sep = ""), destfile = paste("01_datos/inegi/encuesta_intercensal_2015/", variables[j], edos[i], ".xls", sep = ""))
    
  }
}


### Importar archivos de la Encuesta intercensal 2015 descargados previamente ----

# Dado que la estructura de los archivos varía dependiendo de la variable y no existe información municipal para todas las variables, tengo que adecuar el código para el chunk de archivos a importar de cada variable

### Población ----
archivos_pob <- list.files(path = "01_datos/inegi/encuesta_intercensal_2015/", pattern = "01_poblacion")

# df_poblacion_pob ----
# Estimadores de la población total en viviendas particulares habitadas por municipio y grupos quinquenales de edad según sexo
df_poblacion_pob <- archivos_pob %>% 
  map(function(x) {
    read_excel(paste0("01_datos/inegi/encuesta_intercensal_2015/", x), sheet = "02", skip = 7, col_names = F) %>% 
      clean_names() # "Limpiar" nombres de variables
  }) %>% 
  # Unir todos los data frames en uno solo
  reduce(rbind) %>% 
  # Renombrar variables
  rename(edo = x1, 
         mpo = x2, 
         gpo_edad = x3,
         estimador = x4, 
         pob_tot = x5, 
         pob_hombres = x6,
         pob_mujeres = x7) %>%   
  # Excluir renglones con notas metodológicas
  filter(!str_detect(edo, "Nota:|\\*")) 

# df_poblacion_pob_registro ----
# Estimadores de la población total y su distribución porcentual según condición de registro de nacimiento por municipio y sexo

df_poblacion_pob_registro <- archivos_pob %>% 
  map(function(x) {
    read_excel(paste0("01_datos/inegi/encuesta_intercensal_2015/", x), sheet = "03", skip = 8, col_names = F) %>% 
      clean_names() # "Limpiar" nombres de variables
  }) %>% 
  # Unir todos los data frames en uno solo
  reduce(rbind) %>% 
  # Renombrar variables
  rename(edo = x1, 
         mpo = x2, 
         sexo = x3,
         estimador = x4, 
         pob_tot = x5, 
         pob_tot_por = x6,
         pob_registrada_por = x7,
         pob_no_registrada_por = x8,
         pob_registrada_otro_pais_por = x9,
         pob_registro_no_especificado = x10) %>% 
  # Excluir renglones con notas metodológicas
  filter(!str_detect(edo, "Nota:|\\*")) 


### Importar archivo con datos de municipios que participaran en la consulta del NAICM ----
mpos_consulta <- 
  read_excel("01_datos/mpos_consulta_nacim.xlsx")

### Limpiar bases de datos para a unión ----

# Homogeneizar nombres incluidos en la lista de mpos. con nombre de mpos. del INEGI ----
mpos_consulta <- 
  mpos_consulta %>% 
  mutate(municipios = case_when(municipios == "SILAO" ~ "SILAO DE LA VICTORIA",
                                municipios == "TLAQUEPAQUE" ~ "SAN PEDRO TLAQUEPAQUE",
                                municipios == "ACAMBAY" ~ "ACAMBAY DE RUÍZ CASTAÑEDA",
                                municipios == "LAZARO CÁRDENAS" ~ "LÁZARO CÁRDENAS",
                                municipios == "TLALTIZAPÁN" ~ "TLALTIZAPÁN DE ZAPATA",
                                municipios == "GRAL. ESCOBEDO" ~ "GENERAL ESCOBEDO",
                                municipios == "SAN PEDRO MIXTEPEC -DTO. 22 -" ~ "SAN PEDRO MIXTEPEC",
                                municipios == "ALTO LUCERO DE GUTIÉRREZ BARRIO" ~ "ALTO LUCERO DE GUTIÉRREZ BARRIOS",
                                municipios == "MEDELLÍN" ~ "MEDELLÍN DE BRAVO", 
                                municipios == "NANCHITAL DE LAZARO CARDENAS DEL RÍO" ~ "NANCHITAL DE LÁZARO CÁRDENAS DEL RÍO", 
                                TRUE ~ municipios))


# Generar copias de las columnas estados y municipios ----

# Estás columnas son las que conservaré después de unir las bases de datos
mpos_consulta <- 
  mpos_consulta %>% 
  mutate(estados_nom = estados,
         municipios_nom = municipios)
         

