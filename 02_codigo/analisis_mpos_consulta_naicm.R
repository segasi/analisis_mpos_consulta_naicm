### Paquetes ----
library(pacman)
devtools::install_github("tidyverse/readxl") # Descargar la versión en desarrollo de tidyverse. Esto es necesario porque para usar readxl, los archivos .xls de algunas entidades no pueden ser leídos con la versión que está en el CRAN
p_load(animation, curl, extrafont, forcats, gganimate, ggforce, 
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

# Generar df que sólo incluye la estimación de población total municipal por estado ----
df_pob <- 
  df_poblacion_pob %>% 
  filter(estimador == "Valor", 
         mpo != "Total",
         gpo_edad == "Total")

### Cambiar nombre uno de los dos San Pedro Mixtepec a San Pedro Mixtepec Distrito 26 en df_pob ----

# En Oaxaca existen dos municipios que se llaman San Pedro Mixtepec, uno con terminación Distrito 22 y otro Distrito 26. Sin embargo, la base de datos del INEGI no los distingue por nombre. Dado que la consulta será en San Pedro Mixtepec Distrito 22, a continuación renombró la observación de San Pedro Mixtepec Distrito 26. 

# Para distinguir cuál debía eliminar, consulté la ficha básica municipal de población de ambos municipios en http://www.snim.rami.gob.mx/. San Pedro Mixtepec Distrito 26 es el que tiene menor población.  

df_pob <- 
  df_pob %>%
  mutate(mpo = ifelse(mpo == "319 San Pedro Mixtepec *", "319 San Pedro Mixtepec - Distrito 26", mpo)) 



# Generar y guardar nuevas variables dentro df_pob ---- 
df_pob <- 
  df_pob %>% 
  mutate(edo_nom = str_replace(edo, "([0-9][0-9] )", ""), # Generar columna que contenga nombre de edo sin clave estatal
         edo_nom = str_trim(edo_nom), # Quitar potenciales espacios en blanco al principio/final del nombre de cada estado
         edo_nom = str_squish(edo_nom), # Quitar potenciales espacios en blanco en medio del nombre de cada estado
         edo_nom = ifelse(str_detect(edo_nom, "Veracruz"), "Veracruz", ifelse(str_detect(edo_nom, "Coahuila"), "Coahuila", ifelse(str_detect(edo_nom, "Michoacán"), "Michoacán", edo_nom))), # Remplazar nombres oficiales por nombres civilizados de tres entidades 
         edo_min = str_to_lower(edo_nom), # Generar versión en minúsculas del nombre de cada estado
         edo_min = str_replace(edo_min, "á", "a"), # Quitar acentos 
         edo_min = str_replace(edo_min, "é", "e"),
         edo_min = str_replace(edo_min, "í", "i"),
         edo_min = str_replace(edo_min, "ó", "o"),
         edo_min = str_replace(edo_min, "ú", "u"),
         mpo_nom = str_replace(mpo, "([0-9][0-9][0-9] )", ""), # Generar columna que contenga nombre de mpo sin clave municipal
         mpo_nom = str_replace(mpo_nom, "\\*", ""), # Remover asteriscos
         mpo_nom = str_replace(mpo_nom, " \\*", ""), # Remover asteriscos
         mpo_nom = str_trim(mpo_nom), # Quitar potenciales espacios en blanco al principio/final del nombre de cada municipio
         mpo_nom = str_squish(mpo_nom), # Quitar potenciales espacios en blanco en medio del nombre de cada municipio
         mpo_min = str_to_lower(mpo_nom), # Generar versión en minúsculas del nombre de cada estado
         mpo_min = str_replace(mpo_min, "á", "a"), # Quitar acentos 
         mpo_min = str_replace(mpo_min, "é", "e"),
         mpo_min = str_replace(mpo_min, "í", "i"),
         mpo_min = str_replace(mpo_min, "ó", "o"),
         mpo_min = str_replace(mpo_min, "ú", "u"),
         cve_edo = str_replace(edo, " .*", ""), # Generar variable con clave estatal
         cve_mpo = str_replace(mpo, " .*", ""))  # Generar variable con clave municipal



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
                                municipios == "JUCHITAN DE ZARAGOZA" ~ "HEROICA CIUDAD DE JUCHITÁN DE ZARAGOZA", 
                                TRUE ~ municipios))


# Generar copias de las columnas estados y municipios ----
# Estás columnas son las que conservaré después de unir las bases de datos
mpos_consulta <- 
  mpos_consulta %>% 
  mutate(estados_nom = estados,
         municipios_nom = municipios)
         

# Generar y guardar nuevas variables dentro mpos_consulta ----
mpos_consulta <- 
  mpos_consulta %>% 
  mutate(estados = str_to_lower(estados), # Generar versión en minúsculas del nombre de cada estado
         estados = str_trim(estados), # Quitar potenciales espacios en blanco al principio/final del nombre de cada estado
         estados = str_squish(estados), # Quitar potenciales espacios en blanco en medio del nombre de cada estado
         estados = str_replace(estados, "á", "a"), # Quitar acentos
         estados = str_replace(estados, "é", "e"),
         estados = str_replace(estados, "í", "i"),
         estados = str_replace(estados, "ó", "o"),
         estados = str_replace(estados, "ú", "u"),
         municipios = str_to_lower(municipios), # Generar versión en minúsculas del nombre de cada municipio
         municipios = str_trim(municipios), # Quitar potenciales espacios en blanco al principio/final del nombre de cada municipio
         municipios = str_squish(municipios), # Quitar potenciales espacios en blanco en medio del nombre de cada municipio
         municipios = str_replace(municipios, "á", "a"), # Quitar acentos
         municipios = str_replace(municipios, "é", "e"),
         municipios = str_replace(municipios, "í", "i"),
         municipios = str_replace(municipios, "ó", "o"),
         municipios = str_replace(municipios, "ú", "u"))

### Unir bases de datos y generar data frame bd ----
bd <- df_pob %>% 
  full_join(mpos_consulta, by = c("edo_min" = "estados", "mpo_min" = "municipios"))  

### Gráfica de municipios de cada estado incluidos y no incluidos en la consulta y su tamañao poblacional ----
bd %>% 
  mutate(mpo_incluido = ifelse(!is.na(municipios_nom), "Sí", "No")) %>% 
  ggplot(aes(x = pob_tot, 
             y = fct_rev(edo_nom), 
             color = mpo_incluido)) +
  geom_jitter(height = 0.2, alpha = 0.5) +
  scale_x_continuous(labels = comma, breaks = seq(0, 2000000, 250000), expand = c(0, 0), limits = c(0, 2100000)) +
  scale_color_manual(values = c("salmon", "steelblue")) +
  labs(title = str_wrap("RELACIÓN ENTRE LOS MUNICIPIOS EN LOS QUE SE INSTALARÁ (O NO) UNA MESA DE VOTACIÓN EN LA CONSULTA Y SU TAMAÑO POBLACIONAL", width = 70), 
       x = "\nPoblación total", 
       y = NULL,
       color = "¿Habrá consulta en el municipio?",
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: mexicodecide.com.mx y Encuesta Intercensal 2015 del INEGI") +
  tema +
  theme(legend.position = c(0.2, -0.08),
        legend.direction = "horizontal",
        legend.text = element_text(size = 16))

ggsave(filename = "mpos_por_edo_incluidos_y_no.png", path = "03_graficas/", width = 15, height = 10, dpi = 100)



### Gráfica del porcentaje de municipios de cada entidad en los que se instalará una mesa de votación ----

bd %>% 
  mutate(mpo_incluido = ifelse(!is.na(municipios_nom), "Sí", "No")) %>% 
  group_by(edo_nom) %>% 
  summarise(num_mpos = n(),
            total_mpos_incluidos = sum(mpo_incluido == "Sí")) %>% 
  ungroup() %>% 
  mutate(por_mpos_incluidos = round((total_mpos_incluidos/num_mpos)*100, 1)) %>% 
  arrange(-por_mpos_incluidos) %>% 
  ggplot(aes(fct_reorder(edo_nom, por_mpos_incluidos), por_mpos_incluidos)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 100, 10), expand = c(0, 0), limits = c(0, 103)) +
  labs(title = str_wrap("PORCENTAJE DE MUNICIPIOS DE CADA ENTIDAD EN LOS QUE SE INSTALARÁ UNA MESA DE VOTACIÓN DURANTE LA CONSULTA", width = 70), 
       x = NULL, 
       y = "\n% de municipios",
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: mexicodecide.com.mx y Encuesta Intercensal 2015 del INEGI") +
  tema +
  theme(panel.grid.major.y = element_blank())

ggsave(filename = "por_mpos_incluidos_por_edo_consulta.png", path = "03_graficas/", width = 15, height = 10, dpi = 100)



### Gráfica del porcentaje de población que habita en los municipios en los que se instalará una mesa de votación ----

bd %>% 
  mutate(mpo_incluido = ifelse(!is.na(municipios_nom), "Sí", "No")) %>% 
  group_by(edo_nom) %>% 
  mutate(pob_incluidos = ifelse(mpo_incluido == "Sí", pob_tot, 0)) %>% 
  summarise(num_mpos = n(),
            total_mpos_incluidos = sum(mpo_incluido == "Sí"),
            total_pob_edo = sum(pob_tot),
            total_pob_mpos_incluidos = sum(pob_incluidos)) %>% 
  ungroup() %>% 
  mutate(por_mpos_incluidos = round((total_mpos_incluidos/num_mpos)*100, 1),
         por_pob_incluida = round((total_pob_mpos_incluidos/total_pob_edo)*100, 1)) %>% 
  arrange(-por_pob_incluida) %>% 
  ggplot(aes(fct_reorder(edo_nom, por_pob_incluida), por_pob_incluida)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 100, 10), expand = c(0, 0), limits = c(0, 103)) +
  labs(title = str_wrap("PORCENTAJE DE LA POBLACIÓN DE CADA ENTIDAD QUE HABITA EN LOS MUNICIPIOS EN LOS QUE SE INSTALARÁ UNA MESA DE VOTACIÓN DURANTE LA CONSULTA", width = 70), 
       x = NULL, 
       y = "\n% de población",
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: mexicodecide.com.mx y Encuesta Intercensal 2015 del INEGI") +
  tema +
  theme(panel.grid.major.y = element_blank())

ggsave(filename = "por_poblacion_en_mpos_incluidos_por_edo_consulta.png", path = "03_graficas/", width = 15, height = 10, dpi = 100)



### Identificar los municpios que conecentran el 80% de la población nacional y generar archivo .csv ----
bd %>% 
  arrange(-pob_tot) %>% 
  mutate(pob_acumulada = cumsum(pob_tot),
         pob_tot_nal = sum(pob_tot),
         por_pob_acumulada = round((pob_acumulada/pob_tot_nal)*100, 5),
         mpo_incluido = ifelse(!is.na(municipios_nom), "Sí", "No")) %>% 
  select(edo_nom, mpo_nom, 
         pob_tot, 
         pob_acumulada, 
         pob_tot_nal, 
         por_pob_acumulada, 
         mpo_incluido) %>% 
  filter(por_pob_acumulada <=80) %>% 
  print(n = Inf) %>% 
  write_excel_csv("04_datos_output/mpos_80_porciento_poblacion.csv")




### Municipios que forman parte del subuniverso que conecentra el 80% de la población nacional pero en los que no se instalará una casilla de votación en la consulta ----

bd %>% 
  arrange(-pob_tot) %>% 
  mutate(pob_acumulada = cumsum(pob_tot),
         pob_tot_nal = sum(pob_tot),
         por_pob_acumulada = round((pob_acumulada/pob_tot_nal)*100, 5),
         mpo_incluido = ifelse(!is.na(municipios_nom), "Sí", "No")) %>% 
  filter(por_pob_acumulada <=80,
         mpo_incluido == "No") %>% 
  select(edo_nom, mpo_nom, 
         pob_tot, 
         mpo_incluido) %>% 
  mutate(pob_subuniverso_mpos = sum(pob_tot)) %>%
  print(n = Inf) %>% 
  write_excel_csv("04_datos_output/mpos_del_80_porciento_mas_poblado_no_incluidos.csv")



### Gráfica de la distribución por estado de los municpios que a pesar de formar parte del subuniverso que conecentra el 80% de la población nacional, no se instalará una casilla de votación en la consulta ----
bd %>% 
  arrange(-pob_tot) %>% 
  mutate(pob_acumulada = cumsum(pob_tot),
         pob_tot_nal = sum(pob_tot),
         por_pob_acumulada = round((pob_acumulada/pob_tot_nal)*100, 5),
         mpo_incluido = ifelse(!is.na(municipios_nom), "Sí", "No")) %>% 
  select(edo_nom, mpo_nom, 
         pob_tot, 
         pob_acumulada, 
         pob_tot_nal, 
         por_pob_acumulada, 
         mpo_incluido) %>% 
  filter(por_pob_acumulada <=80,
         mpo_incluido == "No") %>% 
  group_by(edo_nom) %>%
  summarise(num_mpos = n()) %>% 
  ungroup() %>% 
  arrange(-num_mpos) %>% 
  mutate(por_mpos_cum = round((cumsum(num_mpos)/sum(num_mpos))*100, 1)) %>% 
  ggplot(aes(fct_reorder(edo_nom, num_mpos), num_mpos)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(breaks = c(seq(0, 10, 5), 13), expand = c(0, 0), limits = c(0, 14)) +
  labs(title = str_wrap("DISTRIBUCIÓN DEL SUBUNIVERSO DE 102 MUNICIPIOS EN LOS QUE NO SE INSTALARÁ UNA MESA DE VOTACIÓN DURANTE LA CONSULTA, A PESAR DE SÍ SER DE LOS 502 MÁS POBLADOS", width = 68), 
       subtitle = str_wrap("Estos 102 municipios forman parte del subuniverso de los 502 más poblados y que conjuntamente conecentra el 80% de la población nacional", width = 120), 
       x = NULL, 
       y = "\nNúm. de   \nmunicipios",
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: mexicodecide.com.mx y Encuesta Intercensal 2015 del INEGI") +
  tema +
  theme(panel.grid.major.y = element_blank())

ggsave(filename = "mpos_del_80_porciento_mas_poblado_no_incluidos.png", path = "03_graficas/", width = 15, height = 10, dpi = 100)


### Cálculo del porcentaje acumulado de municipios que a pesar de formar parte del subuniverso que conecentra el 80% de la población nacional, no se instalará una casilla de votación en la consulta ----

bd %>% 
  arrange(-pob_tot) %>% 
  mutate(pob_acumulada = cumsum(pob_tot),
         pob_tot_nal = sum(pob_tot),
         por_pob_acumulada = round((pob_acumulada/pob_tot_nal)*100, 5),
         mpo_incluido = ifelse(!is.na(municipios_nom), "Sí", "No")) %>% 
  select(edo_nom, mpo_nom, 
         pob_tot, 
         pob_acumulada, 
         pob_tot_nal, 
         por_pob_acumulada, 
         mpo_incluido) %>% 
  filter(por_pob_acumulada <=80,
         mpo_incluido == "No") %>% 
  group_by(edo_nom) %>%
  summarise(num_mpos = n()) %>% 
  ungroup() %>% 
  arrange(-num_mpos) %>% 
  mutate(por_mpos_cum = round((cumsum(num_mpos)/sum(num_mpos))*100, 1)) %>% 
  print(n = Inf)


### Gráfica de la distribución por estado de los municpios que a pesar de NO formar parte del subuniverso que conecentra el 80% de la población nacional, SÍ se instalará una casilla de votación en la consulta ----

bd %>% 
  arrange(-pob_tot) %>% 
  mutate(pob_acumulada = cumsum(pob_tot),
         pob_tot_nal = sum(pob_tot),
         por_pob_acumulada = round((pob_acumulada/pob_tot_nal)*100, 5),
         mpo_incluido = ifelse(!is.na(municipios_nom), "Sí", "No")) %>% 
  select(edo_nom, mpo_nom, 
         pob_tot, 
         pob_acumulada, 
         pob_tot_nal, 
         por_pob_acumulada, 
         mpo_incluido) %>% 
  filter(por_pob_acumulada >=80,
         mpo_incluido == "Sí") %>% 
  group_by(edo_nom) %>%
  summarise(num_mpos = n()) %>% 
  ungroup() %>% 
  arrange(-num_mpos) %>% 
  mutate(suma_acumulada = cumsum(num_mpos), 
         por_mpos_cum = round((cumsum(num_mpos)/sum(num_mpos))*100, 1)) %>% 
  ggplot(aes(fct_reorder(edo_nom, num_mpos), num_mpos)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  # scale_y_continuous(breaks = c(seq(0, 10, 5), 13), expand = c(0, 0), limits = c(0, 14)) +
  labs(title = str_wrap("DISTRIBUCIÓN DEL SUBUNIVERSO DE 138 MUNICIPIOS EN LOS QUE SÍ SE INSTALARÁ UNA MESA DE VOTACIÓN DURANTE LA CONSULTA, A PESAR DE NO SER DE LOS 502 MÁS POBLADOS", width = 68), 
       subtitle = str_wrap("Estos 138 municipios NO forman parte del subuniverso de los 502 más poblados y que conjuntamente conecentra el 80% de la población nacional", width = 120), 
       x = NULL, 
       y = "\nNúm. de   \nmunicipios",
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: mexicodecide.com.mx y Encuesta Intercensal 2015 del INEGI") +
  tema +
  theme(panel.grid.major.y = element_blank())

ggsave(filename = "mpos_incluidos_que_no_son_del_80_porciento_mas_poblado.png", path = "03_graficas/", width = 15, height = 10, dpi = 100)


### Municipios en lo que a pesar de NO formar parte del subuniverso que conecentra el 80% de la población nacional, SÍ se instalará una casilla de votación en la consulta ----

bd %>% 
  arrange(-pob_tot) %>% 
  mutate(pob_acumulada = cumsum(pob_tot),
         pob_tot_nal = sum(pob_tot),
         por_pob_acumulada = round((pob_acumulada/pob_tot_nal)*100, 5),
         mpo_incluido = ifelse(!is.na(municipios_nom), "Sí", "No")) %>% 
  filter(por_pob_acumulada >=80,
         mpo_incluido == "Sí") %>% 
  select(edo_nom, mpo_nom, 
         pob_tot, 
         mpo_incluido) %>%
  mutate(pob_subuniverso_mpos = sum(pob_tot)) %>% 
  print(n = Inf) %>% 
  write_excel_csv("04_datos_output/mpos_incluidos_que_no_son_del_80_porciento_mas_poblado.csv")


### Análisis de la distribución por estado de los municpios que a pesar de formar parte del subuniverso que conecentra el 80% de la población nacional, no se instalará una casilla de votación en la consulta ----

bd %>% 
  arrange(-pob_tot) %>% 
  mutate(pob_acumulada = cumsum(pob_tot),
         pob_tot_nal = sum(pob_tot),
         por_pob_acumulada = round((pob_acumulada/pob_tot_nal)*100, 5),
         mpo_incluido = ifelse(!is.na(municipios_nom), "Sí", "No")) %>% 
  select(edo_nom, mpo_nom, 
         pob_tot, 
         pob_acumulada, 
         pob_tot_nal, 
         por_pob_acumulada, 
         mpo_incluido) %>% 
  filter(por_pob_acumulada >=80,
         mpo_incluido == "Sí") %>% 
  group_by(edo_nom) %>%
  summarise(num_mpos = n()) %>% 
  ungroup() %>% 
  arrange(-num_mpos) %>% 
  mutate(num_mpos_acumulados = cumsum(num_mpos), 
         por_mpos_acumulados = round((cumsum(num_mpos)/sum(num_mpos))*100, 1))
