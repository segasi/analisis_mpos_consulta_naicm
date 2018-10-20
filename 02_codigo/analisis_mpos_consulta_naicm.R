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


