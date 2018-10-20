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
