
#===================================#
#             TALLER R              #
# JOSE LUIS TAVERA RUIZ - 201821999 #
#    UNIVERSIDAD DE LOS ANDES       #
#===================================#

#R version 4.1.0

#==========#
# Taller A #
#==========#

# Configuración Inicial 

rm(list=ls())
if (!require("pacman")) install.packages("pacman") # Instalar pacman (sino está instalada)
require(pacman) # Correr la LIbrería de Pacman 
p_load(rio,tidyverse,viridis,sf,leaflet,osmdata,ggsn,skimr,ggmap,tidycensus,raster,maps) # llamar y/o instalar librerias
if(sessionInfo()$loadedOnly$Rcpp$Version!="1.0.7") update.packages("Rcpp") # Para la librería OSM necesitamos la versión 1.0.7 de Rcpp 
Sys.setlocale("LC_CTYPE", "en_US.UTF-8") # Encoding UTF-8


#==========================#
# Punto 1 Datos Espaciales #
#==========================#

# 1.1. Importar datos espaciales

# 1.1.1 De la carpeta data/outpu importe los shapefiles de VIAS (llame al objeto via) 
# y MGN_URB_TOPONIMIA (llame al objeto puntos). 

via = st_read("task_3/data/input/VIAS.shp")
puntos = st_read("task_3/data/input/MGN_URB_TOPONIMIA.shp")

#1.1.2 Cree un nuevo objeto llamado c_medico, que contenga las observaciones del
#objeto puntos en las que la variable CSIMBOL sea igual a "021001" "021002" o "021003".

c_medico = puntos %>% filter(  CSIMBOL == "021001" 
                             | CSIMBOL == "021002" 
                             | CSIMBOL ==  "021003" )

#1.1.3 De la carpeta data/output importe los c poblado (2017).rds (llame al objeto c poblado).

c_poblado = readRDS("task_3/data/input/c poblado (2017).rds")

# 1.1.3 dp deptos (2017).rds (llame al objeto depto) 

depto = readRDS("task_3/data/input/dp deptos (2017).rds")

# y victimas_map-muse.rds (llame al objeto mapmuse).

mapmuse = readRDS("task_3/data/input/victimas_map-muse.rds")

# Asegúrese de dejar únicamente código DANE >= 54001 & < 55000 

c_poblado = c_poblado %>% filter( cod_dane >= 54001, cod_dane < 55000 )
mapmuse = mapmuse %>% filter( cod_mpio >= 54001, cod_mpio < 55000 )

#y el polígono de Norte de Santander.

depto = depto %>% filter( cod_dpto == 54)

# 1.2 Atributos de los Objetos 

#Sacamos algunas estadísiticas descriptivas de los objetos de 

# Centro Poblado
skim(c_poblado)

# Mapmuse
skim(mapmuse)

# Centro Médico
skim(c_medico)

# El resto de objetos son muy grandes o no tienen estadísiticas descriptivas interesantes

# 1.3 Geometrías del Objeto 

# 1.3.1 Para todos los objetos del punto 1.1., pinte sobre la consola la caja de 
# coordenadas (st _bbox) y el CRS de cada objeto.

vars <- list(via, puntos, c_medico, c_poblado, depto, mapmuse)
vars_names <- list('via', 'puntos', 'c_medico', 'c_poblado', 'depto' ,'mapmuse')
i = 1

for(var in vars){
  print(vars_names[i])
  print('Caja de Coordenadas')
  print(var  %>%  st_bbox())
  print('CRS')
  print(var  %>%  st_crs())
  print('')
  i <- i + 1 
}

# 1.3.2 Ahora va a re proyectar el CRS de todos los objetos. Asigne la siguiente 
# CRS+proj=utm+zone=19 +datum=WGS84 +units=m +no_ defs a todos los objetos del punto 1.1..



