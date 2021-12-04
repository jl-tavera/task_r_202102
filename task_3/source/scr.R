
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
require(pacman) 
p_load(rio,tidyverse,viridis,sf,leaflet,osmdata,ggsn,skimr,ggmap,tidycensus) # llamar y/o instalar librerias

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

c_medico = puntos %>% filter(CSIMBOL == "021001" | CSIMBOL == "021002" |CSIMBOL ==  "021003" )

#1.1.3 De la carpeta data/output importe los c poblado (2017).rds (llame al objeto c poblado).
# Asegúrese de dejar únicamente código DANE >= 54001 & < 55000 y el polígono de Norte de Santander.



