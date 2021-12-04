
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
p_load(rio,
       tidyverse,
       viridis,
       sf,
       leaflet,
       osmdata,
       ggsn,
       skimr,
       ggmap,
       tidycensus,
       raster,
       maps,
       pixiedust,
       outreg,
       broom, # tidy-coefficients
       knitr, # for kable
       mfx, # marginal effects
       margins,  # marginal effects
       estimatr, # robust standard errors
       lmtest, # HAC (Newey-West) standard errors
       fixest, # hdfe regressions (feols)
       modelsummary, # Coefplot with modelplot
       stargazer) # export tables to latex
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
# CRS a todos los objetos del punto 1.1..



sf_df1 = st_as_sf(x = c_medico, coords = 'geometry', crs = "+proj=utm +zone=19 +datum=WGS84 +units=m +no_ defs")
sf_df2 = st_as_sf(x = c_poblado, coords = 'geometry', crs = "+proj=utm +zone=19 +datum=WGS84 +units=m +no_ defs")
sf_df3 = st_as_sf(x = mapmuse, coords = 'geometry', crs = "+proj=utm +zone=19 +datum=WGS84 +units=m +no_ defs")
sf_df4 = st_as_sf(x = depto, coords = 'geometry', crs = "+proj=utm +zone=19 +datum=WGS84 +units=m +no_ defs")
sf_df5 = st_as_sf(x = via, coords = 'geometry', crs = "+proj=utm +zone=19 +datum=WGS84 +units=m +no_ defs")


leaflet() %>% addTiles() %>% 
  addCircleMarkers(data = sf_df1, color = 'purple') %>%
  addCircleMarkers(data = sf_df3, color = 'red')  %>%
  addPolygons(data=sf_df2, weight = 0, fillColor = "yellow")  %>%
  addPolygons(data=sf_df4, weight = 0, fillColor = "green", opacity	= 0.5)  %>%
  addPolygons(data=sf_df5, weight = 2, fillColor = "blue")  

#Operaciones Geométricas

# 1.4.1 Use el objeto depto para hacer cliping y dejar los puntos de mapmuse que están 
# debajo del polígono de Norte de Santander.



#1.4.2 Del objeto c poblado, seleccione cualquier municipio, use este polígono y el objeto via,
# para calcular el largo de las vías en el centro poblado que seleccionó.

# Pintar Mapas


#1.5.1 Use la función leaflet para visualizar en un mismo mapa: los polígonos de 
# los centros poblados,  el polígono del departamento de Norte de Santander y los
# hospitales y puestos de salud del objeto _medicos.

# 1.5.2 Use las librerías ggplot, ggsn y las demás que considere necesarias para visualizar en un mismo mapa:


#=====================#
# Punto 2 Regresiones #
#=====================#

#2.1. Importe el archivo data/outpu/df_mapmuse.rds  

mapmuse_reg = readRDS("task_3/data/input/victimas_map-muse.rds")

#estime un modelo de probabilidad lineal en el que fallecido es la variable dependiente.
#Y use las demás variables como variables explicativas.

# Generamos la variable dicótoma del estado 1 si fallece, 0 de lo contrario
mapmuse_reg = mapmuse_reg %>% mutate(estado = ifelse(estado=='Muerto',1,0))

# Para la regresión del modelo de probabilidad lineal, es necesario enfatizar dos puntos: 
#   1. Al tratarse de  variables no continuas, es necesaria tratarlas a todas como categóricas
#   2. Incluir los codigos municipales (aunque sea como variable categórica daña la regresión)


ols = lm(estado ~ as.factor(tipo_accidente) 
                        + as.factor(year)  
                        + as.factor(month)
                        + as.factor(condicion)
                        + as.factor(genero)
                        + as.factor(actividad),              
                 data = mapmuse_reg) 

#Almacene los resultados de la estimación en un objeto llamado ols.

ols %>% summary()

# 2.2. Exporte a la carpeta views los gráficos con los coeficientes (coef-plot) de las estimaciones.

# Usamos la librería de pixiedust para crear una regr

dust(ols) %>% 
  sprinkle(cols = c("estimate", "std.error", "statistic"), round = 2) %>%
  sprinkle(cols = "p.value", fn = quote(pvalString(value))) %>% 
  sprinkle_colnames("Term", "Coefficient", "SE", "T-statistic", "P-value")


export_ols <- outreg(ols, digits = 3)
cat(as.matrix(export_ols) , file = 'task_3/views/Reg/ols.tex')


#2.3. Ahora estime la ecuación del punto 2.1. usando un modelo logit y un modelo probit, almacene los
# resultados de las estimaciones en dos objetos llamados logit y probit respectivamente.

logit = glm(estado ~ as.factor(tipo_accidente) 
           + as.factor(year)  
           + as.factor(month)
           + as.factor(condicion)
           + as.factor(genero)
           + as.factor(actividad),              
           data = mapmuse_reg, 
           family = binomial(link="logit")) 


probit = glm(estado ~ as.factor(tipo_accidente) 
            + as.factor(year)  
            + as.factor(month)
            + as.factor(condicion)
            + as.factor(genero)
            + as.factor(actividad),              
            data = mapmuse_reg, 
            family = binomial(link="probit")) 

# 2.4. Exporte los resultados de los tres modelos en una misma tabla usando la función outreg.


stargazer(ols,
          probit,
          logit,
          type= 'text',
          df = FALSE,
          digits = 3, 
          out = paste0('task_3/views/Reg/models.text'))

# De forma alternativa podemos hacer lo mismo con la función de outreg

export_models <- outreg(list(ols, probit, logit), digits = 3)
cat(as.matrix(export_models) , file = 'task_3/views/Reg/models.tex')

# 2.5. De los objetos logit y probit y exporte a la carpeta views dos gráficos con el efecto marginal 
# de la distancia a un centro medico sobre la probabilidad de fallecer.




#=======================#
# Punto 3 Web-Scrapping #
#=======================#

#3.1. Desde la consola de Rstudio lea la siguiente url https://es. wikipedia.org/wiki/Departamentos_de_Colombia
# y cree un objeto que contenga el HTML de la página como un objeto tml_document.


# 3.2. Use el rpath para extraer el título de la página (Departamentos de Colombia).


# 3.3. Extraiga la tabla que contiene los departamentos de Colombia.


#======================#
# Referencias & Ayudas #
#======================#

browseURL('https://www.r-bloggers.com/2014/07/clipping-spatial-data-in-r/')
browseURL('https://stackoverflow.com/questions/30147756/exporting-r-regression-summary-for-publishable-paper')
browseURL('https://rdrr.io/cran/rockchalk/man/outreg.html')

