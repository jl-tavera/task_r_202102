#==================================#
#             TALLER R             #
# JOSE LUIS TAVERA RUIZ - 20821999 #
#    UNIVERSIDAD DE LOS ANDES      #
#==================================#

#==========#
# Taller A #
#==========#


#Configuración Inicial

rm(list = ls()) # limpia el entorno de R
require(pacman)  # Instalar la librería pacman
p_load(tidyverse,rio,skimr,haven) # Llamar y/o instalar las librerías necesarias



#=========#
# Punto 1 #
#=========#

#Cree vectores que contengan los números del 1 al 100, posteriormente cree otro
#vector que contenga los números impares del 1 al 99 y use el vector de numeros 
#impares para crear un vector con los numeros pares del primer vector

#Para crear el vector de todos los numeros del 1 al 100 que denominaremos totales
#Usaremos la función seq que crea un vector con una secuencia, en este caso numérica
#Los tres argumentos son el límite inferior, superior y la suma

totales = seq(1,100,1) 

#Para verificar si es vector y su tipo usamos las funciones is.atomic y typeof
print(is.atomic(totales))
print(typeof(totales))


#Análogamente construimos el vector de impares de la forma 2n + 1 con la función seq

impares = seq(1,99,2)

print(is.atomic(impares))
print(typeof(impares))

#Ahora bien, para crear el vector de Pares podemos usar los totales con la condición
#de que se haga drop a todos los elementos que están ya en impares, ya que un número
# solo puede ser par o impar, para esto se usan los operadoress "!" e "%in%"

pares = totales[!totales  %in% impares]

#=========#
# Punto 2 #
#=========#

# Importe la base de datos de cultivos que se encuentra en la carpeta de data/input
# limpie la base de datos eliminando las observaciones que no tienen informacion relevante
# Luego pivotee la base de datos para que quede en formato long


# 1) iImportamos los datos correspondientes con la función de import

data = import("task_1/data/input/cultivos.xlsx")

#Lo anterior cargará una base de datos de 359 obs y 25 variable

# 2) Limpiemos la base de datos 


# 2.1) renombrar datos
colnames(data) = data[4,] %>% tolower()

# 2.2) Para poder leer el codmpio convirtamolo a numerico
data$codmpio = as.numeric(data$codmpio)

# 2.3) eliminar NA que no tienen información alguna, las filas de totales y las 
#primeras 4 filas que no tenían información 

data = data %>% drop_na(codmpio)


# 3) Pivotear los datos
#Pivotear longer significa disminuir el numero de columnas y aumentar el numero de filas.
#Como no se específica la variable lo haremos con las columnas de años enlistadas en filas

# convertir variables en numericas todos los años hasta 2019

years = 1999:2019 %>% as.character()
for(var in years){
  data[,var] = as.numeric(data[,var])
}

pivot = data %>% pivot_longer(!coddepto:municipio,names_to="year",values_to="ha_cultivos")

#Incluso adicionalmente como muchos municipios no tienen cultivos en algunos años 
#Se podría dropear estos NA

pivot = pivot %>% drop_na(ha_cultivos)

#=========#
# Punto 3 #
#=========#

# punto 3
cabe_cg = readRDS("task_1/data/input/2019/Cabecera - Caracteristicas generales (Personas).rds")

cabe_ocu = readRDS("task_1/data/input/2019/Cabecera - Ocupados.rds") %>% mutate(ocupado=1)

cabe_des = readRDS("task_1/data/input/2019/Cabecera - Desocupados.rds") %>% mutate(desocupado=1)

# unir datos
geih = left_join(cabe_cg,cabe_ocu,c("secuencia_p","orden","directorio")) %>%
        left_join(.,cabe_des,c("secuencia_p","orden","directorio")) 

skim(geih)




