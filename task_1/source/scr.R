#==================================#
#             TALLER R             #
# JOSE LUIS TAVERA RUIZ - 20821999 #
#    UNIVERSIDAD DE LOS ANDES      #
#==================================#

#==========#
# Taller A #
#==========#


#Funciones Iniciales
rm(list=ls())
require(pacman)
p_load(tidyverse,rio,skimr)



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

# renombrar datos
colnames(data) = data[4,] %>% tolower()

# covertir codmpio en numerica
data$codmpio = as.numeric(data$codmpio)

# eliminar NA
data = data %>% drop_na(codmpio)

# convertir variables en numericas
years = 1999:2007 %>% as.character()
for(var in years){
    data[,var] = as.numeric(data[,var])
}

# pivotear los datos
pivot = data %>% pivot_longer(!coddepto:municipio,names_to="year",values_to="ha_cultivos")

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




