#==================================#
#             TALLER R             #
# JOSE LUIS TAVERA RUIZ - 20821999 #
#    UNIVERSIDAD DE LOS ANDES      #
#==================================#

#R version 4.1.0

#==========#
# Taller A #
#==========#

# Configuración Inicial 

rm(list=ls())
require(pacman)
p_load(rio,tidyverse)


#===============#
# Punto 1 Loops #
#===============#

#1.0. Crear lista: Cree un objeto tipo lista vacío, llámelo chip.

chip = list()

#1.1. Importar datos: Use un loop para importar cada archivo .xlsx de data/imput
# en un elemento/posición diferente de chip.

paths = lapply(2017:2020 , function(x) list.files(paste0("task_2/data/imput/",x),full.names=T)) %>%
        unlist()


for (i in 1:length(paths)){
     chip[[i]] = import(file = paths[i])  
}

#====================#
# Punto 2 Funciones  #
#====================#

#2.0. Crear función: Cree una función que extraiga de un dataframe 
# dentro de chip, el valor PAGOS(Pesos) para la categoría EDUCACION. 

f_extrac = function(n,lista,tipo_rubro){
  lista_n = lista[[n]] 
  colnames(lista_n) = lista_n[7,]
valor = lista_n %>% subset(NOMBRE==tipo_rubro) %>% select(`PAGOS(Pesos)`)
return(valor)  
}
f_extrac(n = 10 , lista = list_chip , tipo_rubro = "SALUD")


# Completando la funcion
f_extrac = function(n,lista,tipo_rubro){
  
  # crear df
  df = data.frame(valor=NA,cod_dane=NA,periodo=NA)  
  lista_n = lista[[n]] 
  
  # extraer codigo dane
  df$cod_dane = colnames(lista_n)[1]
  
  # extraer periodo
  df$periodo = lista_n[2,1]
  
  # extraer el valor
  colnames(lista_n) = lista_n[7,]
  df$valor = lista_n %>% subset(NOMBRE==tipo_rubro) %>% select(`PAGOS(Pesos)`)

return(df)  
}
f_extrac(n = 10 , lista = list_chip , tipo_rubro = "SALUD")

#========================#
# Punto 3 Familia Apply  #
#========================#

# 3.0. Aplique la función creada en el punto anterior a todos los elementos 
# de la lista chip.









