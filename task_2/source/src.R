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
require(pacman)
p_load(rio,tidyverse)


#===============#
# Punto 1 Loops #
#===============#

#1.0. Crear lista: Cree un objeto tipo lista vacío, llámelo chip.

chip = list() #usamos el comando de list() para generar una lista vacía

#1.1. Importar datos: Use un loop para importar cada archivo .xlsx de data/imput
# en un elemento/posición diferente de chip.


# Para lo anterior podemos usar la función de list.files para obtener el nombre 
# de los archivos dentro de un directorio con el argumento de full names marcado 
# como verdadero
#  
# Lo anterior lo podemos hacer de dos formas: 
#   1. Usar un lapply para simplificar el loop (rutas)
#      Asimismo, podemos usar unlist() para convertir la lista en un vector y 
#      trabajar de forma más eficiente
#
#   2. Hacer el loop con un for o while


rutas = lapply(2017:2020 , function(paths) list.files(paste0("task_2/data/imput/",paths),full.names=T)) %>%
        unlist()

#Finalmente para importar los datos, solo debemos recorrer el vector rutas en un  
# for usando la función de import y guardarlo en la lista chip, terminand con los 
# 80 elementos mencionados en el enunciado 

for (i in 1:length(rutas)){
     chip[[i]] = import(file = rutas[i])  
}  

#====================#
# Punto 2 Funciones  #
#====================#

#2.0. Crear función: Cree una función que extraiga de un dataframe 
# dentro de chip, el valor PAGOS(Pesos) para la categoría EDUCACION. 

pagos = function(n,lista,tipo){
  lista_n = lista[[n]] 
  colnames(lista_n) = lista_n[7,]
valor = lista_n %>% subset(NOMBRE==tipo) %>% select(`PAGOS(Pesos)`)
return(valor)  
}
f_extrac(n = 10 , lista = list_chip , tipo = "EDUCACIÓN")


# Completando la funcion
f_extrac = function(n,lista,tipo){
  
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









