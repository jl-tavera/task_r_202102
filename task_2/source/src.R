#===================================#
#             TALLER R              #
# JOSE LUIS TAVERA RUIZ - 201821999 #
#    UNIVERSIDAD DE LOS ANDES       #
#===================================#

#R version 4.1.0

#==========#
# Taller B #
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
# for usando la función de import y guardarlo en la lista chip, terminand0 con los 
# 80 elementos mencionados en el enunciado 

for (i in 1:length(rutas)){
     chip[[i]] = import(file = rutas[i])  
}  

#====================#
# Punto 2 Funciones  #
#====================#

#2.0. Crear función: Cree una función que extraiga de un dataframe 
# dentro de chip, el valor PAGOS(Pesos) para la categoría EDUCACION. 


# Analizando los archivos de los dataframe, todos tienen el mismo formato
# con los nombres de las columnas en la fila 7 y el valor de PAGOS(Pesos) en la columna 8
# el código DANE como primer nombre de columna y el periodo en la posición (2,1)

pagos = function(sublist,lista,tipo){
  #Extraemos un dataframe en específico en la posición 'n' de chip
  lista_n = sublist
  
  # Creamos el dataframe con los tres datos que nos piden 
  df = data.frame(valor=NA,cod_dane=NA,periodo=NA)  
  
  #Asimismo,  extraemos el codigo dane
  df$cod_dane = colnames(lista_n)[1]
  
  # Y por ultimo, el periodo
  df$periodo = lista_n[2,1]
  
  
  # Definimos los nombres de las columnas que se encuentraan la fila 7
  colnames(lista_n) = lista_n[7,]
  
  # filtramos por el tipo (que dejamos como argumento y podemos llenar por EDUCACIÓN)
  # Seleccionamos finalmente el valor correspondiente a PAGOS(Pesos)
  df$valor = lista_n %>% subset(NOMBRE==tipo) %>% select(`PAGOS(Pesos)`)

  # retornamos el dataframe
return(df)  
}

#========================#
# Punto 3 Familia Apply  #
#========================#

# 3.0. Aplique la función creada en el punto anterior a todos los elementos 
# de la lista chip.

# Lo anterior lo podemos hacer con la función de lapply que permite aplicar una función
# a todos los elementos pertenecientes a nuestra lista, en nuestro caso esta lista es chip

valores = lapply(chip, function(x) pagos(x, lista = chip, tipo = 'EDUCACIÓN' ) ) 

# Para lo anterior fue necesario modificar el argumento de n de pagos, ya que 
# por si solo el lapply itera todos los elementos de la lista por lo que no necesita
# la posición de la lista para extraer cada uno de los archivos

#============================#
# Extra Dataframe de la Info #
#============================#

# Ahora bien, lo ideal sería organizar toda la información en un dataframe
#ya que la función lapply devuelve una lista de dataframes donde se aplicó la función

df_final = data.frame(municipio = NA,cod_dane=NA,periodo=NA, valor=NA)  

for (i in 1:length(valores)) {
  
  sub_df = valores[[i]]
  ciudad_str = strsplit(sub_df$cod_dane, split = "-")[[1]]  %>%  unlist()
  df_final[nrow(df_final) + 1 ,] = c(ciudad_str[[2]],  ciudad_str[[1]], sub_df$periodo, sub_df$valor)   

}

df_final <- df_final[-1,]










