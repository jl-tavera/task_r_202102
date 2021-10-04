#==================================#
#             TALLER R             #
# JOSE LUIS TAVERA RUIZ - 20821999 #
#    UNIVERSIDAD DE LOS ANDES      #
#==================================#

#R version 4.1.0

#==========#
# Taller A #
#==========#


#Configuración Inicial

rm(list = ls()) # limpia el entorno de R
require(pacman)  # Instalar la librería pacman
p_load(tidyverse,rio,skimr,haven,RColorBrewer,ggthemes,hrbrthemes, reshape2, ggplot2, scales) # Llamar y/o instalar las librerías necesarias



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

#3.1) IMPORTAR

#Importamos la base de datos general y mantenemos las columnas de interés
cabe_cg = readRDS("task_1/data/input/2019/Cabecera - Caracteristicas generales (Personas).rds") %>%
          select(.,c("secuencia_p","orden","directorio","P6020", "P6040",
             "P6050", "fex_c_2011", "ESC", "dpto", ))

#Importamos la base de datos de ocupados, generamos la variable de ocupado = 1 
#ya que todas las observaciones son ocupados y dejamos las columnas de interés
#que serán las necesarias para el left_join, el ingreso y la variable creada
cabe_ocu = readRDS("task_1/data/input/2019/Cabecera - Ocupados.rds") %>% mutate(ocupado=1) %>%
           select(., c("secuencia_p","orden","directorio","ocupado", "INGLABO"))

#Importamos la base de datos de desocupados, generamos la variable de desocupado = 1 
#ya que todas las observaciones son desocupados y dejamos las columnas de interés
#que serán las necesarias para el left_join y la variable creada
cabe_des = readRDS("task_1/data/input/2019/Cabecera - Desocupados.rds") %>% mutate(desocupado=1)  %>%
  select(., c("secuencia_p","orden","directorio","desocupado"))

#Importamos la base de datos de inactivos, generamos la variable de inactivos = 1 
#ya que todas las observaciones son inactivos y dejamos las columnas de interés
#que serán las necesarias para el left_join y la variable creada
cabe_inac = readRDS("task_1/data/input/2019/Cabecera - Inactivos.rds") %>% mutate(inactivos=1) %>%
  select(., c("secuencia_p","orden","directorio","inactivos"))

#Importamos la base de datos de fuerza de trabajo, generamos la variable de ftrabajo = 1 
#ya que todas las observaciones son fuerza de trabajo y dejamos las columnas de interés
#que serán las necesarias para el left_join y la variable creada
cabe_ftrabajo = readRDS("task_1/data/input/2019/Cabecera - Fuerza de trabajo.rds") %>% mutate(ftrabajo=1) %>%
  select(., c("secuencia_p","orden","directorio","ftrabajo"))


# Hacemos el merge usando el left join con cada una de las bases y el producto del left join previo
geih = left_join(cabe_cg,cabe_ocu,c("secuencia_p","orden","directorio")) %>%
        left_join(.,cabe_des,c("secuencia_p","orden","directorio")) %>%
        left_join(.,cabe_inac,c("secuencia_p","orden","directorio")) %>%
        left_join(.,cabe_ftrabajo,c("secuencia_p","orden","directorio")) 

#Obteniendo Finalmente una base de datos de 59230 observaciones y 14 variables

#Ahora bien, necesitamos verificar que estamos trabajando con variables numéricas
is_numeric(geih$P6020)
is.numeric(geih$P6040)
is.numeric(geih$P6040)
is.numeric(geih$P6050)
is.numeric(geih$fex_c_2011)
is.numeric(geih$ESC)

# GRÁFICO 1: NÚMERO DE PERSONAS POR CATEGORÍA Y GÉNERO

#Creamos primero un dataframe con las categorías y el total por género
poblacion_sexo = geih %>% 
  group_by(P6020, ocupado, desocupado, inactivos) %>%
  mutate(P6020 = ifelse(test = P6020 == 1, "hombre", "mujer")) %>% 
  summarise(total = table(P6020))

#Esto nos devuelve varias columnas que debemos simplificar a una que llamaremos tipo
df_sexo = data.frame(poblacion_sexo) %>% 
  mutate(tipo = "otro")
  df_sexo$tipo[df_sexo$ocupado == 1] <- "ocupado"
  df_sexo$tipo[df_sexo$desocupado == 1] <- "desocupado"
  df_sexo$tipo[df_sexo$inactivos == 1] <- "inactivo"
  df_sexo = select( df_sexo, c("P6020", "tipo", "total"))

# Hacemos un Cluster Bar Graph para este gráfico que es lo más apropiado
graph_1 = ggplot(df_sexo, aes(fill=P6020, y=total, x=tipo)) + 
    geom_bar(position="dodge", stat="identity") +
    ggtitle("Categorías por Género") +
    scale_fill_discrete(name = "") +
    theme_wsj()
    
graph_1


#GRÁFICO 2: PIE CHART DE CATEGORÍAS

#Hacemos un datafrme con el total por cada categoría 
poblacion_tipo = geih %>%
  mutate(ftrabajo = ifelse(test = is.na(ftrabajo) == TRUE, "0", "1")) %>%
  group_by(ocupado, desocupado, inactivos, ftrabajo) %>%
  summarise(total = table(ftrabajo))

#Análogamente ocurre lo mismo de la vez pasada, debemos resumir todo a una columna
df_tipo = data.frame(poblacion_tipo) %>% 
  mutate(tipo = "otro")
  df_tipo$tipo[df_tipo$ocupado == 1] <- "ocupado"
  df_tipo$tipo[df_tipo$desocupado == 1] <- "desocupado"
  df_tipo$tipo[df_tipo$inactivos == 1] <- "inactivo"
  df_tipo = select( df_tipo, c("tipo", "total"))

# Creamos el Pie Chart
graph_2 = ggplot(df_tipo, aes(x="", y=total, fill=tipo)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  ggtitle("Categorías Pie Chart") +
  scale_fill_brewer(palette="Set1") +
  scale_fill_discrete(name = "") +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(face = "bold")) + 
  theme(plot.title = element_text(size = 20))
  
graph_2

#GRÁFICO 3: DISTRIBUCIÓN DEL INGRESO POR GÉNERO

#Creamos un data frame solo con género e ingreso
poblacion_ingreso_genero = geih %>%
  mutate(P6020 = ifelse(test = P6020 == 1, "hombre", "mujer")) %>% 
  select(., c("P6020", "INGLABO"))

#Eliminamos todos los missing values
poblacion_ingreso_genero <- na.omit(poblacion_ingreso_genero)

#Lo convertimos en dataframe
df_ing_genero = data.frame(poblacion_ingreso_genero)
#Lo sorteamos 
df_ing_genero <- df_ing_genero[order(df_ing_genero$P6020, df_ing_genero$INGLABO),]
#Creamos una variable llamada n, que va a ser nuestro eje x en la gráfica
df_ing_genero = df_ing_genero %>% mutate("n" = 0)

#n indica la posición que tiene el ingreso de la persona en el intervalo de 0 hasta el total de personas
# Para esto iteramos la columna de género y vamos sumando un contador
i = 1
while(df_ing_genero[i, "P6020"] == "hombre" ) { 
  df_ing_genero[i , "n"] <- i
  i <- i + 1
}
#De igual forma hacemos lo mismo para las mujeres desde la última posición de los hombres con otro contador
j = 1
while(df_ing_genero[i, "P6020"] == "mujer" & (i+j) <= 20093) {       
  df_ing_genero[i + j , "n"] <- j + 1
  j <- j + 1
}


#Ploteamos ambas lineas 
graph_3 = ggplot(df_ing_genero, aes(x = n, y = INGLABO)) + 
  geom_line(aes(color = P6020, linetype = P6020)) + 
  scale_color_manual(values = c("darkred", "steelblue")) + 
  scale_y_continuous(name="Ingreso", labels = comma) +
  ggtitle("Distribución del Ingreso por Género") +
  scale_fill_discrete(name = "") 
  
graph_3

#GRÁFICO 4: CATEGORÍAS POR EDAD

#Creamos nuestro dataframe por rango de edad y categorías 
rango_tipo = geih %>% 
  mutate(rango = ifelse(test = P6040 <= 25, "joven",
                        ifelse(test = P6040 >= 25 & P6040 <= 50, "adulto",
                               ifelse(test = P6040 >= 50 & P6040 <= 65, "alta_edad", "retirado"))),
         P6020 = ifelse(test = P6020 == 1, "hombre", "mujer")) %>% 
  mutate(ftrabajo = ifelse(test = is.na(ftrabajo) == TRUE, "0", "1")) %>%
  group_by(rango, ocupado, desocupado, inactivos, ftrabajo) %>% 
  summarise(total = table(rango))

df_rango = data.frame(rango_tipo) %>% 
  mutate(tipo = "otro")
  df_rango$tipo[df_rango$ocupado == 1] <- "ocupado"
  df_rango$tipo[df_rango$desocupado == 1] <- "desocupado"
  df_rango$tipo[df_rango$inactivos == 1] <- "inactivo"
  df_rango = select( df_rango, c("tipo", "total", "rango"))

#De forma similar al primer gráfico ploteamos 
graph_4 = ggplot(df_rango, aes(fill=rango, y=total, x=tipo)) + 
    geom_bar(position="dodge", stat="identity") +
    ggtitle("Categorías por Edad") +
    scale_fill_discrete(name = "") +
    theme_stata()
  
graph_4

#GRÁFICO 5: CATEGORÍAS POR DEPARTAMENTO 

departamento_tipo =  geih %>% 
  group_by(dpto, ocupado, desocupado, inactivos) %>%
  summarise(total = table(dpto))

df_dpto = data.frame(departamento_tipo) %>% 
  mutate(tipo = "otro")
  df_dpto$tipo[df_dpto$ocupado == 1] <- "ocupado"
  df_dpto$tipo[df_dpto$desocupado == 1] <- "desocupado"
  df_dpto$tipo[df_dpto$inactivos == 1] <- "inactivo"
  df_dpto = select( df_dpto, c("dpto", "tipo", "total"))

  graph_5 = ggplot(df_dpto, aes(fill=dpto, y=total, x=tipo)) + 
    geom_bar(position="dodge", stat="identity") +
    ggtitle("Categorías por Departamento") +
    scale_fill_discrete(name = "") +
    theme_fivethirtyeight()
  
graph_5

#GRÁFICO 6: DISPERSIÓN POR ESCOLARIDAD Y CATEGORÍA 

esc_tipo =  geih %>% 
  group_by(ESC, ocupado, desocupado, inactivos) %>%
  summarise(total = table(ESC))
  
df_esc = data.frame(esc_tipo) %>% 
  mutate(tipo = "otro")
  df_esc$tipo[df_esc$ocupado == 1] <- "ocupado"
  df_esc$tipo[df_esc$desocupado == 1] <- "desocupado"
  df_esc$tipo[df_esc$inactivos == 1] <- "inactivo"
  df_esc = select( df_esc, c("ESC", "tipo", "total"))

graph_6 = ggplot(df_esc, aes(x=ESC, y=total, shape=tipo, color= tipo)) +
  geom_point() +
  ggtitle("Categorías por Género") +
  theme_clean() +
  scale_fill_discrete(name = "") 
 

graph_6

# Como no piden ningún análisis sobre las gráficas simplemente faltas exportarlas 

ggsave(plot=graph_1, file = "task_1/views/grafico_1.jpeg")
ggsave(plot=graph_2, file = "task_1/views/grafico_2.jpeg")
ggsave(plot=graph_3, file = "task_1/views/grafico_3.jpeg")
ggsave(plot=graph_4, file = "task_1/views/grafico_4.jpeg")
ggsave(plot=graph_5, file = "task_1/views/grafico_5.jpeg")
ggsave(plot=graph_6, file = "task_1/views/grafico_6.jpeg")



