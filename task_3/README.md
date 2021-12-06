**Task 3 - Taller de R**
================
**Jose Luis Tavera Ruiz**

## **Punto 1: Datos Espaciales**

En el punto 1, nos piden utilizar el Marco Geo Estadísitico Nacional -
MGN del DANE para graficar los centros médicos, centros poblados y
ataques por minas antipersonas en el Norte de Santander. Para lo
anterior, nos pedían seguir los siguientes pasos:

### **1.1 Importar Datos Espaciales**

``` r
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
```

### **1.2 Atributos de los Objetos**

``` r
#Sacamos algunas estadísiticas descriptivas de los objetos de 

# Centro Poblado
skim(c_poblado)

# Mapmuse
skim(mapmuse)

# Centro Médico
skim(c_medico)

# El resto de objetos son muy grandes o no tienen estadísiticas descriptivas interesantes
```

## Including Plots

You can also embed plots, for example:

![](README_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
