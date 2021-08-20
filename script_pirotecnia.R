library(tidyverse)

tabla.datos = read.csv("./data/datosR.csv", header = TRUE, sep = ';', stringsAsFactors = TRUE)

a = ggplot(tabla.datos, aes(x=tiempo, y=nivel_sonoro))+
  geom_point()+
  facet_grid(fecha~punto)
a

## Agregar a la tabla una fila que sea "condicion" que sea "con pirotecnia"
## cuando es tiempo es mayor a 3600 y "sin pirotecnia" cuando es menor o igual 
## a 3600.
## usando mutate --- 