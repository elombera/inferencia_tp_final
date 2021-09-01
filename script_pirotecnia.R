library(tidyverse)
library(magrittr)
library(purrr)
library(dplyr)

tabla.datos = read.csv("./data/datosR.csv", header = TRUE, sep = ';', stringsAsFactors = TRUE)

a = ggplot(tabla.datos, aes(x=tiempo, y=nivel_sonoro))+
  geom_point()+
  facet_grid(fecha~punto)
a

## Agregar a la tabla una fila que sea "condicion" que sea "con pirotecnia"
## cuando es tiempo es mayor a 3600 y "sin pirotecnia" cuando es menor o igual 
## a 3600.
## usando mutate --- 
tabla.datos

datos <- tabla.datos %>%
  mutate(condicion = case_when(
    tiempo <= 3600 ~ 'sin_pirotecnia',
    tiempo > 3600 ~ 'con_pirotecnia',
  ))

datos  %<>%  mutate(intervalo_min = case_when(
      tiempo <= 900 ~  15,
      tiempo <= 1800 ~ 30,
      tiempo <= 2700 ~ 45,
      tiempo <= 3600 ~ 60,
      tiempo <= 4500 ~ 75,
      tiempo <= 5400 ~ 90,
      tiempo <= 6300 ~ 105,
      tiempo <= 7200 ~ 120,
    ))

intervalo_spl <- datos %>%
  group_by(punto,fecha,intervalo_min,condicion) %>%
  summarise(nivel_sonoro_avg = 10*log10(sum(10^(nivel_sonoro/10))/n()))

figura_navidad_spl_avg = intervalo_spl %>%
  filter(fecha=='Navidad') %>%
  ggplot( aes(x=intervalo_min,y=nivel_sonoro_avg)) +
  geom_line()+
  facet_wrap(punto~.)+
  labs(x = "Intervalos de 15 minutos",
       y = "SPL",
       title = "SPL promedio cada 15 minutos para Navidad") +
  theme_minimal()

figura_navidad_spl_avg

figura_anio_nuevo_spl_avg = intervalo_spl %>%
  filter(fecha!='Navidad') %>%
  ggplot( aes(x=intervalo_min,y=nivel_sonoro_avg)) +
  geom_line()+
  facet_wrap(punto~.)+
  labs(x = "Intervalos de 15 minutos",
       y = "SPL",
       title = "SPL promedio cada 15 minutos para Año Nuevo") +
  theme_minimal()

figura_anio_nuevo_spl_avg            

install.packages(ggpubr)
