library(tidyverse)
library(magrittr)
library(purrr)
library(dplyr)
library(ggpubr)
library(seewave)

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

# tengo un problema de encoding con la ñ de columna Año Nuevo
datos %<>% mutate(fecha = case_when(
  fecha != 'Navidad' ~ 'Anio_nuevo',
  fecha == 'Navidad' ~ 'Navidad',
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

# modelo lineal y visualización

m1 <- lm(nivel_sonoro_avg ~ intervalo_min, data = p1_spl)
summary(m1)

m1 %>% 
  ggplot(aes(x = intervalo_min, y = nivel_sonoro_avg)) +
  geom_point() +
  geom_smooth(method = lm)

# calcular media muestral de acuerdo a punto, fecha y condicion
estadistica <- datos %>%
  group_by(punto,fecha) %>%
  summarize(M = 10*log10(sum(10^(nivel_sonoro/10))/n()))

#calcular media muestral de dbs usando seewave
estadistica <- datos %>%
  group_by(punto,fecha) %>%
  summarize(M =meandB(nivel_sonoro, level="IL"))
  
# da el mismo resultado la formulita que meti yo
# que lo que devuelve la biblioteca jeje ;)
desviacion <- datos %>%
  group_by(punto,fecha) %>%
  summarize(S =sddB(nivel_sonoro, level="IL"))

estadistica %<>% inner_join(desviacion)

#calcular desviacion estandar de dbs usando seewave
install.packages("seewave", repos="http://cran.at.r-project.org/")

estadistica <- datos %>%
  group_by(punto,fecha) %>%
  summarize(M = 10*log10(sum(10^(nivel_sonoro/10))/n()))

navidad_p1_stats <- estadistica %>%
  filter('fecha' == 'Navidad')

punto_1 <- estadistica %>%
  filter(punto == 'P1')

histograma <- navidad_p1_sin_pirotecnia %>% 
  with(hist(nivel_sonoro, breaks = c(seq(58,80,by =1)), plot = FALSE)) %$% 
  tibble(from    = head(breaks, -1),
         to      = tail(breaks, -1), 
         mids    = mids,
         counts  = counts, 
         density = density)

ggplot(histograma, aes(x = mids, y = counts)) + geom_col(color='black', fill='white') #width = 0.5)

densidad = tibble(x = seq(from=58, to=80, by=0.1)) %>% mutate(pdf = dnorm(x, mean=66.51844, sd=16.17648))
densidad


ggplot() + geom_col(data = histograma, aes(x = mids, y=density), color='black', fill='white') + #width = 0.5,
  geom_line(data = densidad, aes(x = x, y = pdf), color = 'orange')
