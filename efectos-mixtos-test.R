library(tidyverse)
library(magrittr)
library(purrr)
library(dplyr)
library(ggpubr)
library(nlme)

tabla.datos = read.csv("./data/datosR.csv", header = TRUE, sep = ';', stringsAsFactors = TRUE)

# tengo un problema de encoding con la ñ de columna Año Nuevo
tabla.datos %<>% mutate(fecha = case_when(
  fecha != 'Navidad' ~ 'Anio_nuevo',
  fecha == 'Navidad' ~ 'Navidad',
))

a = ggplot(tabla.datos, aes(x=tiempo, y=nivel_sonoro))+
  geom_point()+
  facet_grid(fecha~punto)
a

## Agregar a la tabla una fila que sea "condicion" que sea "con pirotecnia"
## cuando es tiempo es mayor a 3600 y "sin pirotecnia" cuando es menor o igual 
## a 3600.
## usando mutate --- 
tabla.datos

tabla.datos %<>%
  mutate(condicion = case_when(
    tiempo <= 3600 ~ 'sin_pirotecnia',
    tiempo > 3600 ~ 'con_pirotecnia',
  ))



tabla.datos.lm <- lm(nivel_sonoro ~ punto, data = tabla.datos)


beta  = tabla.datos.lm$coefficients["(Intercept)"]
summary( tabla.datos.lm)
sigma = summary(tabla.datos.lm)$sigma

residuo_datos <- tabla.datos %>% mutate(res = resid(tabla.datos.lm)) %>% tibble()
p2 = ggplot(residuo_datos, aes(x = res, y = tabla.datos$punto)) + geom_boxplot()
p2


Rail_datos %<>% mutate(res2 = resid(fm2Rail.lm))
p3 = ggplot(Rail_datos, aes(x = res2, y = Rail)) + geom_boxplot()
p3



pirotecnia.lme <- lme(nivel_sonoro ~ fecha*condicion, data = tabla.datos, random = ~ 1|punto)

summary(pirotecnia.lme)

residuo_datos2 <- tabla.datos %>% mutate(res = resid(pirotecnia.lme )) %>% tibble()
p3 = ggplot(residuo_datos2, aes(x = res, y = tabla.datos$punto)) + geom_boxplot()
p3
tabla
asd <- tabla.datos %>% 
  filter(punto == 'P1')

tiempo = asd$tiempo
  
mutate(pred = predict(pirotecnia.lme, level=1), res = resid(pirotecnia.lme, level=1))

data_m1.lm <- tabla.datos %>% mutate(pred = predict(pirotecnia.lme), res = resid(pirotecnia.lme))

figura2 = ggplot(tabla.datos, aes(x=tiempo, y=nivel_sonoro))+
  geom_point(alpha = 0.3)+
  facet_grid(fecha~punto)+
  theme_pubr(base_size = 12, margin = TRUE)+
  scale_x_continuous(name= "Tiempo [seg]", breaks=c(0,3500,7000), labels=c("0","3k5","7k")) +
  labs(y = "Nivel de presión [dBA]") +
  theme(legend.position = "top",
        legend.title = element_blank())+
  geom_line(data=data_m1.lm, aes(x=tiempo, y=pred, group=punto), color='white')

figura2
