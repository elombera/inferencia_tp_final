library(tidyverse)
library(magrittr)
library(purrr)
library(dplyr)
library(ggpubr)
library(seewave)



rm(list=ls())
figures_folder = "figures"
table.data = read.csv("./data/dataR.csv", header = TRUE, sep = ';', stringsAsFactors = TRUE)

table.data <- table.data %>%
  mutate(condition = case_when(
    time <= 3600 ~ 'Without fireworks',
    time > 3600 ~ 'With fireworks',
  ))

table.data.all.p <- table.data %>%
  group_by(point,condition,celebration) %>%
  summarise(spl_avg_p = meandB(spl, level= "IL"),
            sd_avg_p = sddB(spl, level = "IL"))


table.data.apra = read.csv("./data/dataAPrA.csv", header = TRUE, sep = ';', stringsAsFactors = TRUE)
table.data.apra$celebration = "Normal day" 
table.data.apra$condition = "Environmental noise"

table.data.apra.p <- table.data.apra %>%
  group_by(point, condition, celebration) %>%
  summarise(spl_avg_p = meandB(LeqA, level= "IL"),
            sd_avg_p = sddB(LeqA, level = "IL"))


table.data.p = merge(table.data.all.p, table.data.apra.p, all=TRUE)

rm("table.data.apra.p","table.data.all.p")


table.data.avg <- table.data %>%
  group_by(time,condition,celebration) %>%
  summarise(spl_avg = 10*log10(sum(10^(spl/10))/n()))

fig.1 = ggplot(table.data.avg, aes(x = time, y = spl_avg, color = condition)) + geom_point(alpha = 0.01)+
        geom_smooth(method = lm, aes(fill=condition),se=TRUE, fullrange=FALSE)+
        facet_grid(.~celebration)+
        labs(y = "Sound Presure Level [dB]",
             x = "Time [min]") +
          theme_bw()+ theme(legend.position= "top", 
                          legend.title = element_blank(),
                          legend.text = element_text(size = 8))
                          
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Fig1", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=fig.1, width=14, height=7, units="cm", limitsize=FALSE, dpi=600)


 
table.data.all <- table.data.p %>%
  group_by(condition) %>%
  summarise(spl_avg = meandB(spl_avg_p, level= "IL"),
            sd_avg = sddB(spl_avg_p, level = "IL"))


fig.2 <- ggplot(table.data.all, aes(x = condition, y = spl_avg, fill = condition, color = condition)) +
                geom_pointrange(aes(x = condition, y = spl_avg, ymin=spl_avg-sd_avg,
                                    ymax=spl_avg+sd_avg, fill = condition),
                                size = 1,shape = 2,
                                position=position_jitter(width=.01, height=0)) +
  geom_jitter(data = table.data.p, mapping = aes(x = condition, y = spl_avg_p, fill = condition, color = condition),
              position=position_jitter(width=.08, height=0))+
  labs(y = "Sound Presure Level [dB]",
       x = "Celebration") +
  
  theme_bw()+ theme(legend.position= "top", 
                    legend.title = element_blank(),
                    legend.text = element_text(size = 8))
fig.2
ggsave("Figuras/LeqAS.png", plot=fig.LeqAS, width = 15, height = 10, units = "cm", dpi=600, limitsize=FALSE) 


# datos  %<>%  mutate(intervalo_min = case_when(
#       tiempo <= 900 ~  15,
#       tiempo <= 1800 ~ 30,
#       tiempo <= 2700 ~ 45,
#       tiempo <= 3600 ~ 60,
#       tiempo <= 4500 ~ 75,
#       tiempo <= 5400 ~ 90,
#       tiempo <= 6300 ~ 105,
#       tiempo <= 7200 ~ 120,
#     ))

supplementary.fig1= ggplot(table.data, aes(x = time, y = spl, color = condition)) + geom_point(alpha = 0.005)+
  geom_smooth(method = lm, aes(fill=condition),se=TRUE, fullrange=FALSE)+
  facet_grid(point~celebration)+
  theme_bw()+ theme(legend.position=c(1,1), legend.title = element_blank(),
                    legend.key.size = .5 )+
  labs(y = "Sound Presure Level [dB]",
       x = "Time [min]") +
  
  theme_bw()+ theme(legend.position= "top", 
                    legend.title = element_blank(),
                    legend.text = element_text(size = 8))

mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "supplementary.fig1", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=supplementary.fig1, width=10, height=25, units="cm", limitsize=FALSE, dpi=600)



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
p1_spl = intervalo_spl
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

install.packages('lme4')

mapa.puntos = read.csv("./data/puntosEs.csv", header = TRUE, sep = ';', stringsAsFactors=TRUE, fileEncoding="latin1")


install.packages('measurements')
library(measurements)
library(stringr)
library(sp)
install.packages('leaflet')
library(leaflet)

mapa.puntos %<>% mutate(lat = dms2dd(Latitud,NS=True))

mapa.puntos %<>%mutate(lat= Ubicacion)

mapa.puntos %<>% mutate(lat = as.character.DMS(Ubicacion))

chd = substr(mapa.puntos$Ubicacion,3,3)[1]
chm = substr(mapa.puntos$Ubicacion,6,6)[1]
chs = substr(mapa.puntos$Ubicacion,9,9)[1]

lat = char2dms(as.character(mapa.puntos$Latitud), chd="°",chm="\'",chs="\"")
mapa.puntos$lat <- c(as.numeric(lat))

lng =  char2dms(as.character(mapa.puntos$Longitud), chd="°",chm="\'",chs="\"")
mapa.puntos$lng <- c(as.numeric(lng))

mapa.puntos %<>% mutate(
  lat = case_when(
    lat < 0 ~ lat,
    lat > 0 ~ lat * -1
  )
)

dms2dd(mapa.puntos$Longitud, ns =True)
mapa.puntos$Longitud(mapa.puntos$Posición == 1)


df.20 <- mapa.puntos

getColor <- function(mapa.puntos) {
  sapply(mapa.puntos$Fecha, function(Fecha) {
    if(Fecha == "N/AN") {
      "red"
    } else if(Fecha == "Navidad") {
      "darkred"
    } else if(Fecha == "Año nuevo") {
      "lightred"
    } else {
      "green"
    } })
}

icons <- awesomeIcons(
  icon = "ion-close",
  iconColor = "black",
  library = "ion",
markerColor = getColor(mapa.puntos)
)


leaflet(mapa.puntos) %>% addTiles() %>%
  addAwesomeMarkers(~lng, ~lat, icon=icons, label=~as.character(Fecha))





map <- leaflet(mapa.puntos) %>%
  addTiles() %>%
  addAwesomeMarkers(~lng, ~lat, icon=icons, label=~as.character(Fecha),
                    popup = mapa.puntos$Barrio...Comuna)%>%
  #addProviderTiles(providers$Esri.WorldStreetMap) %>%
  addMiniMap(
        #tiles = providers$Esri.WorldStreetMap,
        width = 120,
        height = 120,
        zoomLevelOffset = -10,
    toggleDisplay = TRUE)%>%
#addProviderTiles(providers$Stamen.Toner)
#addProviderTiles(providers$CartoDB.Positron)
addProviderTiles(providers$Esri.NatGeoWorldMap)
map

