library(tidyverse)
library(leaflet) # en el mapa
library(magrittr)
library(sp) # char2dms
library(htmltools) # para el popup on focus




mapa.puntos = read.csv("./data/puntosEs.csv", header = TRUE, sep = ';', stringsAsFactors = TRUE)

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

beatCol <- colorFactor(palette = c("blue","red"), mapa.puntos$Fecha)


labelPunto <- paste(sep=" ",'Punto', mapa.puntos$Posicion , mapa.puntos$Barrio...Comuna)


map <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = mapa.puntos, lat = ~lat, lng = ~lng,
                   color = ~beatCol(Fecha),
                   label = labelPunto)%>%
  addLegend("bottomright", pal = beatCol, values = mapa.puntos$Fecha,
            title = "Puntos de medida",
            opacity = .5)



map

