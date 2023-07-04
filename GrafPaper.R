library(tidyverse)
library(magrittr)
library(purrr)
library(dplyr)
library(ggpubr)
library(seewave)
library(measurements)
library(stringr)
library(sp)
library(leaflet)
library(pBrackets) 

rm(list=ls())

load("./data.RData")

figures_folder = "figures"
cbPalette <- c("red", "blue", "green", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442","black","yellow","orange","lightgreen","lightblue","red" )


# FIGURE 1 ----
mapa.puntos = read.csv("./data/puntosEs2.csv", header = TRUE, sep = ';', stringsAsFactors=TRUE, fileEncoding="latin1")

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


df.20 <- mapa.puntos

getColor <- function(mapa.puntos) {
  sapply(mapa.puntos$Fecha, function(Fecha) {
    if(Fecha == "Christmas/New Year") {
      "red"
    } else if(Fecha == "Christmas") {
      "darkred"
    } else if(Fecha == "New Year") {
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


groups <- c("Christmas/New Year" <- "<div style='position: relative; display: inline-block' class='awesome-marker-icon-red awesome-marker'><i class='ion ion-close icon-red '></i></div>Christmas/New Year",
            "Christmas" <- "<div style='position: relative; display: inline-block' class='awesome-marker-icon-darkred awesome-marker'><i class='ion ion-close icon-darkred '></i></div>Christmas",
            "New Year" <- "<div style='position: relative; display: inline-block' class='awesome-marker-icon-lightred awesome-marker'><i class='ion ion-close icon-lightred '></i></div>New Year",
            "Normal Day" <- "<div style='position: relative; display: inline-block' class='awesome-marker-icon-green awesome-marker'><i class='ion ion-close icon-green '></i></div>Normal Day")


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
  addLayersControl(                                                                                                           
    overlayGroups = groups,
    options = layersControlOptions(collapsed = FALSE)
  )
  # addControl(html_legend, position = "topright")
  
  # addLegendAwesomeIcon()
# addControl(icons, position = "bottomleft")
  # addProviderTiles(providers$Stamen.Toner)
  # addProviderTiles(providers$CartoDB.Positron)
  # addProviderTiles(providers$Esri.NatGeoWorldMap)
map
#750 x 600
mapshot(map, file = "figures/Rplot.png")


mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Fig1", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=map, width=14, height=7, units="cm", limitsize=FALSE, dpi=600)

#CHILE

mapa.puntos.chile = read.csv("./data/puntosEsC.csv", header = TRUE, sep = ';', stringsAsFactors=TRUE, fileEncoding="latin1")

chd = substr(mapa.puntos.chile$Ubicacion,3,3)[1]
chm = substr(mapa.puntos.chile$Ubicacion,6,6)[1]
chs = substr(mapa.puntos.chile$Ubicacion,9,9)[1]

lat = char2dms(as.character(mapa.puntos.chile$Latitud), chd="°",chm="\'",chs="\"")
mapa.puntos.chile$lat <- c(as.numeric(lat))

lng =  char2dms(as.character(mapa.puntos.chile$Longitud), chd="°",chm="\'",chs="\"")
mapa.puntos.chile$lng <- c(as.numeric(lng))

mapa.puntos.chile %<>% mutate(
  lat = case_when(
    lat < 0 ~ lat,
    lat > 0 ~ lat * -1
  )
)


df.20 <- mapa.puntos.chile

getColor <- function(mapa.puntos.chile) {
  sapply(mapa.puntos.chile$Fecha, function(Fecha) {
    if(Fecha == "New Year") {
      "lightred"
    } else {
      "blue"
    } })
}

icons <- awesomeIcons(
  icon = "ion-close",
  iconColor = "black",
  library = "ion",
  markerColor = getColor(mapa.puntos.chile)
)


groups <- c("New Year" <- "<div style='position: relative; display: inline-block' class='awesome-marker-icon-lightred awesome-marker'><i class='ion ion-close icon-lightred '></i></div>New Year",
            "Torre Entel" <- "<div style='position: relative; display: inline-block' class='awesome-marker-icon-blue awesome-marker'><i class='ion ion-close icon-blue '></i></div>Torre Entel")

map <- leaflet(mapa.puntos.chile) %>%
  addTiles() %>%
  addAwesomeMarkers(~lng, ~lat, icon=icons, label=~as.character(Fecha))%>%
  #addProviderTiles(providers$Esri.WorldStreetMap) %>%
  addMiniMap(
    #tiles = providers$Esri.WorldStreetMap,
    width = 120,
    height = 120,
    zoomLevelOffset = -10,
    toggleDisplay = TRUE)%>%
  addLayersControl(                                                                                                           
    overlayGroups = groups,
    options = layersControlOptions(collapsed = FALSE)
  )
# addControl(html_legend, position = "topright")

# addLegendAwesomeIcon()
# addControl(icons, position = "bottomleft")
# addProviderTiles(providers$Stamen.Toner)
# addProviderTiles(providers$CartoDB.Positron)
# addProviderTiles(providers$Esri.NatGeoWorldMap)
map

mapshot(map, file = "figures/Rplot.png")




# FIGURE 2 -------------------------------

table.data.avg <- table.data %>%
  group_by(time,condition,celebration,time_interval) %>%
  summarise(spl_avg = 10*log10(sum(10^(spl/10))/n()))

fig.2a = ggplot(table.data.avg, aes(x = time, y = spl_avg, color = condition)) + geom_point(alpha = 0.01, show.legend = FALSE)+
  geom_smooth(method = lm, aes(fill=time_interval, linetype = time_interval, color = condition),size = .5,se=FALSE, fullrange=FALSE)+
  # geom_hline(yintercept = 66.4, color = "green", alpha = .5, size = 2)+
  
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) +       
  facet_grid(.~celebration)+
  labs(y = "Sound Presure Level [dB]",
       x = "Time [sec]") +
  guides(color="none", fill = guide_legend("time_interval"))+
  theme_bw(base_size = 8)+ theme(legend.position = c(.99, .99),
                                 legend.justification = c("right", "top"),
                                 legend.title = element_blank(),
                                 legend.text = element_text(size = 8))

fig.2a
# mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Fig1", ".png", sep = '')
# ggsave(mi_nombre_de_archivo, plot=fig.1a, width=14, height=7, units="cm", limitsize=FALSE, dpi=600)


# table.data.all <- table.data.p %>%
#   group_by(condition,time_interval) %>%
#   summarise(spl_avg = meandB(spl_avg_p, level= "IL"),
#             sd_avg = sddB(spl_avg_p, level = "IL"),
#             L1_spl_avg = meandB(L1, level= "IL"),
#             L1_spl_sd = sddB(L1, level= "IL"),
#             L10_spl_avg = meandB(L10, level= "IL"),
#             L1_spl_sd = sddB(L10, level= "IL"),
#             L50_spl_avg = meandB(L50, level= "IL"),
#             L1_spl_sd = sddB(L50, level= "IL"),
#             L90_spl_avg = meandB(L90, level= "IL"),
#             L1_spl_sd = sddB(L90, level= "IL"))
# table.data.all            = tibble(table.data.all)

table.data.all$condition = factor(table.data.all$condition, levels= c("With fireworks","Without fireworks","Environmental noise"))


fig.2Leq <- ggplot(table.data.all, aes(x = interaction(time_interval,condition), y = spl_avg, fill = condition, color = condition)) +
  geom_pointrange(aes(x = interaction(time_interval,condition), y = spl_avg, ymin=spl_avg-sd_avg,
                      ymax=spl_avg+sd_avg, fill = condition),
                  size = 1,shape = 2,
                  position=position_jitter(width=.01, height=0)) +
  geom_jitter(data = table.data.p, mapping = aes(x = interaction(time_interval,condition), y = spl_avg_p, fill = condition, color = condition),
              position=position_jitter(width=.08, height=0),alpha = .7,size =0.7)+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) +       
  scale_x_discrete(name="Interval of time in celebration days vs Normal day ", labels=c("15 min","30 min","60 min","15 min","30 min","60 min","Normal day"))+
  labs(y = "Equivalent Continuous\nSound Pressure Level LeqA [dBA]") +
  ylim(35,112)+

  annotate("segment", x = 0.8, xend = 3.2, y = 87, yend = 87, colour = "black", size=.5, alpha=1,)+
  annotate("segment", x = 0.8, xend = 0.8, y = 84, yend = 87, colour = "black", size=.5, alpha=1,)+
  annotate("segment", x = 3.2, xend = 3.2, y = 84, yend = 87, colour = "black", size=.5, alpha=1,)+
  
  annotate("segment", x = 2, xend = 2, y = 87, yend = 92, colour = "black", size=.5, alpha=1,)+
  annotate("segment", x = 2, xend = 5, y = 92, yend = 92, colour = "black", size=.5, alpha=1,)+
  annotate("segment", x = 5, xend = 5, y = 87, yend = 92, colour = "black", size=.5, alpha=1,)+
  
  annotate("segment", x = 3.8, xend = 6.2, y = 87, yend = 87, colour = "black", size=.5, alpha=1,)+
  annotate("segment", x = 3.8, xend = 3.8, y = 84, yend = 87, colour = "black", size=.5, alpha=1,)+
  annotate("segment", x = 6.2, xend = 6.2, y = 84, yend = 87, colour = "black", size=.5, alpha=1,)+
  
  annotate("text", x = 3.5, y = 94,  label = "***", size = 4) +
  

  annotate("segment", x = 3.8, xend = 6.2, y = 72, yend = 72, colour = "black", size=.5, alpha=1,)+
  annotate("segment", x = 3.8, xend = 3.8, y = 69, yend = 72, colour = "black", size=.5, alpha=1,)+
  annotate("segment", x = 6.2, xend = 6.2, y = 69, yend = 72, colour = "black", size=.5, alpha=1,)+
  
  
  annotate("segment", x = 5, xend = 5, y = 72, yend = 77, colour = "black", size=.5, alpha=1,)+
  annotate("segment", x = 5, xend = 7, y = 77, yend = 77, colour = "black", size=.5, alpha=1,)+
  annotate("segment", x = 7, xend = 7, y = 72, yend = 77, colour = "black", size=.5, alpha=1,)+
  
  annotate("text", x = 6, y = 79,  label = "***", size = 4) +
  # annotate("text", x = 5.5, y = 76,  label = "***", size = 4) +
  # annotate("segment", x = 4, xend = 7, y = 75, yend = 75, colour = "black", size=.5, alpha=1,)+
  # annotate("text", x = 6, y = 82,  label = "***", size = 4) +
  # annotate("segment", x = 5, xend = 7, y = 81, yend = 81, colour = "black", size=.5, alpha=1,)+
  # annotate("text", x = 6.5, y = 88,  label = "***", size = 4) +
  # annotate("segment", x = 6, xend = 7, y = 87, yend = 87, colour = "black", size=.5, alpha=1,)+
  # 
  theme_bw(base_size = 8)+ theme(legend.position= "top",
                                 axis.title.x = element_blank(),
                                 legend.title = element_blank(),
                                 legend.text = element_text(size = 8))
fig.2Leq


# mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Fig2", ".png", sep = '')
# ggsave(mi_nombre_de_archivo, plot=fig.2Leq, width=14, height=7, units="cm", limitsize=FALSE, dpi=600)


fig.2L1 <- ggplot(table.data.all, aes(x = interaction(time_interval,condition), y = L1_spl_avg, fill = condition, color = condition)) +
  geom_pointrange(aes(x = interaction(time_interval,condition), y = L1_spl_avg, ymin=L1_spl_avg-L1_spl_sd,
                      ymax=L1_spl_avg+L1_spl_sd, fill = condition),
                  size = 1,shape = 2,
                  position=position_jitter(width=.01, height=0)) +
  geom_jitter(data = table.data.p, mapping = aes(x = interaction(time_interval,condition), y = L1, fill = condition, color = condition),
              position=position_jitter(width=.08, height=0),alpha = .7,size =0.7)+
  scale_x_discrete(name="Interval of time in celebration days vs Normal day ", labels=c("15 min","30 min","60 min","15 min","30 min","60 min","Normal day"))+
  labs(y = "Statistical noise levels L01 [dB]", size = 1) +
  ylim(35,112)+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) +  
  
  annotate("segment", x = 0.8, xend = 3.2, y = 95, yend = 95, colour = "black", size=.5, alpha=1,)+
  annotate("segment", x = 0.8, xend = 0.8, y = 92, yend = 95, colour = "black", size=.5, alpha=1,)+
  annotate("segment", x = 3.2, xend = 3.2, y = 92, yend = 95, colour = "black", size=.5, alpha=1,)+
  
  annotate("segment", x = 2, xend = 2, y = 95, yend = 100, colour = "black", size=.5, alpha=1,)+
  annotate("segment", x = 2, xend = 5, y = 100, yend = 100, colour = "black", size=.5, alpha=1,)+
  annotate("segment", x = 5, xend = 5, y = 95, yend = 100, colour = "black", size=.5, alpha=1,)+
  
  annotate("segment", x = 3.8, xend = 6.2, y = 95, yend = 95, colour = "black", size=.5, alpha=1,)+
  annotate("segment", x = 3.8, xend = 3.8, y = 92, yend = 95, colour = "black", size=.5, alpha=1,)+
  annotate("segment", x = 6.2, xend = 6.2, y = 92, yend = 95, colour = "black", size=.5, alpha=1,)+
  
  annotate("text", x = 3.5, y = 102,  label = "***", size = 4) +
  
  
  annotate("segment", x = 3.8, xend = 6.2, y = 80, yend = 80, colour = "black", size=.5, alpha=1,)+
  annotate("segment", x = 3.8, xend = 3.8, y = 77, yend = 80, colour = "black", size=.5, alpha=1,)+
  annotate("segment", x = 6.2, xend = 6.2, y = 77, yend = 80, colour = "black", size=.5, alpha=1,)+
  
  
  annotate("segment", x = 5, xend = 5, y = 80, yend = 85, colour = "black", size=.5, alpha=1,)+
  annotate("segment", x = 5, xend = 7, y = 85, yend = 85, colour = "black", size=.5, alpha=1,)+
  annotate("segment", x = 7, xend = 7, y = 80, yend = 85, colour = "black", size=.5, alpha=1,)+
  
  annotate("text", x = 6, y = 87,  label = "***", size = 4) +
  
  
  
  annotate("segment", x = 0.8, xend = 2.2, y = 107, yend = 107, colour = "black", size=.5, alpha=1,)+
  annotate("segment", x = 0.8, xend = 0.8, y = 105, yend = 107, colour = "black", size=.5, alpha=1,)+
  annotate("segment", x = 2.2, xend = 2.2, y = 105, yend = 107, colour = "black", size=.5, alpha=1,)+
  
  
  annotate("segment", x = 1.5, xend = 1.5, y = 107, yend = 110, colour = "black", size=.5, alpha=1,)+
  annotate("segment", x = 1.5, xend = 7, y = 110, yend = 110, colour = "black", size=.5, alpha=1,)+
  annotate("segment", x = 7, xend = 7, y = 107, yend = 110, colour = "black", size=.5, alpha=1,)+
  
  annotate("text", x = 4, y = 112,  label = "*", size = 4) +
  
  # annotate("text", x = 5.5, y = 82,  label = "***", size = 4) +
  # annotate("segment", x = 4, xend = 7, y = 81, yend = 81, colour = "black", size=.5, alpha=1,)+
  # annotate("text", x = 6, y = 88,  label = "***", size = 4) +
  # annotate("segment", x = 5, xend = 7, y = 87, yend = 87, colour = "black", size=.5, alpha=1,)+
  # annotate("text", x = 6.5, y = 94,  label = "***", size = 4) +
  # annotate("segment", x = 6, xend = 7, y = 93, yend = 93, colour = "black", size=.5, alpha=1,)+
  # 
  # annotate("text", x = 4, y = 111,  label = "*", size = 4) +
  # annotate("segment", x = 1, xend = 7, y = 110, yend = 110, colour = "black", size=.5, alpha=1,)+
  # annotate("text", x = 4.5, y = 105,  label = "*", size = 4) +
  # annotate("segment", x = 2, xend = 7, y = 104, yend = 104, colour = "black", size=.5, alpha=1,)+
  # 
  
  theme_bw(base_size = 8)+ theme(legend.position= "none",
                                 axis.title.x = element_blank(),
                                 legend.title = element_blank(),
                                 legend.text = element_text(size = 8))
fig.2L1
# mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Fig2L1", ".png", sep = '')
# ggsave(mi_nombre_de_archivo, plot=fig.2L1, width=14, height=7, units="cm", limitsize=FALSE, dpi=600)



Figure3 = ggarrange(fig.2a,
                    ggarrange(fig.2Leq, fig.2L1, ncol = 2, labels = c("B", "C"),common.legend = TRUE, legend="bottom", align = "hv"),
                    labels = "A",
                    nrow = 2,
                    align = "hv")
Figure3
ggsave("figures/FIGURE3.png", plot=Figure3, width = 17, height = 17, units = "cm", dpi=600, limitsize=FALSE,bg = "white")  





# FIGURE 3 ------
table.data.avg.arg <- filter(table.data.comparision, country == "Argentina") %>%
  group_by(time,condition,country) %>%
  summarise(Leq = meandB(Leq, level = "IL"))
table.data.avg.arg$point = "mean"

table.data.avg.chi.l <- filter(table.data.comparision, country == "Chile", point == "P1" | point == "P4") %>%
  group_by(time,condition,country) %>%
  summarise(Leq = meandB(Leq, level = "IL"))
table.data.avg.chi.l$point = "P1 and P4"

table.data.avg.chi.c <- filter(table.data.comparision, country == "Chile", point == "P2" | point == "P3") %>%
  group_by(time,condition,country) %>%
  summarise(Leq = meandB(Leq, level = "IL"))
table.data.avg.chi.c$point = "P2 and P3"



table.data.comparision3 = merge(table.data.avg.chi.c,table.data.avg.arg,all=TRUE)

table.data.comparision2 = merge(filter(table.data.comparision3, country == "Chile"), table.data.avg.chi.l,all=TRUE)
fig.3a = ggplot(table.data.comparision2, aes(x = time, y = Leq, color = condition)) + geom_point(alpha = 1, show.legend = T)+
  geom_smooth(method = lm, aes(fill=interaction(condition, point), linetype = point, color = condition),size = .5,se=FALSE, fullrange=FALSE)+
  #geom_hline(yintercept = 66.4, color = "green", alpha = .5, size = 2)+
  # geom_line(aes(fill = point))+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) +       
  #facet_grid(.~celebration)+
  labs(y = "Sound Presure Level [dB]",
       x = "Time [sec]") +
  #guides(color="none", fill = guide_legend("time_interval"))+
  theme_bw(base_size = 8)+ theme(legend.position = c(.99, .99),
                                 legend.justification = c("right", "top"),
                                 legend.title = element_blank(),
                                 legend.text = element_text(size = 8))

fig.3a
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Fig1", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=fig.1a, width=14, height=7, units="cm", limitsize=FALSE, dpi=600)


# FI
# Supplementary figure ----
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

supplementary.fig1
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "supplementary.fig1", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=supplementary.fig1, width=10, height=25, units="cm", limitsize=FALSE, dpi=600)

