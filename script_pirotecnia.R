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


table.data <- table.data %>%
  mutate(time_interval = case_when(
    time <= 3600 ~ '60 min',
    time > 3600 ~ '60 min',
  ))
table.data.30 <- table.data %>% mutate(time_interval = case_when(
  time >= 1800 & time <=3600 ~ "30 min",
  time > 3600 & time <=5400 ~ "30 min"
))
table.data.30 <- na.omit(table.data.30)

table.data.15  <- table.data %>% mutate(time_interval = case_when(
  time >= 2700 & time <=3600 ~ "15 min",
  time > 3600 & time <=4500 ~ "15 min"
))
table.data.15 <- na.omit(table.data.15)


table.data.time = merge(table.data.30, table.data.15, all=TRUE)
table.data = merge(table.data, table.data.time, all=TRUE)
rm("table.data.30","table.data.15","table.data.time")

table.data.all.p <- table.data %>%
  group_by(point,condition,celebration,time_interval) %>%
  summarise(spl_avg_p = meandB(spl, level= "IL"),
            sd_avg_p = sddB(spl, level = "IL"),
            L1 = quantile(spl, probs = c(0.99)),
            L10 = quantile(spl, probs = c(0.90)),
            L50 = quantile(spl, probs = c(0.5)),
            L90 = quantile(spl, probs = c(0.1)))

table.data.apra = read.csv("./data/dataAPrA2.csv", header = TRUE, sep = ';', stringsAsFactors = TRUE)
table.data.apra$celebration = "Normal day" 
table.data.apra$condition = "Environmental noise"
table.data.apra$time_interval = "Environmental noise"


table.data.apra.p <- filter(table.data.apra, point != "P10") %>%
  group_by(point, condition, celebration,time_interval) %>%
  summarise(spl_avg_p = meandB(LeqA, level= "IL"),
            sd_avg_p = sddB(LeqA, level = "IL"),
            L1 = meandB(L1, level= "IL"),
            L10 = meandB(L10, level = "IL"),
            L50 = meandB(L50, level= "IL"),
            L90 = meandB(L90, level = "IL"))


table.data.p = merge(table.data.all.p, table.data.apra.p, all=TRUE)
table.data.p             = tibble(table.data.p)
rm("table.data.apra.p","table.data.all.p")

idx = table.data.p$point == "P4" & table.data.p$time_interval =="15 min" & table.data.p$celebration =="Christmas"
table.data.p = table.data.p[!idx,] 


table.data.avg <- table.data %>%
  group_by(time,condition,celebration,time_interval) %>%
  summarise(spl_avg = 10*log10(sum(10^(spl/10))/n()))


cbPalette <- c("red", "blue", "green", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")

fig.1 = ggplot(table.data.avg, aes(x = time, y = spl_avg, color = condition)) + geom_point(alpha = 0.01, show.legend = FALSE)+
  geom_smooth(method = lm, aes(fill=time_interval, linetype = time_interval, color = condition),size = .5,se=FALSE, fullrange=FALSE)+
  geom_hline(yintercept = 66.4, color = "green", alpha = .5, size = 2)+
  
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
                          
fig.1
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Fig1", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=fig.1, width=14, height=7, units="cm", limitsize=FALSE, dpi=600)


table.data.all <- table.data.p %>%
  group_by(condition,time_interval) %>%
  summarise(spl_avg = meandB(spl_avg_p, level= "IL"),
            sd_avg = sddB(spl_avg_p, level = "IL"),
            L1_spl_avg = meandB(L1, level= "IL"),
            L1_spl_sd = sddB(L1, level= "IL"),
            L10_spl_avg = meandB(L10, level= "IL"),
            L50_spl_avg = meandB(L50, level= "IL"),
            L90_spl_avg = meandB(L90, level= "IL"))
table.data.all            = tibble(table.data.all)

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
  
  annotate("text", x = 5.5, y = 76,  label = "***", size = 4) +
  annotate("segment", x = 4, xend = 7, y = 75, yend = 75, colour = "black", size=.5, alpha=1,)+
  annotate("text", x = 6, y = 82,  label = "***", size = 4) +
  annotate("segment", x = 5, xend = 7, y = 81, yend = 81, colour = "black", size=.5, alpha=1,)+
  annotate("text", x = 6.5, y = 88,  label = "***", size = 4) +
  annotate("segment", x = 6, xend = 7, y = 87, yend = 87, colour = "black", size=.5, alpha=1,)+
  
  theme_bw(base_size = 8)+ theme(legend.position= "top",
                                 axis.title.x = element_blank(),
                    legend.title = element_blank(),
                    legend.text = element_text(size = 8))
fig.2Leq
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Fig2", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=fig.2Leq, width=14, height=7, units="cm", limitsize=FALSE, dpi=600)


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

  annotate("text", x = 5.5, y = 82,  label = "***", size = 4) +
  annotate("segment", x = 4, xend = 7, y = 81, yend = 81, colour = "black", size=.5, alpha=1,)+
  annotate("text", x = 6, y = 88,  label = "***", size = 4) +
  annotate("segment", x = 5, xend = 7, y = 87, yend = 87, colour = "black", size=.5, alpha=1,)+
  annotate("text", x = 6.5, y = 94,  label = "***", size = 4) +
  annotate("segment", x = 6, xend = 7, y = 93, yend = 93, colour = "black", size=.5, alpha=1,)+
  
  annotate("text", x = 4, y = 111,  label = "*", size = 4) +
  annotate("segment", x = 1, xend = 7, y = 110, yend = 110, colour = "black", size=.5, alpha=1,)+
  annotate("text", x = 4.5, y = 105,  label = "*", size = 4) +
  annotate("segment", x = 2, xend = 7, y = 104, yend = 104, colour = "black", size=.5, alpha=1,)+

  
  theme_bw(base_size = 8)+ theme(legend.position= "none",
                                 axis.title.x = element_blank(),
                                 legend.title = element_blank(),
                                 legend.text = element_text(size = 8))
fig.2L1
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Fig2L1", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=fig.2L1, width=14, height=7, units="cm", limitsize=FALSE, dpi=600)



Figure1 = ggarrange(fig.1,
                    ggarrange(fig.2Leq, fig.2L1, ncol = 2, labels = c("B", "C"),common.legend = TRUE, legend="bottom", align = "hv"),
                    labels = "A",
                    nrow = 2,
                    align = "hv")
Figure1
ggsave("figures/FIGURE1.png", plot=Figure1, width = 17, height = 17, units = "cm", dpi=600, limitsize=FALSE,bg = "white")  




# Stadistic analyasis -------------------

m.SPL<- lm(spl_avg_p ~ condition*time_interval*celebration, 
           data = table.data.p)
summary(m.SPL)
summ(m.SPL)
anova(m.SPL)


t.test(filter(table.data.p,
              condition =="Environmental noise")$spl_avg_p,
       filter(table.data.p,
              condition =="With fireworks" & time_interval =="15 min")$spl_avg_p,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$spl_avg_p,
       filter(table.data.p,
              condition =="With fireworks" & time_interval =="30 min")$spl_avg_p,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$spl_avg_p,
       filter(table.data.p,
              condition =="With fireworks" & time_interval =="60 min")$spl_avg_p,
       paired = FALSE)



t.test(filter(table.data.p,
              condition =="Environmental noise")$spl_avg_p,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="15 min")$spl_avg_p,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$spl_avg_p,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="30 min")$spl_avg_p,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$spl_avg_p,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="60 min")$spl_avg_p,
       paired = FALSE)


t.test(filter(table.data.p,
              condition =="With fireworks" & time_interval =="15 min")$spl_avg_p,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="15 min")$spl_avg_p,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="With fireworks" & time_interval =="15 min")$spl_avg_p,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="30 min")$spl_avg_p,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="With fireworks" & time_interval =="15 min")$spl_avg_p,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="60 min")$spl_avg_p,
       paired = FALSE)



t.test(filter(table.data.p,
              condition =="With fireworks" & time_interval =="30 min")$spl_avg_p,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="15 min")$spl_avg_p,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="With fireworks" & time_interval =="30 min")$spl_avg_p,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="30 min")$spl_avg_p,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="With fireworks" & time_interval =="30 min")$spl_avg_p,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="60 min")$spl_avg_p,
       paired = FALSE)





t.test(filter(table.data.p,
              condition =="With fireworks" & time_interval =="60 min")$spl_avg_p,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="15 min")$spl_avg_p,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="With fireworks" & time_interval =="60 min")$spl_avg_p,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="30 min")$spl_avg_p,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="With fireworks" & time_interval =="60 min")$spl_avg_p,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="60 min")$spl_avg_p,
       paired = FALSE)



m.L1<- lm(L1 ~ condition*time_interval*celebration, 
          data = table.data.p)
summary(m.L1)
summ(m.L1)
anova(m.L1)


t.test(filter(table.data.p,
              condition =="Environmental noise")$L1,
       filter(table.data.p,
              condition =="With fireworks" & time_interval =="15 min")$L1,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$L1,
       filter(table.data.p,
              condition =="With fireworks" & time_interval =="30 min")$L1,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$L1,
       filter(table.data.p,
              condition =="With fireworks" & time_interval =="60 min")$L1,
       paired = FALSE)




t.test(filter(table.data.p,
              condition =="Environmental noise")$L1,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="15 min")$L1,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$L1,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="30 min")$L1,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$L1,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="60 min")$L1,
       paired = FALSE)






t.test(filter(table.data.p,
              condition =="With fireworks" & time_interval =="15 min")$L1,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="15 min")$L1,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="With fireworks" & time_interval =="15 min")$L1,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="30 min")$L1,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="With fireworks" & time_interval =="15 min")$L1,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="60 min")$L1,
       paired = FALSE)



t.test(filter(table.data.p,
              condition =="With fireworks" & time_interval =="30 min")$L1,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="15 min")$L1,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="With fireworks" & time_interval =="30 min")$L1,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="30 min")$L1,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="With fireworks" & time_interval =="30 min")$L1,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="60 min")$L1,
       paired = FALSE)





t.test(filter(table.data.p,
              condition =="With fireworks" & time_interval =="60 min")$L1,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="15 min")$L1,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="With fireworks" & time_interval =="60 min")$L1,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="30 min")$L1,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="With fireworks" & time_interval =="60 min")$L1,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="60 min")$L1,
       paired = FALSE)

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

supplementary.fig1
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

