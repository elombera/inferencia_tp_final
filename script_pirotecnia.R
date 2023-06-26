library(tidyverse)
library(magrittr)
library(purrr)
library(dplyr)
library(ggpubr)
library(seewave)

rm(list=ls())

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
idx = table.data.p$point == "P4" & table.data.p$time_interval =="15 min" & table.data.p$celebration =="Christmas"
table.data.p = table.data.p[!idx,] 
table.data.p             = tibble(table.data.p)
rm("table.data.apra.p","table.data.all.p")




table.data.prom = read.csv("./data/dataR.csv", header = TRUE, sep = ';', stringsAsFactors = TRUE)
table.data.avg.prom <- table.data.prom %>%
  group_by(point,time) %>%
  summarise(Leq = meandB(spl, level= "IL"),
            L5 = quantile(spl, probs = c(0.95)),
            L10 = quantile(spl, probs = c(0.90)),
            L50 = quantile(spl, probs = c(0.5)),
            L90 = quantile(spl, probs = c(0.1)))



table.data.comparision = data.frame("Leq" = 0, "time" = 0, "point" = 0)

Leq = replicate(115,0)
table.prom = as.data.frame(Leq)

table.prom$time = c(1:115)
factor_tiempo = factor(table.prom$time)
niveles_tiempo = levels(factor_tiempo)

factor_point = factor(table.data.avg.prom$point)
point_p = levels(factor_point)
i = 60
for(p in 1:length(point_p)) {
  for(t in 1:length(niveles_tiempo)) {
    table.prom$point[t] = point_p[p]
    table.prom$time[t] = t
    aux = table.data.avg.prom %>% filter(point == point_p[p])
    calculo = aux$Leq[(i-60):i]
    table.prom$Leq[t] = meandB(calculo)
    calculo = aux$L5[(i-60):i]
    table.prom$L5[t] = meandB(calculo)
    calculo = aux$L10[(i-60):i]
    table.prom$L10[t] = meandB(calculo)
    calculo = aux$L50[(i-60):i]
    table.prom$L50[t] = meandB(calculo)
    calculo = aux$L90[(i-60):i]
    table.prom$L90[t] = meandB(calculo)
    i=i+60
  }
  i= 60
  table.data.comparision = merge(table.data.comparision, table.prom, all=TRUE)
}

idx = table.data.comparision$point == 0
table.data.comparision = table.data.comparision[!idx,]
table.data.comparision <- table.data.comparision %>%
  mutate(condition = case_when(
    time >= 0 & time <=60 ~ 'Without fireworks',
    time > 60 & time <=120 ~ 'With fireworks',
  ))

table.data.comparision$country = "Argentina"

table.data.chile = read.csv("./data/dataCHILE.csv", header = TRUE, sep = ';', stringsAsFactors = TRUE)
table.data.chile$country = "Chile"

table.data.comparision = merge(table.data.comparision, table.data.chile, all=TRUE)

save(table.data, table.data.p, table.data.comparision, file = 'data.RData')


#old ----

# table.data.chile = read.csv("./data/dataCHILE.csv", header = TRUE, sep = ';', stringsAsFactors = TRUE)
# table.data.chile$country = "Chile"
# table.data.all.chile = table.data.chile %>%
#   group_by(point,condition,country) %>%
#   summarise(Leq = meandB(Leq, level= "IL"),
#             L5 = meandB(L5, level= "IL"),
#             L10 = meandB(L10, level= "IL"),
#             L50 = meandB(L50, level= "IL"),
#             L90 = meandB(L90, level= "IL"))
# 
# 
# 
# table.data.arg = read.csv("./data/dataR.csv", header = TRUE, sep = ';', stringsAsFactors = TRUE)
# table.data.arg$country = "Argentina"
# table.data.arg <- table.data.arg %>%
#   mutate(condition = case_when(
#     time >= 2400 & time <=3600 ~ 'Without fireworks',
#     time > 3600 & time <=4800 ~ 'With fireworks',
#   ))
# table.data.arg = na.omit(table.data.arg)
# 
# table.data.all.arg <- table.data.arg %>%
#   group_by(point,condition,country) %>%
#   summarise(Leq = meandB(spl, level= "IL"),
#             L5 = quantile(spl, probs = c(0.95)),
#             L10 = quantile(spl, probs = c(0.90)),
#             L50 = quantile(spl, probs = c(0.5)),
#             L90 = quantile(spl, probs = c(0.1)))
# 
# table.data.compare = merge(table.data.all.arg, table.data.all.chile, all=TRUE)
# rm("table.data.all.arg", "table.data.all.chile")


# # FIGURE 2
# 
# table.data.avg <- table.data %>%
#   group_by(time,condition,celebration,time_interval) %>%
#   summarise(spl_avg = 10*log10(sum(10^(spl/10))/n()))
# 
# fig.1a = ggplot(table.data.avg, aes(x = time, y = spl_avg, color = condition)) + geom_point(alpha = 0.01, show.legend = FALSE)+
#   geom_smooth(method = lm, aes(fill=time_interval, linetype = time_interval, color = condition),size = .5,se=FALSE, fullrange=FALSE)+
#   geom_hline(yintercept = 66.4, color = "green", alpha = .5, size = 2)+
#   
#   scale_colour_manual(values = cbPalette) + 
#   scale_fill_manual(values = cbPalette) +       
#   facet_grid(.~celebration)+
#         labs(y = "Sound Presure Level [dB]",
#              x = "Time [sec]") +
#   guides(color="none", fill = guide_legend("time_interval"))+
#           theme_bw(base_size = 8)+ theme(legend.position = c(.99, .99),
#                                          legend.justification = c("right", "top"),
#                           legend.title = element_blank(),
#                           legend.text = element_text(size = 8))
#                           
# fig.1a
# mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Fig1", ".png", sep = '')
# ggsave(mi_nombre_de_archivo, plot=fig.1a, width=14, height=7, units="cm", limitsize=FALSE, dpi=600)
# 
# 
# table.data.all <- table.data.p %>%
#   group_by(condition,time_interval) %>%
#   summarise(spl_avg = meandB(spl_avg_p, level= "IL"),
#             sd_avg = sddB(spl_avg_p, level = "IL"),
#             L1_spl_avg = meandB(L1, level= "IL"),
#             L1_spl_sd = sddB(L1, level= "IL"),
#             L10_spl_avg = meandB(L10, level= "IL"),
#             L50_spl_avg = meandB(L50, level= "IL"),
#             L90_spl_avg = meandB(L90, level= "IL"))
# table.data.all            = tibble(table.data.all)
# 
# table.data.all$condition = factor(table.data.all$condition, levels= c("With fireworks","Without fireworks","Environmental noise"))
# 
# fig.2Leq <- ggplot(table.data.all, aes(x = interaction(time_interval,condition), y = spl_avg, fill = condition, color = condition)) +
#                 geom_pointrange(aes(x = interaction(time_interval,condition), y = spl_avg, ymin=spl_avg-sd_avg,
#                                     ymax=spl_avg+sd_avg, fill = condition),
#                                 size = 1,shape = 2,
#                                 position=position_jitter(width=.01, height=0)) +
#   geom_jitter(data = table.data.p, mapping = aes(x = interaction(time_interval,condition), y = spl_avg_p, fill = condition, color = condition),
#               position=position_jitter(width=.08, height=0),alpha = .7,size =0.7)+
#   scale_colour_manual(values = cbPalette) + 
#   scale_fill_manual(values = cbPalette) +       
#   scale_x_discrete(name="Interval of time in celebration days vs Normal day ", labels=c("15 min","30 min","60 min","15 min","30 min","60 min","Normal day"))+
#   labs(y = "Equivalent Continuous\nSound Pressure Level LeqA [dBA]") +
#   ylim(35,112)+
#   
#   annotate("text", x = 5.5, y = 76,  label = "***", size = 4) +
#   annotate("segment", x = 4, xend = 7, y = 75, yend = 75, colour = "black", size=.5, alpha=1,)+
#   annotate("text", x = 6, y = 82,  label = "***", size = 4) +
#   annotate("segment", x = 5, xend = 7, y = 81, yend = 81, colour = "black", size=.5, alpha=1,)+
#   annotate("text", x = 6.5, y = 88,  label = "***", size = 4) +
#   annotate("segment", x = 6, xend = 7, y = 87, yend = 87, colour = "black", size=.5, alpha=1,)+
#   
#   theme_bw(base_size = 8)+ theme(legend.position= "top",
#                                  axis.title.x = element_blank(),
#                     legend.title = element_blank(),
#                     legend.text = element_text(size = 8))
# fig.2Leq
# mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Fig2", ".png", sep = '')
# ggsave(mi_nombre_de_archivo, plot=fig.2Leq, width=14, height=7, units="cm", limitsize=FALSE, dpi=600)
# 
# 
# fig.2L1 <- ggplot(table.data.all, aes(x = interaction(time_interval,condition), y = L1_spl_avg, fill = condition, color = condition)) +
#   geom_pointrange(aes(x = interaction(time_interval,condition), y = L1_spl_avg, ymin=L1_spl_avg-L1_spl_sd,
#                       ymax=L1_spl_avg+L1_spl_sd, fill = condition),
#                   size = 1,shape = 2,
#                   position=position_jitter(width=.01, height=0)) +
#   geom_jitter(data = table.data.p, mapping = aes(x = interaction(time_interval,condition), y = L1, fill = condition, color = condition),
#               position=position_jitter(width=.08, height=0),alpha = .7,size =0.7)+
#   scale_x_discrete(name="Interval of time in celebration days vs Normal day ", labels=c("15 min","30 min","60 min","15 min","30 min","60 min","Normal day"))+
#   labs(y = "Statistical noise levels L01 [dB]", size = 1) +
#   ylim(35,112)+
#   scale_colour_manual(values = cbPalette) + 
#   scale_fill_manual(values = cbPalette) +  
# 
#   annotate("text", x = 5.5, y = 82,  label = "***", size = 4) +
#   annotate("segment", x = 4, xend = 7, y = 81, yend = 81, colour = "black", size=.5, alpha=1,)+
#   annotate("text", x = 6, y = 88,  label = "***", size = 4) +
#   annotate("segment", x = 5, xend = 7, y = 87, yend = 87, colour = "black", size=.5, alpha=1,)+
#   annotate("text", x = 6.5, y = 94,  label = "***", size = 4) +
#   annotate("segment", x = 6, xend = 7, y = 93, yend = 93, colour = "black", size=.5, alpha=1,)+
#   
#   annotate("text", x = 4, y = 111,  label = "*", size = 4) +
#   annotate("segment", x = 1, xend = 7, y = 110, yend = 110, colour = "black", size=.5, alpha=1,)+
#   annotate("text", x = 4.5, y = 105,  label = "*", size = 4) +
#   annotate("segment", x = 2, xend = 7, y = 104, yend = 104, colour = "black", size=.5, alpha=1,)+
# 
#   
#   theme_bw(base_size = 8)+ theme(legend.position= "none",
#                                  axis.title.x = element_blank(),
#                                  legend.title = element_blank(),
#                                  legend.text = element_text(size = 8))
# fig.2L1
# mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Fig2L1", ".png", sep = '')
# ggsave(mi_nombre_de_archivo, plot=fig.2L1, width=14, height=7, units="cm", limitsize=FALSE, dpi=600)
# 
# 
# 
# Figure1 = ggarrange(fig.1,
#                     ggarrange(fig.2Leq, fig.2L1, ncol = 2, labels = c("B", "C"),common.legend = TRUE, legend="bottom", align = "hv"),
#                     labels = "A",
#                     nrow = 2,
#                     align = "hv")
# Figure1
# ggsave("figures/FIGURE1.png", plot=Figure1, width = 17, height = 17, units = "cm", dpi=600, limitsize=FALSE,bg = "white")  
# 
# 
# 
# 
# 
# # FIGURE 3 
# table.data.avg <- table.data.chile %>%
#   group_by(time,condition) %>%
#   summarise(spl_avg = 10*log10(sum(10^(Leq/10))/n()))
# 
# fig.1a = ggplot(table.data.avg, aes(x = time, y = spl_avg, color = condition)) + geom_point(alpha = 1, show.legend = FALSE)+
#   # geom_smooth(method = lm, aes(fill=time_interval, linetype = time_interval, color = condition),size = .5,se=FALSE, fullrange=FALSE)+
#   #geom_hline(yintercept = 66.4, color = "green", alpha = .5, size = 2)+
#   
#   scale_colour_manual(values = cbPalette) + 
#   scale_fill_manual(values = cbPalette) +       
#   #facet_grid(.~celebration)+
#   labs(y = "Sound Presure Level [dB]",
#        x = "Time [sec]") +
#   #guides(color="none", fill = guide_legend("time_interval"))+
#   theme_bw(base_size = 8)+ theme(legend.position = c(.99, .99),
#                                  legend.justification = c("right", "top"),
#                                  legend.title = element_blank(),
#                                  legend.text = element_text(size = 8))
# 
# fig.1a
# mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Fig1", ".png", sep = '')
# ggsave(mi_nombre_de_archivo, plot=fig.1a, width=14, height=7, units="cm", limitsize=FALSE, dpi=600)
# 
