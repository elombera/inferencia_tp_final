library(tidyverse)
library(magrittr)
library(purrr)
library(dplyr)
library(ggpubr)
library(seewave)
library(Routliers)

table.outliers <- table.data.p %>% 
  filter(condition =="With fireworks" & time_interval =="15 min" & celebration == "New Year") %>% 
  ungroup()
res3 <- outliers_mad(x = table.outliers$L1 ,na.rm=TRUE)
plot_outliers_mad(res3,x=table.outliers$L1,pos_display=TRUE)
table.outliers[res3$outliers_pos,] # ----> NA

table.outliers <- table.data.p %>% 
  filter(condition =="With fireworks" & time_interval =="15 min" & celebration == "Christmas") %>% 
  ungroup()
res3 <- outliers_mad(x = table.outliers$L1 ,na.rm=TRUE)
plot_outliers_mad(res3,x=table.outliers$L1,pos_display=TRUE)
table.outliers[res3$outliers_pos,] # ----> P4

table.outliers <- table.data.p %>% 
  filter(condition =="With fireworks" & time_interval =="30 min" & celebration == "New Year") %>% 
  ungroup()
res3 <- outliers_mad(x = table.outliers$L1 ,na.rm=TRUE)
plot_outliers_mad(res3,x=table.outliers$L1,pos_display=TRUE)
table.outliers[res3$outliers_pos,] # ----> NA

table.outliers <- table.data.p %>% 
  filter(condition =="With fireworks" & time_interval =="30 min" & celebration == "Christmas") %>% 
  ungroup()
res3 <- outliers_mad(x = table.outliers$L1 ,na.rm=TRUE)
plot_outliers_mad(res3,x=table.outliers$L1,pos_display=TRUE)
table.outliers[res3$outliers_pos,] # ----> NA

table.outliers <- table.data.p %>% 
  filter(condition =="With fireworks" & time_interval =="60 min" & celebration == "New Year") %>% 
  ungroup()
res3 <- outliers_mad(x = table.outliers$L1 ,na.rm=TRUE)
plot_outliers_mad(res3,x=table.outliers$L1,pos_display=TRUE)
table.outliers[res3$outliers_pos,] # ----> NA

table.outliers <- table.data.p %>% 
  filter(condition =="With fireworks" & time_interval =="60 min" & celebration == "Christmas") %>% 
  ungroup()
res3 <- outliers_mad(x = table.outliers$L1 ,na.rm=TRUE)
plot_outliers_mad(res3,x=table.outliers$L1,pos_display=TRUE)
table.outliers[res3$outliers_pos,] # ----> NA

table.outliers <- table.data.p %>% 
  filter(condition =="With fireworks" & time_interval =="15 min" & celebration == "New Year") %>% 
  ungroup()
res3 <- outliers_mad(x = table.outliers$spl_avg_p ,na.rm=TRUE)
plot_outliers_mad(res3,x=table.outliers$spl_avg_p,pos_display=TRUE)
table.outliers[res3$outliers_pos,] # ----> NA

table.outliers <- table.data.p %>% 
  filter(condition =="With fireworks" & time_interval =="15 min" & celebration == "Christmas") %>% 
  ungroup()
res3 <- outliers_mad(x = table.outliers$spl_avg_p ,na.rm=TRUE)
plot_outliers_mad(res3,x=table.outliers$spl_avg_p,pos_display=TRUE)
table.outliers[res3$outliers_pos,] # ----> NA

table.outliers <- table.data.p %>% 
  filter(condition =="With fireworks" & time_interval =="30 min" & celebration == "New Year") %>% 
  ungroup()
res3 <- outliers_mad(x = table.outliers$spl_avg_p ,na.rm=TRUE)
plot_outliers_mad(res3,x=table.outliers$spl_avg_p,pos_display=TRUE)
table.outliers[res3$outliers_pos,] # ----> NA

table.outliers <- table.data.p %>% 
  filter(condition =="With fireworks" & time_interval =="30 min" & celebration == "Christmas") %>% 
  ungroup()
res3 <- outliers_mad(x = table.outliers$spl_avg_p ,na.rm=TRUE)
plot_outliers_mad(res3,x=table.outliers$spl_avg_p,pos_display=TRUE)
table.outliers[res3$outliers_pos,] # ----> NA

table.outliers <- table.data.p %>% 
  filter(condition =="With fireworks" & time_interval =="60 min" & celebration == "New Year") %>% 
  ungroup()
res3 <- outliers_mad(x = table.outliers$spl_avg_p ,na.rm=TRUE)
plot_outliers_mad(res3,x=table.outliers$spl_avg_p,pos_display=TRUE)
table.outliers[res3$outliers_pos,] # ----> NA

table.outliers <- table.data.p %>% 
  filter(condition =="With fireworks" & time_interval =="60 min" & celebration == "Christmas") %>% 
  ungroup()
res3 <- outliers_mad(x = table.outliers$spl_avg_p ,na.rm=TRUE)
plot_outliers_mad(res3,x=table.outliers$spl_avg_p,pos_display=TRUE)
table.outliers[res3$outliers_pos,] # ----> NA

table.outliers <- table.data.p %>% 
  filter(condition =="With fireworks" & time_interval =="15 min" & celebration == "New Year") %>% 
  ungroup()
res3 <- outliers_mad(x = table.outliers$L10 ,na.rm=TRUE)
plot_outliers_mad(res3,x=table.outliers$L10,pos_display=TRUE)
table.outliers[res3$outliers_pos,] # ----> NA

table.outliers <- table.data.p %>% 
  filter(condition =="With fireworks" & time_interval =="15 min" & celebration == "Christmas") %>% 
  ungroup()
res3 <- outliers_mad(x = table.outliers$L10 ,na.rm=TRUE)
plot_outliers_mad(res3,x=table.outliers$L10,pos_display=TRUE)
table.outliers[res3$outliers_pos,] # ----> P4

table.outliers <- table.data.p %>% 
  filter(condition =="With fireworks" & time_interval =="30 min" & celebration == "New Year") %>% 
  ungroup()
res3 <- outliers_mad(x = table.outliers$L10 ,na.rm=TRUE)
plot_outliers_mad(res3,x=table.outliers$L10,pos_display=TRUE)
table.outliers[res3$outliers_pos,] # ----> NA

table.outliers <- table.data.p %>% 
  filter(condition =="With fireworks" & time_interval =="30 min" & celebration == "Christmas") %>% 
  ungroup()
res3 <- outliers_mad(x = table.outliers$L10 ,na.rm=TRUE)
plot_outliers_mad(res3,x=table.outliers$L10,pos_display=TRUE)
table.outliers[res3$outliers_pos,] # ----> NA

table.outliers <- table.data.p %>% 
  filter(condition =="With fireworks" & time_interval =="60 min" & celebration == "New Year") %>% 
  ungroup()
res3 <- outliers_mad(x = table.outliers$L10 ,na.rm=TRUE)
plot_outliers_mad(res3,x=table.outliers$L10,pos_display=TRUE)
table.outliers[res3$outliers_pos,] # ----> NA

table.outliers <- table.data.p %>% 
  filter(condition =="With fireworks" & time_interval =="60 min" & celebration == "Christmas") %>% 
  ungroup()
res3 <- outliers_mad(x = table.outliers$L10 ,na.rm=TRUE)
plot_outliers_mad(res3,x=table.outliers$L10,pos_display=TRUE)
table.outliers[res3$outliers_pos,] # ----> NA
