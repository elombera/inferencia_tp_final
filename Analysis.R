library(tidyverse)
library(magrittr)
library(purrr)
library(dplyr)
library(ggpubr)
library(seewave)
library(ggstatsplot)

rm(list=ls())

load("./data.RData")

# Stadistic analyasis -------------------

# Comparison Measurement points on festive days ----
m.SPL<- lm(spl_avg_p ~ condition*time_interval*celebration, 
           data = filter(table.data.p, celebration != "Normal day"))
summary(m.SPL)
extract_stats(ggcoefstats(m.SPL))
anova(m.SPL)

m.LA<- lm(LA ~ condition*time_interval*celebration, 
          data = filter(table.data.p, celebration != "Normal day"))
summary(m.LA)
extract_stats(ggcoefstats(m.LA))
anova(m.LA)

m.L1<- lm(L1 ~ condition*time_interval*celebration, 
          data = filter(table.data.p, celebration != "Normal day"))
summary(m.L1)
extract_stats(ggcoefstats(m.L1))
anova(m.L1)

m.L10<- lm(L10 ~ condition*time_interval*celebration, 
           data = filter(table.data.p, celebration != "Normal day"))
summary(m.L10)
extract_stats(ggcoefstats(m.L10))
anova(m.L10)

m.L50<- lm(L50 ~ condition*time_interval*celebration, 
           data = filter(table.data.p, celebration != "Normal day"))
summary(m.L50)
extract_stats(ggcoefstats(m.L50))
anova(m.L50)

m.L90<- lm(L90 ~ condition*time_interval*celebration, 
           data = filter(table.data.p, celebration != "Normal day"))
summary(m.L90)
extract_stats(ggcoefstats(m.L90))
anova(m.L90)

# LeqA -----

t.test(filter(table.data.p,
              celebration =="Christmas" & time_interval =="60 min" & condition =="With fireworks")$spl_avg_p,
       filter(table.data.p,
              celebration =="Christmas" & time_interval =="15 min" & condition =="With fireworks")$spl_avg_p,
       paired = FALSE)

t.test(filter(table.data.p,
              celebration =="Christmas" & time_interval =="30 min" & condition =="With fireworks")$spl_avg_p,
       filter(table.data.p,
              celebration =="New Year" & time_interval =="30 min" & condition =="With fireworks")$spl_avg_p,
       paired = FALSE)

t.test(filter(table.data.p,
              celebration =="Christmas" & time_interval =="15 min" & condition =="With fireworks")$spl_avg_p,
       filter(table.data.p,
              celebration =="New Year" & time_interval =="15 min" & condition =="With fireworks")$spl_avg_p,
       paired = FALSE)



t.test(filter(table.data.p,
              celebration =="Christmas" & time_interval =="60 min" & condition =="Without fireworks")$spl_avg_p,
       filter(table.data.p,
              celebration =="New Year" & time_interval =="60 min" & condition =="Without fireworks")$spl_avg_p,
       paired = FALSE)

t.test(filter(table.data.p,
              celebration =="Christmas" & time_interval =="30 min" & condition =="Without fireworks")$spl_avg_p,
       filter(table.data.p,
              celebration =="New Year" & time_interval =="30 min" & condition =="Without fireworks")$spl_avg_p,
       paired = FALSE)

t.test(filter(table.data.p,
              celebration =="Christmas" & time_interval =="15 min" & condition =="Without fireworks")$spl_avg_p,
       filter(table.data.p,
              celebration =="New Year" & time_interval =="15 min" & condition =="Without fireworks")$spl_avg_p,
       paired = FALSE)


t.test(filter(table.data.p,
              celebration =="Christmas")$spl_avg_p,
       filter(table.data.p,
              celebration =="New Year")$spl_avg_p,
       paired = FALSE)



# Comparison with normal environmental noise levels in CABA ----

m.SPL<- lm(spl_avg_p ~ condition*time_interval, 
           data = table.data.p)
summary(m.SPL)
extract_stats(ggcoefstats(m.SPL))
anova(m.SPL)

m.LA<- lm(LA ~ condition*time_interval, 
           data = table.data.p)
summary(m.LA)
extract_stats(ggcoefstats(m.LA))
anova(m.LA)

m.L1<- lm(L1 ~ condition*time_interval, 
           data = table.data.p)
summary(m.L1)
extract_stats(ggcoefstats(m.L1))
anova(m.L1)

m.L10<- lm(L10 ~ condition*time_interval, 
           data = table.data.p)
summary(m.L10)
extract_stats(ggcoefstats(m.L10))
anova(m.L10)

m.L50<- lm(L50 ~ condition*time_interval, 
           data = table.data.p)
summary(m.L50)
extract_stats(ggcoefstats(m.L50))
anova(m.L50)

m.L90<- lm(L90 ~ condition*time_interval, 
           data = table.data.p)
summary(m.L90)
extract_stats(ggcoefstats(m.L90))
anova(m.L90)

# LeqA - t-test -----------

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


# LA - t-test -----------

t.test(filter(table.data.p,
              condition =="Environmental noise")$LA,
       filter(table.data.p,
              condition =="With fireworks" & time_interval =="15 min")$LA,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$LA,
       filter(table.data.p,
              condition =="With fireworks" & time_interval =="30 min")$LA,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$LA,
       filter(table.data.p,
              condition =="With fireworks" & time_interval =="60 min")$LA,
       paired = FALSE)


t.test(filter(table.data.p,
              condition =="Environmental noise")$LA,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="15 min")$LA,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$LA,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="30 min")$LA,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$LA,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="60 min")$LA,
       paired = FALSE)

# L1 - t-test -----------

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

# L10 - t-test -----------

t.test(filter(table.data.p,
              condition =="Environmental noise")$L10,
       filter(table.data.p,
              condition =="With fireworks" & time_interval =="15 min")$L10,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$L10,
       filter(table.data.p,
              condition =="With fireworks" & time_interval =="30 min")$L10,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$L10,
       filter(table.data.p,
              condition =="With fireworks" & time_interval =="60 min")$L10,
       paired = FALSE)


t.test(filter(table.data.p,
              condition =="Environmental noise")$L10,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="15 min")$L10,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$L10,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="30 min")$L10,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$L10,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="60 min")$L10,
       paired = FALSE)
# L50 - t-test -----------

t.test(filter(table.data.p,
              condition =="Environmental noise")$L50,
       filter(table.data.p,
              condition =="With fireworks" & time_interval =="15 min")$L50,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$L50,
       filter(table.data.p,
              condition =="With fireworks" & time_interval =="30 min")$L50,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$L50,
       filter(table.data.p,
              condition =="With fireworks" & time_interval =="60 min")$L50,
       paired = FALSE)


t.test(filter(table.data.p,
              condition =="Environmental noise")$L50,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="15 min")$L50,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$L50,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="30 min")$L50,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$L50,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="60 min")$L50,
       paired = FALSE)
# L90 - t-test -----------

t.test(filter(table.data.p,
              condition =="Environmental noise")$L90,
       filter(table.data.p,
              condition =="With fireworks" & time_interval =="15 min")$L90,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$L90,
       filter(table.data.p,
              condition =="With fireworks" & time_interval =="30 min")$L90,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$L90,
       filter(table.data.p,
              condition =="With fireworks" & time_interval =="60 min")$L90,
       paired = FALSE)


t.test(filter(table.data.p,
              condition =="Environmental noise")$L90,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="15 min")$L90,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$L90,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="30 min")$L90,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$L90,
       filter(table.data.p,
              condition =="Without fireworks" & time_interval =="60 min")$L90,
       paired = FALSE)









# Comparison measurement points on festive days in Argentina with Chile ----
table.data.comparision.avg = table.data.comparision %>%
  group_by(point,condition,country) %>%
  summarise(Leq = meandB(Leq, level = "IL"),
            L5_spl_avg = meandB(L5, level= "IL"),
            L5_spl_sd = sddB(L5, level= "IL"),
            L10_spl_avg = meandB(L10, level= "IL"),
            L10_spl_sd = sddB(L10, level= "IL"),
            L50_spl_avg = meandB(L50, level= "IL"),
            L1_spl_sd = sddB(L50, level= "IL"),
            L90_spl_avg = meandB(L90, level= "IL"),
            L90_spl_sd = sddB(L90, level= "IL"))


m.SPL<- lm(Leq ~ condition*country, 
           data = table.data.comparision.avg)
summary(m.SPL)
extract_stats(ggcoefstats(m.SPL))
anova(m.SPL)

m.L5<- lm(L5_spl_avg ~ condition*country, 
          data = table.data.comparision.avg)
summary(m.L5)
extract_stats(ggcoefstats(m.L5))
anova(m.L5)

m.L10<- lm(L10_spl_avg ~ condition*country, 
           data = table.data.comparision.avg)
summary(m.L10)
extract_stats(ggcoefstats(m.L10))
anova(m.L10)

m.L50<- lm(L50_spl_avg ~ condition*country, 
           data = table.data.comparision.avg)
summary(m.L50)
extract_stats(ggcoefstats(m.L50))
anova(m.L50)

m.L90<- lm(L90_spl_avg ~ condition*country, 
           data = table.data.comparision.avg)
summary(m.L90)
extract_stats(ggcoefstats(m.L90))
anova(m.L90)

#  - t-test  calculated separate averages based on the distance to the noise source, labeled as "Near" and "Far". -----------

table.data.avg.chi.l <- filter(table.data.comparision, country == "Chile", point == "P1" | point == "P4") %>%
  group_by(point,condition,country) %>%
  summarise(Leq = meandB(Leq, level = "IL"),
            L5_spl_avg = meandB(L5, level= "IL"),
            L5_spl_sd = sddB(L5, level= "IL"),
            L10_spl_avg = meandB(L10, level= "IL"),
            L10_spl_sd = sddB(L10, level= "IL"),
            L50_spl_avg = meandB(L50, level= "IL"),
            L1_spl_sd = sddB(L50, level= "IL"),
            L90_spl_avg = meandB(L90, level= "IL"),
            L90_spl_sd = sddB(L90, level= "IL"))
table.data.avg.chi.l$label = "Nearby points"

table.data.avg.chi.c <- filter(table.data.comparision, country == "Chile", point == "P2" | point == "P3") %>%
  group_by(point,condition,country) %>%
  summarise(Leq = meandB(Leq, level = "IL"),
            L5_spl_avg = meandB(L5, level= "IL"),
            L5_spl_sd = sddB(L5, level= "IL"),
            L10_spl_avg = meandB(L10, level= "IL"),
            L10_spl_sd = sddB(L10, level= "IL"),
            L50_spl_avg = meandB(L50, level= "IL"),
            L1_spl_sd = sddB(L50, level= "IL"),
            L90_spl_avg = meandB(L90, level= "IL"),
            L90_spl_sd = sddB(L90, level= "IL"))
table.data.avg.chi.c$label = "Far points"

table.data.avg.chile = merge(table.data.avg.chi.l,table.data.avg.chi.c,all=TRUE)


t.test(filter(table.data.comparision.avg,
              condition =="With fireworks" & country =="Argentina")$Leq,
       filter(table.data.avg.chile,
              condition =="With fireworks" & label =="Nearby points")$Leq,
       paired = FALSE)
t.test(filter(table.data.comparision.avg,
              condition =="With fireworks" & country =="Argentina")$Leq,
       filter(table.data.avg.chile,
              condition =="With fireworks" & label =="Far points")$Leq,
       paired = FALSE)


t.test(filter(table.data.comparision.avg,
              condition =="With fireworks" & country =="Argentina")$L5_spl_avg,
       filter(table.data.avg.chile,
              condition =="With fireworks" & label =="Nearby points")$L5_spl_avg,
       paired = FALSE)
t.test(filter(table.data.comparision.avg,
              condition =="With fireworks" & country =="Argentina")$L5_spl_avg,
       filter(table.data.avg.chile,
              condition =="With fireworks" & label =="Far points")$L5_spl_avg,
       paired = FALSE)


t.test(filter(table.data.comparision.avg,
              condition =="With fireworks" & country =="Argentina")$L10_spl_avg,
       filter(table.data.avg.chile,
              condition =="With fireworks" & label =="Nearby points")$L10_spl_avg,
       paired = FALSE)
t.test(filter(table.data.comparision.avg,
              condition =="With fireworks" & country =="Argentina")$L10_spl_avg,
       filter(table.data.avg.chile,
              condition =="With fireworks" & label =="Far points")$L10_spl_avg,
       paired = FALSE)

t.test(filter(table.data.comparision.avg,
              condition =="With fireworks" & country =="Argentina")$L50_spl_avg,
       filter(table.data.avg.chile,
              condition =="With fireworks" & label =="Nearby points")$L50_spl_avg,
       paired = FALSE)
t.test(filter(table.data.comparision.avg,
              condition =="With fireworks" & country =="Argentina")$L50_spl_avg,
       filter(table.data.avg.chile,
              condition =="With fireworks" & label =="Far points")$L50_spl_avg,
       paired = FALSE)

t.test(filter(table.data.comparision.avg,
              condition =="With fireworks" & country =="Argentina")$L90_spl_avg,
       filter(table.data.avg.chile,
              condition =="With fireworks" & label =="Nearby points")$L90_spl_avg,
       paired = FALSE)
t.test(filter(table.data.comparision.avg,
              condition =="With fireworks" & country =="Argentina")$L90_spl_avg,
       filter(table.data.avg.chile,
              condition =="With fireworks" & label =="Far points")$L90_spl_avg,
       paired = FALSE)

# dbug ---------------


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



# ----------

a = filter(table.data.p, time_interval == "15 min" | time_interval == "Environmental noise")

m.SPL<- lm(spl_avg_p ~ condition, 
           data = a)
summary(m.SPL)
extract_stats(ggcoefstats(m.SPL))
anova(m.SPL)


t.test(filter(a,
              condition =="Environmental noise")$spl_avg_p,
       filter(a,
              condition =="With fireworks")$spl_avg_p,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$spl_avg_p,
       filter(table.data.p,
              condition =="Without fireworks")$spl_avg_p,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Without fireworks")$spl_avg_p,
       filter(table.data.p,
              condition =="With fireworks")$spl_avg_p,
       paired = FALSE)



m.LA<- lm(LA ~ condition*time_interval, 
          data = a)
summary(m.LA)
extract_stats(ggcoefstats(m.LA))
anova(m.LA)

t.test(filter(a,
              condition =="Environmental noise")$LA,
       filter(a,
              condition =="With fireworks")$LA,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$LA,
       filter(table.data.p,
              condition =="Without fireworks")$LA,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Without fireworks")$LA,
       filter(table.data.p,
              condition =="With fireworks")$LA,
       paired = FALSE)



m.L1<- lm(L1 ~ condition*time_interval, 
          data = a)
summary(m.L1)
extract_stats(ggcoefstats(m.L1))
anova(m.L1)

t.test(filter(a,
              condition =="Environmental noise")$L1,
       filter(a,
              condition =="With fireworks")$L1,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Environmental noise")$L1,
       filter(table.data.p,
              condition =="Without fireworks")$L1,
       paired = FALSE)
t.test(filter(table.data.p,
              condition =="Without fireworks")$L1,
       filter(table.data.p,
              condition =="With fireworks")$L1,
       paired = FALSE)

m.L10<- lm(L10 ~ condition*time_interval, 
           data = table.data.p)
summary(m.L10)
extract_stats(ggcoefstats(m.L10))
anova(m.L10)

m.L50<- lm(L50 ~ condition*time_interval, 
           data = table.data.p)
summary(m.L50)
extract_stats(ggcoefstats(m.L50))
anova(m.L50)

m.L90<- lm(L90 ~ condition*time_interval, 
           data = table.data.p)
summary(m.L90)
extract_stats(ggcoefstats(m.L90))
anova(m.L90)