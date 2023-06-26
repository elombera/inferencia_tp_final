library(tidyverse)
library(magrittr)
library(purrr)
library(dplyr)
library(ggpubr)
library(seewave)

load("./data.RData")

# Stadistic analyasis -------------------

m.SPL<- lm(spl_avg_p ~ condition*time_interval*celebration, 
           data = filter(table.data.p, celebration != "Normal day"))
summary(m.SPL)
summ(m.SPL)
anova(m.SPL)

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


