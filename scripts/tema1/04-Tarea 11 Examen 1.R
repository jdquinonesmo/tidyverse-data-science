##### EXAMEN 1 #####
library(tidyverse)
library(nycflights13)
# PUNTO 1.) #
View(flights)

t1<-flights %>%
  group_by(carrier) %>% 
  summarise(distancia=mean(distance))  
  View(arrange(t1, distancia))
  ggplot(t1, mapping = aes(x=reorder(carrier,-distancia), y=distancia, fill=carrier))+
    geom_col()

ggplot(flights, mapping=aes(x=reorder(carrier,-dep_delay), y=dep_delay))+
  stat_summary(fun.ymin = min,
               fun.ymax = max,
               fun.y = median)
  
