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
  
#a) Compañía con menor porcentaje de vuelos retrasados

  t1<-flights%>%
  filter(!is.na(dep_delay), !is.na(arr_delay))%>%
  group_by(carrier)%>%
  summarise(N=n(), n=sum(dep_delay>0), prop=n/N)
  arrange(t1, prop)
  
#b) Destino con mayor proporción de vuelos retrasados.
  
  t2<-flights%>%
    filter(!is.na(dep_delay), !is.na(arr_delay))%>%
    group_by(dest)%>%
    summarise(N=n(), n=sum(dep_delay>0), prop=n/N)
    View(arrange(t2,desc(prop)))
    
#c) Total de vuelos que se retrasan más de 60 minutos.
    
    t2<-flights%>%
      filter(!is.na(dep_delay), !is.na(arr_delay))
    prop=sum(t2$dep_delay>60)/length(t2$dep_delay) 
  
#d) Vuelos que llegan tarde.
    
   prop_ret_arr=sum(t2$arr_delay>0)/length(t2$arr_delay)
   prop_ret_dep=sum(t2$dep_delay>0)/length(t2$dep_delay)
   
   ggplot(t2, mapping = aes(x=dep_delay, y=arr_delay, col=carrier))+
     geom_point()
   
#e) Vuelos considerados que llegan a tiempo.
   
   prop_atiempo=sum(t2$arr_delay<=30)/length(t2$arr_delay)
   