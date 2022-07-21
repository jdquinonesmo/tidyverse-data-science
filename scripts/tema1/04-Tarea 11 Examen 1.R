##### EXAMEN 1 #####
library(tidyverse)
library(nycflights13)
# PUNTO 1.) #

  
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
   
# PUNTO 2.)
   
   not_cancelled<-flights%>% 
     filter(!is.na(dep_delay), !is.na(arr_delay))
   
   not_cancelled %>% count(dest)
   
   not_cancelled %>% group_by(dest) %>% summarise(n())
   
   not_cancelled %>% count(tailnum, wt = distance)
   
   not_cancelled %>% group_by(tailnum) %>% summarise(sum(distance))
  
# PUNTO 3.) 
   
  view(flights) 
   