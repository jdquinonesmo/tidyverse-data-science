##### EXAMEN 1 #####
library(tidyverse)
library(nycflights13)
library(cowplot)
# PUNTO 1.) #

  
#a) Compañía con menor porcentaje de vuelos retrasados

  t1<-flights%>%
  filter(!is.na(dep_delay), !is.na(arr_delay))%>%
  group_by(carrier)%>%
  summarise(N=n(), n=sum(dep_delay>0), prop=n/N)
  arrange(t1, desc(prop))
  
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
  
# PUNTO 4.) 
   
  # Investiga si existe algún patrón del número de vuelos que se cancelan cada día.
  View(flights) 
  t4 <- flights %>% group_by(day) %>% summarise(n=sum(is.na(arr_time)), N=n(), p=n/N)
   ggplot(t4, mapping = aes(x = day, y = N))+
    geom_point()
  a<-ggplot(t4, mapping = aes(x = day, y = n))+
  geom_point()
  b<-ggplot(t4, mapping = aes(x = N, y= n))+
    geom_point()
  plot_grid(a, b)

 t6 <- group_by(flights, carrier, day) %>% summarise(n=sum(is.na(arr_time)), N=n(), p=n/N)
 a<-ggplot(t6, mapping = aes(x = day, y = p, col=carrier))+
   geom_smooth(se=F)
 arrange(t6, N)
 View(filter(flights, carrier == "OO") %>% group_by(day)
      %>% summarise(n=sum(is.na(arr_time)), N=n(), p=n/N))
 t6 <- filter(t6, carrier != "OO")
 b<-ggplot(t6, mapping = aes(x = day, y = p, col=carrier))+
   geom_smooth(se=F)
 plot_grid(a, b)
 filter(flights, carrier == "OO") %>% summarise(n())
 
 
 # Investiga si la proporción de vuelos cancelados está relacionada con el 
 # retraso promedio por día en los vuelos.
 View(flights)
 t7 <- filter(flights, carrier != "OO")
 t7 <- group_by(t7, year, month, day)  %>%
   summarise(n=sum(is.na(arr_time)), N=n(), p=n/N, delay_prom=mean(dep_delay, na.rm=T))
 ggplot(t7, mapping = aes(x = delay_prom, y = p))+
   geom_smooth(se=F)+
   geom_point()

 #Investiga si la proporción de vuelos cancelados está relacionada con el 
 #retraso promedio por aeropuerto en los vuelos.
 
 t8 <- filter(flights, carrier != "OO")
 t8 <- group_by(t8, origin)  %>%
   summarise(n=sum(is.na(arr_time)), N=n(), p=n/N, delay_prom=mean(dep_delay, na.rm=T))
 ggplot(t8, mapping = aes(x = delay_prom, y = p))+
    geom_text(aes(label=origin))

 sum(is.na(flights$arr_time))
