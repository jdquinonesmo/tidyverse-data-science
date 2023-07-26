###### EXAMEN 1 #####
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
  t4 <- flights %>% group_by(year, month, day) %>% summarise(n=sum(is.na(arr_time)), N=n(), p=n/N)
  d<-c(1:365)
  t4<-data.frame(t4,d)
   ggplot(t4, mapping = aes(x = d, y = N))+
    geom_point()
  a<-ggplot(t4, mapping = aes(x = d, y = n))+
  geom_point()
  b<-ggplot(t4, mapping = aes(x = N, y= n))+
    geom_point()
  
  plot_grid(a, b, c, d)

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
 t7 <- group_by(t7, day)  %>%
   summarise(n=sum(is.na(arr_time)), N=n(), p=n/N, delay_prom=mean(dep_delay, na.rm=T))
 t7<-data.frame(t7,d)
 
 
 ggplot(t7, mapping = aes(x = delay_prom, y = p))+
   geom_smooth(se=F)+
   geom_point()

 #Investiga si la proporción de vuelos cancelados está relacionada con el 
 #retraso promedio por aeropuerto en los vuelos.
 
 t8 <- filter(flights, carrier != "OO")
 t8 <- group_by(t8, dest)  %>%
   summarise(n=sum(is.na(arr_time)), N=n(), p=n/N, delay_prom=mean(dep_delay, na.rm=T))
 ggplot(t8, mapping = aes(x = delay_prom, y = p))+geom_point()+
    geom_smooth(se=F)+
    ylim(-0.02,0.15)
    geom_text(aes(label=dest))

 sum(is.na(flights$arr_time))
 
 #¿Qué compañía sufre los peores retrasos?
 
 t9 <- filter(flights, carrier != "OO")
 t9 <- group_by(t9, carrier)  %>%
   summarise(n=sum(dep_delay>0,na.rm=T),N=n(), p=n/N, delay_prom=mean(dep_delay, na.rm=T))%>%
   arrange(desc(delay_prom))
 ggplot(t9, mapping=aes(y=p, x=delay_prom))+geom_point(aes(type=carrier))
 #F9 Frontier Airlines Inc. En promedio tiene el mayor tiempo de retraso. 
 
#Punto 5.
 
#Difícil: Intenta desentrañar los efectos que producen los retrasos por culpa 
#de malos aeropuertos vs malas compañías aéreas. Por ejemplo, intenta usar 
 t10 <- filter(flights, carrier != "OO")
 t10 <- group_by(t10, carrier) %>% summarise(n=n(), N=sum(is.na(arr_time)), p=N/n, 
        delay_prom=mean(dep_delay, na.rm=T))
 a<-ggplot(t10, mapping=aes(x=delay_prom, y=p))+geom_smooth(se=F)+
 geom_text(aes(label=carrier))+
 geom_point()
 
 t11 <- filter(flights, carrier != "OO")
 t11 <- group_by(t11, dest) %>% summarise(n=n(), N=sum(is.na(arr_time)), p=N/n, 
                                             delay_prom=mean(dep_delay, na.rm=T))
 b<-ggplot(t11, mapping=aes(x=delay_prom, y=p))+geom_smooth(se=F)+
   geom_text(aes(label=dest))+ylim(c(-0.05,0.17))

 t12 <- filter(flights, carrier != "OO")
 t12 <- group_by(t12, carrier, dest) %>% summarise(n=n(), N=sum(is.na(arr_time)), p=N/n, 
                                             delay_prom=mean(dep_delay, na.rm=T))
 c<-ggplot(t12, mapping=aes(x=delay_prom, y=p, color=carrier))+geom_line()
 
 d<-ggplot(t10,mapping = aes(x=reorder(carrier,-p), y=p, fill=carrier))+
   geom_col();d
 plot_grid(a,b,c,d)
#Punto 6. 

#Si sort es True entonces pone en orden descendente los grupos organizados por
#el count() con mayor cantidad de miembros. Es útil cuando queremos observar 
#cuáles agrupaciones según las características que deseamos conocer son más 
#frecuentes, con más datos y con mayor información interna. 
 
#La función group_by() agrupa un conjunto de filas seleccionado en un conjunto 
#de filas de resumen de acuerdo con los valores de una o más columnas o 
#expresiones. Cuando se aplica antes una función como filter(), no se tendrán 
#en cuenta las filas filtradas y eso afectará los conteos de las agrupaciones,
#con eso también las funciones como mean(), sd(), max() o cualquier otra de 
#resumen estadístico, pues no tendrá las filas completas del dataset. 
#A veces el objetivo es ese, depurar algunas filas que afecten el análisis por resultados atípicos. 
 
#En select() no afectará las filas y por tanto la agrupación con group_by no se
#alterará a menos que las columnas usadas para esa agrupación sean quitadas 
#antes por el mismo select(), pues no habría criterio para agrupar y no tendría sentido. 
 
 

#Punto 7.
 
#Investiga el top 10 de qué aviones (número de cola y compañía)
#llegaron más tarde a su destino.


arrange(flights, desc(arr_delay)) %>% select(tailnum, carrier, arr_delay)
t13<-group_by(flights, tailnum, carrier) %>% summarise(N=n(), delay_prom=mean(arr_delay, na.rm = T))
arrange(t13, desc(delay_prom)) 


#Punto 8. Queremos saber qué hora del día nos conviene volar si queremos evitar los retrasos en la salida.
#Difícil: Queremos saber qué día de la seman nos conviene volar si queremos evitar los retrasos en la salida.

View(flights)
t14<-group_by(flights, hour) %>% summarise(N=n(), delay_prom=mean(dep_delay,na.rm=T))
a<-ggplot(t14, mapping = aes(x=hour, y=delay_prom))+geom_point()+geom_line()

t15<-mutate(flights, dia=weekdays(time_hour))%>% group_by(dia) %>% summarise(N=n(), n=sum(dep_delay>0, na.rm=T), 
                                              delay_prom=mean(dep_delay,na.rm=T), p=n/N)

b<-ggplot(t15,mapping = aes(x=reorder(dia,-delay_prom), y=delay_prom, fill=dia))+
  geom_col()


plot_grid(a,b)

# Punto 9. 
#Para cada destino, calcula el total de minutos de retraso acumulado.
#Para cada uno de ellos, calcula la proporción del total de retraso para dicho destino.

t16<-count(flights, dest, wt=arr_delay) %>% transmute(dest, min_acum=n, p=min_acum/sum(min_acum))

#Punto 10. 

t15<-mutate(t14[-1,], dif=diff(t14$delay_prom, lag=1))

a<-ggplot(t15[-1,], mapping = aes(x=hour, y=delay_prom))+geom_point()+geom_line()
b<-ggplot(t15[-1,], mapping = aes(x=hour, y=dif))+geom_point()+geom_line()
plot_grid(a,b)


#Punto 11. 

t17<-transmute(flights, veloz=min(air_time, na.rm=T), rel=air_time/veloz, origin, dest) %>% arrange(desc(rel))
t17

#Punto 12. 

t18<-group_by(flights, dest, carrier) %>% summarise(N=n(), prom_delay=mean(dep_delay, na.rm=T),
                                cancel=sum(is.na(arr_time)), p.cancel=cancel/N, ret=sum(dep_delay>60, na.rm=T), p.ret=ret/N)
filtro<-filter(group_by(t18, dest)%>%summarise(n=n()), n==1)
View(filter(t18, !dest %in% filtro$dest)%>%arrange(dest, prom_delay, p.cancel))

t19<-select(flights, tailnum, dep_delay)%>%
  filter(!is.na(dep_delay))%>% group_by(tailnum)%>%
  summarise(N=n(), n=sum(dep_delay>60), p=n/N)%>%arrange(p)%>%
  filter(N>10)
