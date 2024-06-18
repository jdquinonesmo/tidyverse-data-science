library(tidyverse)
# Análisis exploratorio de datos
# * Modelar
# * Representación gráfica
# * Transformar datos

# * ¿Qué tipo de variaciones sufren las variables?
# * ¿Qué tipo de covariación sufren las variables?

# * variable: cantidad, factor o propiedad medible
# * valor: estado de una variable al ser medida
# * observación: conjunto de medidas tomadas en condiciones similares
#                data point, conjunto de valores tomados para cada variable
# * datos tabulares: conjunto de valores, asociado a cada variable y observación
#                si los datos están limpios, cada valor tiene su propia celda
#                cada variable tiene su columna, y cada observación su fila


#### VARIACIÓN
## Variables categóricas: factor o vector de caracteres
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x=cut))

diamonds %>%
  count(cut)

## Variable contínua: conjunto infinito de valores ordenados (números, fechas)
ggplot(data = diamonds) + 
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

diamonds %>%
  count(cut_width(carat, 0.5))


ggplot(diamonds, mapping = aes(x = "Kilates", y = carat)) + 
  geom_boxplot()

diamonds %>%
  ggplot() + 
  geom_boxplot(mapping = aes(x = cut, y = carat, color = cut))

diamonds_filter <- diamonds %>%
  filter(carat<3)

ggplot(data = diamonds_filter) + 
  geom_histogram(mapping = aes(x = carat), binwidth = 0.01)

ggplot(data = diamonds_filter, 
       mapping = aes(x = carat, color = cut))+
  geom_freqpoly(binwidth = 0.01)

# * Cuales son los valores más comunes? Por qué?
# * Cuales son los valores más raros? Por qué? Cumple con lo que esperábamos?
# * Vemos algún patrón característico o inusual? Podemos explicarlos?

# * Qué determina que los elementos de un cluster sean similares entre si?
# * Qué determina que clusters separados sean diferentes entre si?
# * Describir y explicar cada uno de los clusters.
# * Por qué alguna observación puede ser clasificada en el cluster erróneo...

View(faithful)
?faithful

ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_histogram(binwidth = 0.2)

# outliers
  ggplot(diamonds) + 
    geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0,100))

diamonds %>%
  ggplot(mapping=aes(x = price)) + 
  geom_histogram(binwidth = 100)

diamonds %>%
  filter(price > 18000) %>%
  ggplot(mapping = aes(x = y))+
  geom_histogram()


unusual_diamonds <- diamonds %>%
  filter(y<2 | y >30) %>%
  select(price, x,y,z) %>%
  arrange(y)
View(unusual_diamonds)

#Eliminar toda la fila de valores atípicos
good_diamonds <- diamonds %>%
  filter(between(y, 2.5, 29.5))

#Reemplazar los valores atípicos con NAs
good_diamonds <- diamonds %>%
  mutate(y = ifelse(y<2 | y>30, NA, y))

?ifelse  

ggplot(data = good_diamonds, 
       mapping = aes(x = x, y = y)) + 
  geom_point(na.rm = T)

nycflights13::flights %>%
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min/60
  ) %>%
  ggplot(mapping = aes(sched_dep_time)) + 
    geom_freqpoly(mapping = aes(color = cancelled), binwidth = 1/4)


#Ejercicio 1
a<- ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = x, color=cut), binwidth = 0.05)+
  coord_cartesian(xlim=c(3,10))

b<- ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y, color=cut), binwidth = 0.05)+
  coord_cartesian(xlim=c(0,10))

c<- ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = z, color=cut), binwidth = 0.05)+
  coord_cartesian(xlim=c(0,10))
library(cowplot)
plot_grid(a,b,c)

pairs(data=good_diamonds,~ price + x + y + z)
colnames(diamonds)




# Ejercicio 2
ggplot(good_diamonds) + 
  geom_histogram(mapping = aes(x = price), binwidth = 3) +
  coord_cartesian(xlim = c(1400,1600))

summary(good_diamonds$price)

ggplot(good_diamonds)+
  geom_point(mapping = aes(x = carat, y = price, 
                           color= clarity, shape=cut ))
  
# Ejercicio 3
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = carat), binwidth = 0.01)+
  coord_cartesian(xlim = c(0.95,1.05))

diamonds %>% 
  filter(between(carat, 0.95, 1.05)) %>%
  count(cut_width(carat, 0.01))

good_diamonds %>%
  filter(carat == 0.99 | carat == 1) %>%
  count(carat)


# Ejercicio 4
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = carat), binwidth = 0.01)+
  coord_cartesian(xlim = c(0.95,1.05))

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = carat), binwidth = 0.01)+
  xlim(0.95,1.05)

# Ejercicio 5
na_diamonds <-good_diamonds %>%
  mutate(cut2 = ifelse(cut == "Fair", NA, cut))

ggplot(na_diamonds) + 
  geom_bar(mapping = aes(x = cut2))

ggplot(na_diamonds) + 
  geom_histogram(mapping = aes(x = cut2), binwidth = 1)

group_by(na_diamonds, cut2) %>% summarise(n=sum(is.na(cut2)))


#### COVARIACIÓN

# Categoría vs Contínua

ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(color = cut), binwidth = 50)

ggplot(diamonds) + 
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 100)

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) + 
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()

ggplot(data = mpg) + 
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median),
                             y = hwy))


ggplot(data = mpg) + 
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median),
                             y = hwy)) + 
  coord_flip()


# Categoría vs Categoría
ggplot(data = diamonds) + 
  geom_count(mapping = aes(x = cut, y = color))

diamonds %>%
  count(color, cut)

diamonds %>%
  count(color, cut) %>%
  ggplot(mapping = aes(x = cut, y = color)) + 
    geom_tile(mapping = aes(fill = n))

#d3heatmap
#heatmaply

# Contínua vs Contínua

ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price), 
             alpha = 0.01)

install.packages("hexbin")
library(hexbin)

ggplot(data = diamonds) + 
  geom_bin2d(mapping = aes(x = carat, y = price))

ggplot(data = diamonds) + 
  geom_hex(mapping = aes(x = carat, y = price))

diamonds %>%
  filter(carat < 3) %>%
  ggplot(mapping = aes(x = carat, y = price)) + 
    geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)), varwidth = T)

diamonds %>%
  filter(carat < 3) %>%
  ggplot(mapping = aes(x = carat, y = price)) + 
    geom_boxplot(mapping = aes(group = cut_number(carat, 10)))

faithful %>%
  filter(eruptions > 3) %>%
  ggplot(aes(eruptions)) + 
    geom_freqpoly(binwidth = 0.2)

diamonds %>%
  count(cut, clarity) %>%
  ggplot(aes(clarity, cut, fill = n)) +
    geom_tile()

# Relaciones y los patrones
# * ¿Coincidencias?
# * ¿Relaciones que implica el patrón?
# * ¿Fuerza de la relación?
# * ¿Otras variables afectadas?
# * ¿Subgrupos?

ggplot(data = faithful) + 
  geom_point(mapping = aes(x = eruptions, y = waiting))

library(modelr)
mod <- lm(log(price) ~log (carat), data = diamonds)
mod

diamonds_pred <- diamonds %>%
  add_residuals(mod) %>%
  mutate(res = exp(resid))

View(diamonds_pred)

ggplot(data = diamonds_pred) + 
  geom_point(mapping = aes(x = carat, y = resid))

ggplot(data = diamonds_pred) +
  geom_boxplot(mapping = aes(x = cut, y = resid))


#Ejercicio 1
nycflights13::flights %>%
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min/60
  ) %>%
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly(mapping = aes(y = ..density.., color = cancelled))

#Ejercicio 2 

ggplot(diamonds, mapping = aes(x = carat, y = price, size = z))+
  geom_point()

ggplot(diamonds, mapping = aes(x = cut, y = clarity))+
  geom_count()

ggplot(diamonds, mapping = aes(x = cut, y = color))+
  geom_count()

ggplot(diamonds, mapping = aes(x = cut, y = carat))+
  geom_boxplot()

diamonds %>%
  count(cut, color) %>%
  ggplot(aes(color, cut, fill = n)) +
  geom_tile()

ggplot(good_diamonds)+
  geom_point(mapping = aes(x = carat, y = price, 
                           color= cut, shape = clarity ))

#Ejercicio 3
install.packages("ggstance")
library(ggstance)
library(tidyverse)
ggplot(data = mpg, mapping = aes(y = hwy,
                                x = reorder(class, hwy, FUN = median),
                                fill = factor(class)
                                )
       ) + 
  geom_boxploth()

ggplot(data = mpg, mapping = aes(x = hwy,
                                 y = reorder(class, hwy, FUN = median),
                                 fill = factor(class)
                                 )
       )+
geom_boxplot()+
  coord_flip()

#geom_boxploth de ggstance grafica horizontalmente tal como se hayan puesto las 
#aesthetics, coord_flip() cambia simplemente la orientación de las aesthetics, 
#no importa cómo las hayan definido. Entonces con ggstance se ve afectada la 
#generación del boxplot si no se pone la variable correcta en las aesthetics, 
#no toma los puntos.



#Ejercicio 4
install.packages("lvplot")
library(lvplot)

a<-ggplot(data = diamonds,
       mapping = aes(x = cut, y = price)
) + geom_lv(k=4)  

b<-ggplot(data = diamonds,
       mapping = aes(x = cut, y = price)
) + geom_lv() 
library(patchwork)
a+b


#Ejercicio 5
a<-ggplot(diamonds, mapping = aes(x = cut, y = price)) + 
  geom_violin()

b<-ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = price), binwidth = 100)+ 
  facet_wrap(~cut, nrow = 3)

c<-ggplot(diamonds) + 
  geom_freqpoly(mapping = aes(x = price, color = cut), binwidth = 100)
a+b+c

#Ejercicio 6
install.packages("ggbeeswarm")
library(ggbeeswarm)

ggplot(diamonds,aes(cut, price)) + 
  geom_quasirandom()

#Ejercicio 7 
diamonds %>%
  count(color, cut) %>%
  ggplot(mapping = aes(x = cut, y = color)) + 
  geom_tile(mapping = aes(fill = log(n)))

diamonds %>%
  count(color, cut) %>%
  ggplot(mapping = aes(x = color, y = cut)) + 
  geom_tile(mapping = aes(fill = log(n)))

#Ejercicio 8
nycflights13::flights %>%
  count(month, dest) %>%
  ggplot(mapping = aes(x = dest, y = month)) + 
  geom_tile(mapping = aes(fill = n))

#Ejercicio 9
ggplot(diamonds, aes(price, colour = cut_width(carat, 1.0))) +
  geom_freqpoly()

ggplot(diamonds, aes(price, colour = cut_number(carat, 5))) +
  geom_freqpoly()

ggplot(diamonds, aes(carat, colour = cut_width(price, 5000))) +
  geom_freqpoly()

#Ejercicio 10
diamonds %>%
  filter(between(x,2,20)) %>%
  filter(between(y,2,20)) %>%
  filter(between(z,2,20)) %>%
  ggplot(aes(price, x*y*z)) + 
   geom_bin2d()

#Ejercicio 11
diamonds %>%
  ggplot(aes(price, colour = cut)) + 
  geom_freqpoly() + 
  facet_wrap(~cut_number(carat, 5), nrow = 2)

diamonds %>%
  ggplot(aes(price)) + 
  geom_freqpoly() + 
  facet_wrap(cut~cut_number(carat, 5))

#Ejercicio 12
ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = x, y = y)) + 
  coord_cartesian(xlim = c(4,12), ylim = c(4,12))


