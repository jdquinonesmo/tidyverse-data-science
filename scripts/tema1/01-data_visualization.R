############################################
##VERSIONES CON LAS QUE SE HIZO ESTA PARTE##
##                2021-01-28              ##
############################################
# v ggplot2 3.3.2     v purrr   0.3.4
# v tibble  3.0.1     v dplyr   1.0.2
# v tidyr   1.1.2     v stringr 1.4.0
# v readr   1.3.1     v forcats 0.5.0
# -- Conflicts ------------------------------------------ tidyverse_conflicts() --
# x dplyr::filter() masks stats::filter()
# x dplyr::lag()    masks stats::lag()
library(tidyverse)

#PREGUNTAS:
#?LOS COCHES CON MOTOR M?S GRANDE CONSUMEN M?S COMBUSTIBLE?
#?LA RELACI?N CONSUMO/TAMA?O ES LINEAL??NO ES LINEAL??POSITIVA O NEGATIVA?

View(mpg) #mpg es una base de datos que ya viene con el paquete ggplot2
help(mpg) # entrega informaci?n del dataset 
summary(mpg)
#visualizaci?n
#Se representar? en las abscisas displ: tama?o del motor en litros (cilindraje)
#En las ordenadas estar? hwy: millas recorridas por gal?n en una carretera.
ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ, y=hwy)) # crea una nube de puntos o scatterplot
#cada + que a?ada a la gr?fica es una capa.

## Tarea 1
ggplot(data=mpg)+geom_point(mapping=aes(x=cyl, y=hwy))
ggplot(data=mpg)+geom_point(mapping=aes(x=cyl, y=cty))


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = class, y = drv))



## Color de los puntos. Clasificados por tipo de veh?culo "class"
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

#Tama?o de los puntos (conviene que sea num?rico)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

#Transparencia de los puntos
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

#Forma de los puntos (solo permite 6 formas a la vez)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

#Elecci?n manual de est?ticas
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "red")
# color = nombre del color en formato string
# size = tama?o del punto en mm
# shape = forma del punto con n?meros desde el 0 al 25
# 0 - 14: son formas huecas y por tanto solo se le puede cambiar el color
# 15- 20: son formas rellenas de color, por tanto se le puede cambiar el color
# 21 - 25: son formas con borde y relleno, y se les puede cambiar el color (borde) y el fill (relleno)

## Ver formas
d=data.frame(p=c(0:25))
ggplot() +
  scale_y_continuous(name="") +
  scale_x_continuous(name="") +
  scale_shape_identity() +
  geom_point(data=d, mapping=aes(x=p%%16, y=p%/%16, shape=p), size=5, fill="yellow") +
  geom_text(data=d, mapping=aes(x=p%%16, y=p%/%16+0.25, label=p), size=3)



ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), 
             shape = 23, size = 5, color = "black", 
             fill = 'red')

ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ, y = hwy, color = displ<4))

## Tarea 2

ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ, y = hwy, color = hwy))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ, y = hwy, size = hwy))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ, y = hwy, size = year))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ, y = hwy, color = year))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ, y = hwy, color = year, size=year))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ, y = hwy, fill=drv),shape=23,size=4,
              color="white", stroke =1.6 ) #Stroke es para cambiar el grosor de los bordes de las formas con bordes.


##FACETS: divide el gr?fico general en varias gr?ficas, clasificadas seg?n una variable discreta elegida 
# facet_wrap(~<FORMULA_VARIABLE>): la variable debe ser discreta
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~class, nrow = 3,as.table = T)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~cyl, nrow = 1)

# facet_grid(<FORMULA_VARIABLE1>~<FORMULA_VARIABLE2>): divide las gr?ficas seg?n
#clasificaci?n con dos variables (factorial)
ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y = hwy)) +
  facet_grid(drv~cyl)

#Tarea 3
#Punto 2
ggplot(data=mpg)+
geom_point(mapping=aes(x=drv, y=cyl))
unique(mpg$cyl) #Ver niveles de un factor o valores diferentes en un vector.

#Punto 3
ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y = hwy)) +
  facet_grid(.~cyl) #con el punto el faced_grid es igual que el _wrap

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y = hwy)) +
  facet_grid(drv~.)
#Punto 5
?facet_wrap

#Diferentes geometr?as
ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ, y =hwy))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x=displ, y = hwy))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x=displ, y = hwy, linetype = drv))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ, y=hwy, color = drv)) +
  geom_smooth(mapping = aes(x=displ, y = hwy, linetype = drv, color = drv))

?geom_smooth


ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x=displ, y=hwy))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x=displ, y=hwy, group = drv, color = drv),
              show.legend = T)

ggplot(data = mpg, mapping = aes(x=displ, y=hwy)) +
  geom_point(mapping = aes(shape = class)) + 
  geom_smooth(mapping = aes(color = drv))


ggplot(data = mpg, mapping = aes(x=displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "suv"), se = F)

ggplot(data = mpg, mapping = aes(x=displ, y = hwy,color = drv)) + 
  geom_point() + 
  geom_smooth( se = F)


#Ejercicio 4
ggplot(data = mpg, mapping = aes(x=displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ, y = hwy)) + 
  geom_smooth(mapping = aes(x=displ, y = hwy))

#Ejercicio 5
ggplot(data = mpg, mapping = aes(x=displ, y = hwy)) + 
  geom_point() + 
  geom_smooth(se=F)

#Ejercicio 6
ggplot(data = mpg, mapping = aes(x=displ, y = hwy)) + 
  geom_point() + 
  geom_smooth(mapping = aes(group=drv), se=F)

ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ, y=hwy))+
  geom_smooth(mapping=aes(x=displ, y=hwy,group=drv),se=F)

#Ejercicio 7
ggplot(data = mpg, mapping = aes(x=displ, y = hwy, col=drv)) + 
  geom_point() + 
  geom_smooth( se=F)

#Ejercicio 8
ggplot(data = mpg, mapping = aes(x=displ, y = hwy)) + 
  geom_point(mapping = aes(col=drv, shape = drv)) + 
  geom_smooth( se=F)

ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ, y=hwy, shape=drv, color=drv))+
  geom_smooth(mapping=aes(x=displ, y=hwy),se=F)

#Ejercicio 9
ggplot(data = mpg, mapping = aes(x=displ, y = hwy) ) + 
  geom_point(mapping = aes(col=drv, shape = drv)) + 
  geom_smooth(mapping = aes(linetype = drv), se=F)

#Ejercicio 10
ggplot(data = mpg, mapping = aes(x=displ, y = hwy) ) + 
  geom_point(mapping = aes(fill = drv), size = 4, 
             shape = 23, col = "white", stroke = 2) 


## Ejemplo del dataset de diamantes
View(diamonds)

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

?geom_bar

ggplot(data = diamonds)+
  stat_count(mapping = aes(x=cut))


demo_diamonds <- tribble(
  ~cut,       ~freqs,
  "Fair",       1610,
  "Good",       4906,
  "Very Good", 12082,
  "Premium",   13791,
  "Ideal",     21551
)

ggplot(data = demo_diamonds) + 
  geom_bar(mapping = aes(x=cut, y = freqs), 
           stat = "identity")

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))

ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max, 
    fun.y = median
  )
#### TAREA 5 ####


?geom_col

ggplot(data = diamonds) + 
  geom_col(mapping = aes(x = cut, y = depth))

?stat_smooth

#punto 5.)
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop..)) #este c?digo no cierra la suma=1 en proporciones

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color, y = ..prop.., group=1))


#Colores y formas de los gráficos


ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color))

## position = "identity"
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 0.2, position = "identity")

ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")


## position = "fill"
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(position = "fill")

## position = "dodge"
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(position = "dodge")


## Volvemos al scatterplot
## position = "jitter"
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point( position = "jitter" )

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_jitter()

?position_stack
?position_identity
?position_fill
?position_dodge
?position_jitter


# Sistemas de Coordenadas

#coord_flip() -> cambia los papeles de x e y
ggplot(data = mpg, mapping = aes(x=class, y = hwy)) + 
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x=class, y = hwy)) + 
  geom_boxplot() + 
  coord_flip()

#coord_quickmap() -> configura el aspect ratio para mapas

usa <- map_data("usa")

ggplot(usa, aes(long, lat, group = group)) + 
  geom_polygon(fill = "blue", color = "white") + 
  coord_quickmap()

italy <- map_data("italy")

ggplot(italy, aes(long, lat, group = group)) + 
  geom_polygon(fill = "blue", color = "white") + 
  coord_quickmap()

#coord_polar()

ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut),
    show.legend = F,
    width = 1
  ) +
  theme(aspect.ratio = 1) + 
  labs(x = NULL, y = NULL) + 
  coord_polar()


ggplot(data = mpg, mapping = aes(x = cty, y = hwy )) + 
  geom_point()+
  geom_abline() + 
  coord_fixed()

?geom_jitter
?geom_boxplot
?labs
?geom_abline

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color))+ 
  theme(aspect.ratio = 1) + 
  labs(x = NULL, y = NULL) + 
  coord_polar()


# Gramática por capas de ggplot2

#ggplot(data = <DATA_FRAME>) +
#  <GEOM_FUNCTION>(
#                  mapping = aes(<MAPPINGS>),
#                  stat = <STAT>,
#                  position = <POSITION>
#                 ) + 
#   <COORDINATE_FUNCTION>() + 
#   <FACET_FUNCTION>()

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = clarity, fill = clarity, y = ..count..)) +
  coord_polar() +
  facet_wrap(~cut) +
  labs(x=NULL, y = NULL, title = "Ejemplo final de ggplot con JB",
       caption = "Dos variables cruzadas de diamonds",
       subtitle="Aprender ggplot puede ser hasta divertido ;)")

#############
## TAREA 6 ##
#############

#punto 1) 
library(ggplot2)
ggplot(data = mpg, mapping = aes(x = cty, y = hwy, color=as.factor(cyl))) + 
  geom_point()+
  coord_fixed(1)+
  geom_smooth()+
  geom_abline(intercept = 5)+
  geom_hline(yintercept=10)


#punto 2) 
?geom_jitter

#punto 3)
ggplot(mpg, aes(cty, hwy)) +
  geom_point()
ggplot(mpg, aes(cty, hwy)) +
  geom_jitter()
ggplot(mpg, aes(cty, hwy)) +
  geom_count()

#punto 4)

f1<-ggplot(diamonds, aes(cut,depth,color=color))+
  geom_boxplot()
f2<-ggplot(diamonds, aes(cut,depth,color=color))+
  geom_boxplot(position="dodge2")
f3<-ggplot(diamonds, aes(cut,depth,color=color))+
  geom_boxplot(position="dodge")
f4<-ggplot(diamonds, aes(cut,depth,color=color))+
  geom_boxplot(position="identity")
library(cowplot)
plot_grid(f1,f2,f3,f4)

#punto 5)

ggplot(diamonds, aes(cut,fill=color))+
geom_bar()

ggplot(diamonds, aes(cut,fill=color))+
  geom_bar()+
  coord_polar()

#punto 6)
?labs

#punto 7) 
?coord_map

#punto 8) 
?coord_fixed


