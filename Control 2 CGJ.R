
library("dplyr")
library(ggplot2)
setwd("C:/Users/charl/OneDrive/Desktop/Programación Diplomado/Control 2")
general <- read.csv("generalinfo.csv")
location <- read.csv("location.csv")
#P1a) (1pt) Basándose en a tabla general, ¿cuántos restaurants (id's) distintos hay en total?
summary(general)
max(general$id_restaurant)

#Basándose solo en ID existen 9590 restaurants distintos

#P1b) (1pt)¿En cuántos tipos de comida diferentes se clasifican los restaurants?
general%>%count(,food_type)%>%arrange(desc(n))

#Se clasifican en 145 tipos de comidas

#P1c) (2pt) ¿Cuántas ciudades distintas considera el sondeo?

location%>%count(city)%>%arrange(desc(n))
  
#Considera 167 ciudades distintas

#P1d) (2pt) ¿Indique el tipo de comida y las ciudades donde se encuentra el restaurant "great wall restaurant"?

location<-rename(location,id_restaurant=id_rest)
location
tabla<-general%>%full_join(location,by="id_restaurant")
tabla%>%filter(,label=="great wall restaurant")%>%select(label,food_type,city)

#P1e) (2pt) ¿Cuántos restaurantes de la ciudad de san francisco tienen calificación mayor o igual a 3.8 
#y venden comida vegetariana (vegetarian)?

tabla%>%filter(,review>=3.8)%>%filter(,food_type=="vegetarian")%>%count()


#3 restaurantes venden comida vegetariana con review mayor igual a 3.8

#Preguntas 1.2
#P2a) (2pt) Sin considerar San Francisco, ¿cuál es la ciudad con mayor cantidad de restaurantes sondeados?

tabla%>%count(,city)%>%arrange(n)



no_sf<-tabla %>%filter(city != "san francisco")%>%group_by(city)%>%summarise(cantidad_restaurantes=n())%>%arrange(desc(cantidad_restaurantes))
ciudad_mas_restaurantes<-no_sf%>%slice(1)
print(paste("La ciudad con mas restaurantes luego de san francisco es",ciudad_mas_restaurantes$city))

#P2b) (1pt) ¿Cuáles son los 3 tipos de comida ofrecido más comunes ?

creacion<-tabla%>%group_by(food_type)%>%summarise(suma_tipos=n())%>%arrange(desc(suma_tipos))
tres_top<-creacion%>%slice(1:3)
print(tres_top$food_type)


#P2c) (2pt) Sin considerar San Francisco, ¿Cuáles son las 3 ciudades con mayor cantidad de 
#restaurants que ofrecen comido tipo japanese?

no_sf2<-tabla %>%filter(city != "san francisco")%>%filter(food_type=="japanese")%>%group_by(city)%>%summarise(suma_rest=n())%>%arrange(desc(suma_rest))
no_sf2 %>%slice(1:3)


#P2d) (2pt) Usted decide viajar a una de las ciudades en cuestión, para ello calcula el promedio de las valoraciones medias 
#(promedio de review) por cada ciudad, y escoje aquella con mayor review promedio. ¿Qué ciudad escoge?

mejor_promedio<-tabla %>% group_by(city)%>%summarise(promedio=mean(review,na.rm=T))%>%arrange(desc(promedio))

#P2e) (2pt) Cuál es la ciudad con mejor valoración promedio de restaurantes tipo "barbeque"

prom_barbeque<-tabla %>% group_by(city)%>%filter(food_type=="barbeque")%>%summarise(promedio=mean(review,na.rm=T))%>%arrange(desc(promedio))


#P3a) (4pt) En la pregunta 1d), se pudo observar que un mismo restaurant puede estar presente en más de una ciudad. 
#¿Cuántos restaurants tienen esta característica, es decir están en más de una ciudad distinta ? De ser de utilidad 
#puede investigar y utilizar la función distinct().

#Son 167 ciudades distintas 

tabla <- tabla%>%mutate(label = tolower(label)) %>%mutate(label = gsub("'","", label)) %>%mutate(label = gsub(" ","_",label)) %>%
  mutate(label = chartr("áéíóú", "aeiou", label))

rest_ciudad<-tabla%>%group_by(label)%>%summarise(suma_tipos=n_distinct(city))%>%filter(suma_tipos>1)

#P3b) (2pt) ¿Cuál es el restaurant que tiene presencia en la mayor cantidad de ciudades distintas?
#¿En cuántas ciudades está presente?

rest_repetido <- tabla %>% group_by(label)%>% summarise(distintas=n_distinct(city))%>% arrange(desc(distintas))
rest_mas_distinto<-rest_repetido%>%slice(1)

print(paste("El restaurant que mayor presencia tiene es el",rest_mas_distinto$label,"con",rest_mas_distinto$distintas,"locaciones en diferentes ciudades"))

#P3c) (5pt) Diremos que un restaurant posee sucursales si en la tabla general existe más de un registro con el mismo label

grafico<- general%>%group_by(label)%>% summarise(suc_repetidas=n())%>%arrange(desc(suc_repetidas))%>%head(15)
#restaurantes ordenados segun su cantidad de sucursales,

library(ggplot2)
grafico_descenso<-grafico[order(grafico$suc_repetidas,decreasing=TRUE),]

ggplot(grafico,aes(x=reorder(label,-suc_repetidas),y=suc_repetidas, fill=suc_repetidas))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 80,vjust=1, hjust = 1,size=10),axis.title=element_text(size=10),title=element_text(size=15))+
  labs(x="Restaurante",y="Numero de repeticiones",title="Top 15 restaurantes repetidos")+
  geom_label(aes(label=suc_repetidas),vjust=0,size=4,color="white")+
  scale_fill_gradient( high = "blue",low="red" )


#aux<-tabla%>%group_by(food_type)%>%summarise(distintas=n_distinct(city))%>%arrange(desc(distintas))


resumen <- tabla %>% group_by(city, food_type) %>% summarise(n_rest = n(),review_prom = mean(review, na.rm = TRUE)) %>% mutate(total_rest = sum(n_rest),review_prom_city = mean(review_prom, na.rm = TRUE))



#P4a) (4pts) Genere una tabla llamada resumen, que contenga la siguiente información:
  
#  city: Ciudad
#food_type: Tipo de comida
#n_rest: Cantidad de restaurantes por cada ciudad y tipo de comida. OK
#review_prom: Valoración promedio por cada ciudad y tipo de comida.
#total_rest: Total de restaurantes por cada ciudad (se puede repetir el valor por cada tipo de comida).
#review_prom_city: Valoración promedio de los restaurantes por cada ciudad (se puede repetir el valor por cada tipo de comida).
#Su tabla deberá tener la siguiente estructura (la imgen sólo muestra los primeros registros):

resumen2 <- tabla %>% group_by(city, food_type)%>% summarise(n_rest=n(),review_prom=mean(review,rm.na=T))%>% 
  mutate(total_rest=sum(n_rest))%>% arrange(desc(n_rest)) # OK

resumen3 <- tabla %>%
  group_by(city) %>%mutate(review_prom_city = mean(review, na.rm = TRUE)  )

resumen <- resumen3 %>%
  left_join(resumen2 %>% select(city, total_rest, review_prom), by = "city")

resumen3<-resumen3$label <- NULL

nuevo_resumen <- resumen3 %>%
  group_by(city) %>%
  mutate(promedio_ciudad = mean(review_prom, na.rm = TRUE))

resumen<- left_join(resumen2,by="city",relationship- "marry-to-one")

prueba <- general %>%
  left_join(location, by = c("id_restaurant"))%>% 
  group_by(city,food_type)%>%
  summarise(n_rest = n(), 
            review_prom_city = mean(review, rm.na = T))%>%
  mutate(total_rest = sum(n_rest))%>%
  arrange(desc(n_rest))

prueba2 <- tabla%>% 
  group_by(city)%>%mutate(review_prom_city=mean(review,na.rm=T))%>%distinct(city,review_prom_city)




#total por ciudad, 
                                            
#P4b) (2pts) Basado en la tabla anterior, construya dos nuevas columnas llamadas density_food_type 
#y ratio_review que contengan la siguiente información:
#density_food_type: Representa el cuociente entre le total de restaurants por tipo de comida y ciudad,
#respecto del total de restaurantes de la ciudad. (n_rest/total_rest)
#ratio_review: Representa el cociente entre a valoración del restaurant por tipo de comida y ciudad, 
#respecto de la valoración promedio de los resturants de la misma ciudad. (review_prom/review_prom_city)

resumen <-resumen%>%mutate(density_food_type=n_rest/total_rest,ratio_review=review_prom/review_prom_city)

#P4c) (3pts) Mediante un gráfico de dispersión, muestre la relación entre density_food_type y ratio_review. 
#Investigue sobre el parámetro alpha dentro de la capa geométrica para una mejor visualizaciónd de los puntos. 
#Adicionalmente añada una curva de tendencia y, con base en él, indique si cabe la posibilidad de establecer algún
#tipo de dependencia entre density_food_type y ratio_review
ggplot(resumen, aes(x = density_food_type, y = ratio_review)) +
  geom_point(alpha = 0.3) +  # Configurar transparencia de los puntos
  geom_smooth(method="gam",se=F, na.rm=T,color="red") +  # Agregar curva de tendencia
  labs(x = "Density Food Type", y = "Ratio Review", title = "Relación entre Density Food Type y Ratio Review") +
  theme_minimal()

head(resumen,20)

#Preguntas 1.5
#P5a) (3pts) En la tabla resumen creada en P4a), genere una nueva columna llamada type_review, que contenga "review alto",
#si ratio_review >= 1 y "review bajo" ratio_review < 1. ¿Qué indica esta variable? Comente.


