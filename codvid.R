#made by https://github.com/Amyur
library(ggplot2)
library(maps)
library(ggthemes)
library(gganimate)
library(dplyr)

attach(covid2)

days = covid2[5:45]
city = covid2[1:4]

#Processing de data1
ciudad = vector()
countr = vector()
latit = vector()
longit = vector()
for (i in 1:141){
  for (j in 1:41){
    ciudad[j] = city[i, 1]
    countr[j] = city[i, 2]
    latit[j] = city[i, 3]
    longit[j] = city[i, 4]
  }
  
  if(i==1){
    data1 = data.frame("ciudad"=as.character(ciudad), "pais"=as.character(countr), "latitud"=as.numeric(latit), "longitud"=as.numeric(longit))
  }
  else{
    data1 = rbind(data1, data.frame("ciudad"=as.character(ciudad), "pais"=as.character(countr), "latitud"=as.numeric(latit), "longitud"=as.numeric(longit)))
  }
       
}

#preprocessing data2
dat = vector()
for (i in 1:141){
  for (j in 1:41){
    dat[j] = colnames(days)[j]
  }
  
  if(i==1){
    data2 = data.frame("date"=as.character(dat))
  }
  else {
    data2 = rbind(data2, data.frame("date"=as.character(dat)))
  }
  
}

confirm = vector()
for (i in 1:141){
  for (j in 1:41){
    confirm[j] = days[i, j]
  }
  
  if(i==1){
    data3 = data.frame("confirm"=as.numeric(confirm))
  }
  else {
    data3 = rbind(data3, data.frame("confirm"=as.numeric(confirm)))
  }
  
}

#Groupin the data

datos = cbind(data1, data2, data3)

datos$newdate <- strptime(as.character(datos$date), "%m/%d/%y")
datos$txtdate <- format(datos$newdate, "%Y-%m-%d")
datos$txtdate <- as.Date(datos$txtdate)
datos$newdate <- as.Date(datos$newdate)
datos$latitud <- as.numeric(datos$latitud) 
datos$longitud <- as.numeric(datos$longitud) 



world <- ggplot(data = datos) +
  borders("world", colour = "gray90", fill = "gray76") +
  theme_map() + 
  geom_point(aes(x = datos$longitud, y = datos$latitud, size = confirm), colour = "#FF0000", alpha = 0.8) +
  labs(title = "Evolution of covid-19 virus. Date: {frame_time}") +
  transition_time(datos$txtdate) +
  ease_aes("linear") +
  scale_size(range = c(-0.34, 5)) +


p_ani <- animate(world, duration=40, fps=10, detail = 1)

p_ani
anim_save("codvid.gif", animation = last_animation())
#made by https://github.com/Amyur