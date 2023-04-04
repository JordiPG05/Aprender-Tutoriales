
# -------------
# Preproceso
# ------------- 
# Instalar paquetes si aun no se tienen instalados en la distribución de R
#install.packages("ggplot2");
#install.packages("reshape2"); 
#install.packages("plyr")
#install.packages("scales")
#install.packages("readr")

# Cargar los paquetes que utilizaremos en el ejercicio 
library(ggplot2)
library(reshape2)
library(plyr)
library(scales)
library(readr)

# Fijar directorio de trabajo
# Cambiar el directorio de trabajo donde hemos guardado el archivo
# data-nba-points-per-game-2008.csv, sustituyendo ... por la ruta correspondiente
setwd("...") 

# Cargar el archivo .csv
mydata <- read_csv("data-nba-points-per-game-2008.csv")

# Mostrar el dataframe y los datos
# View(mydata)
# print(mydata)

# -------------
# Proceso
# ------------- 
# Reordenar jugadores por nombre y convertir la variable en factor para asegurar
# ordenado adecuado en el gráfico
mydata$Name <- with(mydata, reorder(Name, PTS))

# Agrupar todas las columnas de las distintas estadísticas en una unica columna
mydata_melted <- melt(mydata)

# Para hacer comparable las distintas estadisticas, escalar las variables
mydata_melted <- ddply(mydata_melted, .(variable), transform, rescale = scale(value))

# -------------
# Postproceso
# -------------
# ggplot() con la funcion geom_tile y un relleno de degradado permite crear mapa de calor
myplot <- ggplot(mydata_melted, aes(variable, Name)) + 
  geom_tile(aes(fill = rescale),  colour = "white") +
  scale_fill_gradient(low = "white", high = "red")

# Editar al gusto el grafico obtenido
myplot + labs(x = "", y = "") +
       scale_x_discrete(expand = c(0, 0)) +
       scale_y_discrete(expand = c(0, 0)) +
       theme(legend.position = "none",  axis.ticks = element_blank())

