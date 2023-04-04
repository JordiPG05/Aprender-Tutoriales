
# -------------
# Preproceso
# ------------- 
# Instalar paquetes si aun no se tienen instalados en la distribución de R
#install.packages("ggplot2");

# Cargar los paquetes que utilizaremos en el ejercicio 
library(ggplot2)

# -------------
# Proceso
# -------------
# Definir los KPIs que queremos representar en nuestro diagrama polar
kpis = c("Posesion", "Tiros por gol\n a favor", 
			"Tiros por gol \n en contra", 
         "Juego directo", "Centros", "Agresividad", 
		 "Inicio \n de juego (Portero)")
percentagedata = c(18.03, -6.27, 11.67, -40.04, 6.19, -9.30, 95.98)
clubname = "Atletico Villalpando"

# Para poder trabajar posteriormente con la transformación polar, 
# necesitamos convertir nuestros datos en una estructura que asocie  
# los nombres de los KPIs y los valores porcentuales
mydata <- structure(
  list(Category = structure(1:7, 
                            .Label = kpis
                            , class = "factor"),
       Percentage = percentagedata
       ),
  .Names = c("Category", "Percentage"),
  row.names = c(NA, 7L),
  class = "data.frame"
)

# -------------
# Postproceso
# -------------
# Construir el gráfico de barras y aplicar transformación polar
ggplot(mydata) +
  geom_bar(aes(x = Category, y = pmin(200, 100 + Percentage)), 
										fill = "#004da8", 
										stat = "identity", width = 1) +
  geom_hline(yintercept = seq(0,200,by = 25), linetype = "dashed", 
								color = "#e9e9e9", size = 0.5) +
  geom_vline(xintercept = seq(0.5, 7.5, by = 1), color = "#c9c9c9", 
								size = 0.5) +
  geom_hline(yintercept = 100, color = "#a9a9a9", size = 0.75) +
  geom_hline(yintercept = 200, color = "#a9a9a9", size = 0.75) +
  coord_polar() +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  labs(title = clubname) +
    theme(plot.title = element_text(hjust = 0.5, size = 20, color = "#a9a9a9"), 
        plot.subtitle = element_text(vjust = 0, hjust = 0.5, size = 12, color = 
										"#a9a9a9"),
        axis.text.y = element_blank(), 
        legend.position = "none", 
        panel.grid = element_blank(),
        axis.text.x = element_blank()) +
  geom_text(aes(x = Category, y = 290, label = sprintf("%s \n %1.2f%%", Category,
														Percentage)), size = 4, 
														color = "#a9a9a9")
