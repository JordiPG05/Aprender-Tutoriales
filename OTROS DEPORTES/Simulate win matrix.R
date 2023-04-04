
# ------------- 
# Preproceso
# -------------  
# Instalar paquetes si aun no se tienen instalados en la distribución de R
#install.packages("lattice");
#install.packages("ggplot2");

# Cargar los paquetes que utilizaremos en el ejercicio 
library(lattice)

# ------------- 
# Proceso
# -------------   
# Crear función que calcula probabilidad de victoria basada en 
# simulación niterations veces con
# distribuciones binomiales negativas
simulator <- function(home_mean, away_mean, niterations){
  # Para asegurar repetitibilidad, inicializar simulación con semilla 1234
  set.seed(1234)
  # Inicializar variables away_game_score, home_game_score y home_win,
  # así como contador i
  away_game_score <- numeric(niterations)
  home_game_score <- numeric(niterations)
  home_win <- numeric(niterations)
  i <- 1
  # Comenzar bucle que calcula carreras probables del equipo 
  # visitante y local
  # de acuerdo con distribución binomial negativa de media 4.
  while (i < niterations + 1){
    away_game_score[i] <- rnbinom(1, mu = away_mean, size = 4)
    home_game_score[i] <- rnbinom(1, mu = home_mean, size = 4)
    # En función de las carreras simuladas en la iteración, 
	# se suma victoria y sigue el bucle con contador +1 
    # o sólo se sigue el bucle con contador +1
    if(away_game_score[i] > home_game_score[i]) 
      home_win[i] <- 1
    if(away_game_score[i] > home_game_score[i] ||
       away_game_score[i] < home_game_score[i]) i <- i + 1 
  }
  # Calcular probabilidad de victoria como cociente de 
  # victorias totales en la simulación
  myprob <- sum(home_win)/niterations
}

# Fijar el valor de numero de iteraciones para reducir el 
# coste computacional en este test. Se puede jugar con diferentes ordenes
# de magnitud para ver su efecto
niterations <- 10000

# Crear matriz de probabilidad, dejando vacía la diagonal 
# principal y asignando valores a cada elemento
# de la matriz de acuerdo con valor i,j de fila y columna.
probability_matrix <- matrix(data = NA, nrow = 9, ncol = 9, 
							dimnames = list(c(as.character(1:9)),
							c(as.character(1:9))))
for (index_home in 1:9)
  for(index_away in 1:9)
    if (index_home != index_away){
      probability_matrix[index_home, index_away] <- simulator(index_home,
												index_away, niterations)
    }

# ------------- 
# Postproceso
# ------------- 
# Fijar directorio de trabajo
# Cambiar el directorio de trabajo donde hemos queremos 
# guardar el .pdf de salida,
# sustituyendo ... por la ruta correspondiente
#setwd("...")
setwd("G:/ix-blog/xx-work/master/modulo-04/r/simulate-team-win-prob") 

# Definir .pdf en el que se guardará la información generada tras simulación
pdf(file = "fig_sports_analytics_prob_matrix.pdf", width = 8.5, height = 8.5)
# Crear variables x e y para recorrer toda la matriz de probabilidad
x <- rep(1:nrow(probability_matrix), times = ncol(probability_matrix))
y <- NULL
for (i in 1:ncol(probability_matrix)) y <- c(y, 
										rep(i, times = nrow(probability_matrix)))

# Construir matriz a mostrar en .pdf, girada con respecto a la matriz de probabilidades
matrix_text <- sprintf("%0.2f", 
						as.numeric(probability_matrix))
text_data_frame <- data.frame(x, y, matrix_text)
text_data_frame$matrix_text <- as.character(text_data_frame$matrix_text)
text_data_frame$matrix_text <- ifelse((text_data_frame$matrix_text == "NA"),
								NA, text_data_frame$matrix_text)
text_data_frame <- na.omit(text_data_frame)
print(levelplot(probability_matrix, cuts = 25, tick.number = 10, 
                col.regions = colorRampPalette(c("violet", "white", "light blue")), 
                xlab = "Carreras esperadas por equipo visitante",
                ylab = "Carreras esperadas por equipo local",
                panel = function(...){
                  panel.levelplot(...)
                  panel.text(text_data_frame$x, text_data_frame$y,
                             labels = text_data_frame$matrix_text)
                }))
# Desactivar archivo para poder ser abierto
dev.off()
