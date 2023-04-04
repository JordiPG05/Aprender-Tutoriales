
# ------------- 
# Preproceso
# ------------- 
# Instalar paquetes si aun no se tienen instalados en la distribución de R
#install.packages("ggplot2");
#install.packages("lubridate"); 
#install.packages("plyr")

# Cargar los paquetes que utilizaremos en el ejercicio 
library(ggplot2)
library(lubridate)
library(plyr)

# Definir funcion que se usara en la representacion grafica para dividir la region de
# plotting y anadir varios graficos simultaneamente
vplayout <-function(x,y) +
  viewport(layout.pos.row=x, layout.pos.col=y)

# ------------- 
# Proceso + Postproceso
# ------------- 
# Crear vectores que contengan acronimos y nombres de todos los equipos de la NBA
team_code_2008 = c("ATL", "BOS", "CHA", "CHI", "CLE", 
                   "DAL", "DEN", "DET", "GSW", "HOU", 
                   "IND", "LAC", "LAL", "MEM", "MIA", 
                   "MIL", "MIN", "NJN", "NOH", "NYK", 
                   "OKC", "ORL", "PHI", "PHX", "POR", 
                   "SAC", "SAS", "TOR", "UTA", "WAS") 
team_name_2008 = c("Atlanta Hawks", "Boston Celtics", 
                   "Charlotte Hornets", "Chicago Bulls", 
                   "Cleveland Cavaliers", "Dallas Mavericks", 
                   "Denver Nuggets", "Detroit Pistons", 
                   "Golden State Warriors", "Houston Rockets", 
                   "Indiana Pacers", "Los Angeles Clippers", 
                   "Los Angeles Lakers", "Memphis Grizzlies", 
                   "Miami Heat", "Milwaukkee Bucks", 
                   "Minnesota Timberwolves", "New Jersey Nets", 
                   "New Orleans Hornets", "New York Knicks", 
                   "Oklahoma City Thunder", "Orlando Magic", 
                   "Philadelphia 76ers", "Phoenix Suns", 
                   "Portland Trail Blazers", "Sacramento Kings", 
                   "San Antonio Spurs", "Toronto Raptors", 
                   "Utah Jazz", "Washington Wizards")

# Crear variables de fecha y tiempo del partido en formato Año-Mes-Dia
# y Minutos Segundos, respectivamente
lakers$date <-ymd(lakers$date)
lakers$time <- ms(lakers$time)
# Convertir el tiempo en duración del partido, transformando 
# Minutos Segundos en Segundos
lakers$time <- as.duration(lakers$time)
# En función del periodo correspondiente, crear variable gametime 
# que recoge duración acumulada de cada periodo
# de acuerdo a cómo se define un periodo en la NBA de 12 minutos
lakers$gametime <- dminutes(c(12,24,36,48,53)[lakers$period]) 
# Restar duración restante a duración total del periodo para determinar 
#tiempo acumulado desde 0 de la acción
lakers$gametime <- lakers$gametime - as.duration(lakers$time)
# Crar variable minutos correspondiente a los segundos de la acción
lakers$minutes <- as.numeric(seconds(lakers$gametime))/60

# Filtrar acciones o jugadas donde no este definido equipo que posee balon
lakers_games <- lakers[(lakers$team !="OFF"),]

# Contar partidos de la temporada
gamedate <- unique(lakers_games$date)

# Bucle for de acciones a aplicar a cada partido
for (igame in seq_along(gamedate))
{
  # Cargar el partido correspondiente al indice igame
 this_game <- lakers_games[lakers_games$date == gamedate[igame],]
  # Calcular el resultado en cada jugada como suma acumulada de 
  # puntos hasta la jugada 
 this_game_scores <- ddply(this_game, "team", transform, 
							score = cumsum(points))
  # Identificar equipo 1 (siempre Los Angeles Lakers) y equipo 2 (oponente)
 first_team_name <- team_name_2008[which(team_code_2008 == "LAL")]
 second_team_name <- team_name_2008[which(team_code_2008 == this_game_scores$opponent[1])]
  # Calcular resultado final como valor maximo de resultados a lo largo del partido
 first_team_score <- max(this_game_scores[(this_game_scores$team == "LAL"), 
										"score"])
 second_team_score <- max(this_game_scores[(this_game_scores$team == 
											this_game_scores$opponent[1]),
											"score"])
  
  # Crear resumen informativo del partido, que sera utilizado en titulo del grafico
 this_game_summary_text <- paste(gsub(" UTC", "", ymd(this_game_scores$date)[1]), 
                                 ". ", first_team_name, " ", this_game_scores$game_type[1], 
                                 " (", first_team_score, ") vs ", second_team_name, " ", 
                                 "(", second_team_score, ")", sep = "")
  # Crear visualizacion
 myplot <- ggplot() +
   geom_step(data = subset(this_game_scores, team == "LAL"), 
             aes(x = minutes, y = score), 
				size = 0.75, colour = "purple") +
   geom_step(data = subset(this_game_scores, 
							team == this_game_scores$opponent[1]), 
             aes(x = minutes, y = score), size = 0.75, colour = "darkgrey") +
   geom_segment(x = 1.0, xend = 3.0, y = max(this_game_scores$score) - 5,
                yend = max(this_game_scores$score) - 5,
                colour = "purple", size = 0.75) +
   annotate("text", x = max(this_game$minutes), 
					y = first_team_score, 
					colour = "purple", size = 5, 
					label = first_team_score, hjust = 0) +
   annotate("text", x = max(this_game$minutes), 
					y = second_team_score, 
					colour = "darkgrey", size = 5, 
					label = second_team_score, hjust = 0) +
   annotate("text", x = 4, 
			y = max(this_game_scores$score) - 5,
			colour = "#a9a9a9", size = 5, 
			label = first_team_name, hjust = 0) +
   annotate("text", x = 4, 
			y = max(this_game_scores$score) - 12, 
			colour = "#a9a9a9", size = 5, 
			label = second_team_name, hjust = 0) +
   geom_rect(aes(xmin = 1.0, xmax = 3.0, 
			ymin = max(this_game_scores$score) - 5,
			ymax = max(this_game_scores$score) - 5),
			color = "purple", size = 0.75) +
   geom_rect(aes(xmin = 1.0, xmax = 3.0, 
			ymin = max(this_game_scores$score) - 12,
			ymax = max(this_game_scores$score) - 12), 
			color = "darkgrey", size = 0.75) +
   ggtitle(this_game_summary_text) +
   xlab("Minutos del partido") +
   ylab("Resultado") +
   theme_bw() + 
   theme(axis.ticks = element_blank(),
         axis.title.x = element_text(size = 16, color = "#a9a9a9"),
         axis.title.y = element_text(size = 16, color = "#a9a9a9"),
         axis.text.x = element_text(size = 12, color = "#a9a9a9"),
         axis.text.y = element_text(size = 12, color = "#a9a9a9"),
         axis.line = element_line(colour = "#a9a9a9")
         ) 
  
}

myplot
