install.packages("devtools")
install.packages("xlsx")
install.packages("tidyverse")
install.packages("ggrepel")
devtools::install_github("JaseZiv/worldfootballR", ref = "main")


library(worldfootballR)
library(tidyverse)
library(xlsx)
library(dplyr)


league_url <- fb_league_urls(country = "ESP", gender = "M",season_end_year=2023, tier = "1st")
teams <- fb_teams_urls(league_url)

SHOOTING_DF <- fb_team_player_stats(team_urls= teams, stat_type= "shooting")
PASSING_DF <- fb_team_player_stats(team_urls= teams, stat_type= "passing")
PASSING_TYPE_DF <- fb_team_player_stats(team_urls= teams, stat_type= "passing_types")
POSESSION_DF <- fb_team_player_stats(team_urls= teams, stat_type= "possession")
GCA_DF <- fb_team_player_stats(team_urls= teams, stat_type= "gca")
MISC_DF <- fb_team_player_stats(team_urls= teams, stat_type= "misc")
STANDARD_DF <- fb_team_player_stats(team_urls= teams, stat_type= "standard")




dataset<- merge(x = SHOOTING_DF, y = PASSING_DF, by = c("Player", "Squad",'Comp','Nation','Pos','Age','Season','Mins_Per_90','PlayerURL'),all.x = TRUE)
dataset<- merge(x = dataset, y = PASSING_TYPE_DF, by = c("Player", "Squad",'Comp','Nation','Pos','Age','Season','Mins_Per_90','PlayerURL'))
dataset<- merge(x = dataset, y = POSESSION_DF, by = c("Player", "Squad",'Comp','Nation','Pos','Age','Season','Mins_Per_90','PlayerURL'))
dataset<- merge(x = dataset, y = GCA_DF, by = c("Player", "Squad",'Comp','Nation','Pos','Age','Season','Mins_Per_90','PlayerURL'))
dataset<- merge(x = dataset, y = MISC_DF, by = c("Player", "Squad",'Comp','Nation','Pos','Age','Season','Mins_Per_90','PlayerURL'))
dataset<- merge(x = dataset, y = STANDARD_DF, by = c("Player", "Squad",'Comp','Nation','Pos','Age','PlayerURL'))

colnames(dataset)


dataset$Touches90<- dataset$Touches_Touches/dataset$Mins_Per_90
dataset$KP90<- dataset$KP/dataset$Mins_Per_90
dataset$Recov90<- dataset$Recov/dataset$Mins_Per_90
dataset$FinalThirdPasses90<- dataset$Final_Third/dataset$Mins_Per_90

dataset %>% distinct()

metrics <- c("Player", "Squad", "Mins_Per_90", "G_minus_xG_Expected", "SoT_per_90_Standard", "KP90", 
             "FinalThirdPasses90", "SCA90_SCA", "Cmp_Total", 
             "Touches90", "Recov90")
midfielders_df <- dataset %>%
  filter(Pos == "MF") %>% select(metrics)
midfielders_df <- midfielders_df[midfielders_df$Mins_Per_90 > 1,]
cat("N?mero de centrocampistas analizados:", nrow(midfielders_df))

# ---------------------------------------------------
# ------------ Vsualizaci?n gr?fica -----------------
# --------------------------------------------------
library(beeswarm)

plot_swarmplot <- function(
    data, metric, metric_text, player, player_text, leg_pos){
  df_metric <- data %>%
    select(Player, metric) %>%
    mutate(p = round(100*percent_rank(data[,metric])))
  colors <- c("red","coral","gold","lightgreen","green")
  df_metric$color <- ifelse(
    df_metric$p < 20, colors[1], 
    ifelse(df_metric$p < 40, colors[2], 
           ifelse(df_metric$p < 60, colors[3],
                  ifelse(df_metric$p < 80, colors[4], colors[5]))))
  df_metric[df_metric$Player == player, "color"] = "darkblue"
  beeswarm(df_metric[,metric], 
           horizontal = TRUE,
           bty="n", pch=21, cex = 0.7,
           pwbg = df_metric$color, 
           method = "swarm")
  legend(leg_pos, 
         legend=c("0-20","20-40","40-60","60-80","80-100"), 
         pch=16, horiz=TRUE, cex = 0.5, inset = .02,
         col = colors,
         title="Rango del percentil")
  title("Centrocampistas 5 grandes Ligas", 
        xlab = metric_text)
  title(paste("\n \n", player_text), col.main="darkblue")
}


plot_swarmplot(
  midfielders_df, "KP90", "Pases clave por 90'", 
  "Aleix García", "Aleix García (GIR)", "bottomright")


# ----------------------------------------------
# ------------------- Gr?fico de Radar ---------
# ----------------------------------------------

## Definici?n de la funci?n

library(fmsb)
radar_plot <- function(
    data, players, colors_line, colors_fill, metrics, radar_title){
  # Accedemos a las stats de los jugadores
  data_players <- data[data$Player %in% players, ]
  rownames(data_players) <- data_players[, "Player"]
  data_players <- data_players[,-c(1)]
  # Calculamos extremos (p5 y p95) del data total
  top_value <- sapply(data[,-c(1)], quantile, probs=0.95)
  down_value <- sapply(data[,-c(1)], quantile, probs=0.05)
  stats <- rbind(top_value, down_value)
  colnames(stats) <- metrics
  # Modificamos el DF de partida
  for (i in 1: ncol(data_players)){
    for (j in players){
      if (data_players[j,i] > top_value[i]){
        data_players[j,i] <- top_value[i]
      }
      if (data_players[j,i] < down_value[i]){
        data_players[j,i] <- down_value[i]
      }
    }
  }
  # Concatenamos DF: umbrales y estad?sticas jugadores
  all_df <- rbind(stats, data_players)
  # Radar
  radarchart(all_df,cglty = 1, seg = 5, title = radar_title, pcol = colors_line, 
             pfcol = colors_fill, plwd = 2, plty=1, vlcex=0.6)
  legend(x = 0.7, y = 1.35, inset = .02, 
         legend = rownames(all_df[-c(1,2),]), bty = 'n', pch = 20, 
         col = colors_line, cex = 0.6, pt.cex = 1)
}


## Ejemplo: RADAR - centrocampistas de La Liga

# Seleccionamos variables y renombramos
data_MF <- midfielders_df %>% select(-c(2,3))
metrics <- c( "G-xG", "SoT/90", "KP/90", 
             "FinalThirdPasses/90", "SCA/90", "Cmp Total", 
             "Touches/90", "Recov/90")

colnames(data_MF) <- c("Player", metrics)

# Definimos colores
library(scales)

colors_fill <- c(alpha("tomato", 0.1), 
                 alpha("lightgreen", 0.1), 
                 alpha("blue", 0.1))
colors_line <- c(alpha("tomato", 0.9), 
                 alpha("darkgreen", 0.5), 
                 alpha("royalblue", 0.9))

# Seleccionamos jugadores
players <- c("Sergio Busquets")

# Radar
radar_plot(
  data_MF, players, colors_line, colors_fill, 
  metrics, "Sergio Busquets")

write.xlsx(dataset, file = "LALIGA_23_9_22.xlsx",
           sheetName = "pLAYERS", append = FALSE)





