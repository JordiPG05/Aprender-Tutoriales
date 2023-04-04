install.packages("devtools")
install.packages("xlsx")
install.packages("tidyverse")
install.packages("ggrepel")
install.packages("rlang")
devtools::install_github("JaseZiv/worldfootballR", ref = "main")


library(worldfootballR)
library(tidyverse)
library(xlsx)
library(dplyr)

team_city_url <- "https://fbref.com/en/squads/b8fd03ef/2021-2022/Manchester-City-Stats"
#man_city_wages <- fb_squad_wages(team_urls = team_city_url)

lista<- fb_player_urls(team_city_url)

multiple_playing_time = data.frame()
multiple_shooting = data.frame()
year= 2022
for (i in lista){
  
  multiple_playing_time1 <- fb_player_match_logs(i, season_end_year = year, stat_type = 'summary')
  
  multiple_shooting1 <- fb_player_match_logs(i, season_end_year = year, stat_type = 'passing')
  
  multiple_playing_time <- rbind(multiple_playing_time, multiple_playing_time1)
  multiple_shooting <- rbind(multiple_shooting, multiple_shooting1)
  
}

merge(x = multiple_playing_time, y = multiple_shooting, by = c("Player","Season","Date","Day","Comp","Round",
                                                               "Venue","Result","Squad","Opponent","Start","Pos",
                                                               "Min"), all = TRUE)


write.xlsx(man_city_results, file = "TEAM_INFORME_2022.xlsx",
           sheetName = "RESULTS", append = FALSE)
write.xlsx(dataset, file = "TEAM_INFORME_2022.xlsx",
           sheetName = "MATCH_LOG", append = TRUE)
write.xlsx(goal_log, file = "TEAM_INFORME_2022.xlsx",
           sheetName = "GOAL_LOG", append = TRUE)