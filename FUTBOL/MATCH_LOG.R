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

# EXTRACT DATA
team_city_url <- "https://fbref.com/en/squads/b8fd03ef/2021-2022/Manchester-City-Stats"
team_passing <- fb_team_match_log_stats(team_urls = team_city_url, stat_type = "passing")
team_passing_types <- fb_team_match_log_stats(team_urls = team_city_url, stat_type = "passing_types")
team_shooting <- fb_team_match_log_stats(team_urls = team_city_url, stat_type = "shooting")
team_gca <- fb_team_match_log_stats(team_urls = team_city_url, stat_type = "gca")
team_defense <- fb_team_match_log_stats(team_urls = team_city_url, stat_type = "defense")
team_misc <- fb_team_match_log_stats(team_urls = team_city_url, stat_type = "misc")

dataset<- merge(x = team_passing, y = team_passing_types, by = 
                  c('Team_Url','Team',"ForAgainst", "Date",'Time','Comp','Round','Day',
                    'Venue','Result','GF','GA','Opponent'),all.x = TRUE)
dataset<- merge(x = dataset, y = team_shooting, by = 
                  c('Team_Url','Team',"ForAgainst", "Date",'Time','Comp','Round','Day',
                    'Venue','Result','GF','GA','Opponent'),all.x = TRUE)
dataset<- merge(x = dataset, y = team_gca, by = 
                  c('Team_Url','Team',"ForAgainst", "Date",'Time','Comp','Round','Day',
                    'Venue','Result','GF','GA','Opponent'),all.x = TRUE)
dataset<- merge(x = dataset, y = team_defense, by = 
                  c('Team_Url','Team',"ForAgainst", "Date",'Time','Comp','Round','Day',
                    'Venue','Result','GF','GA','Opponent'),all.x = TRUE)
dataset<- merge(x = dataset, y = team_misc, by = 
                  c('Team_Url','Team',"ForAgainst", "Date",'Time','Comp','Round','Day',
                    'Venue','Result','GF','GA','Opponent'),all.x = TRUE)


goal_log <- fb_team_goal_logs(team_urls = team_city_url, for_or_against="both")

man_city_results <- fb_team_match_results(team_city_url)

write.xlsx(man_city_results, file = "TEAM_INFORME_2022.xlsx",
           sheetName = "RESULTS", append = FALSE)
write.xlsx(dataset, file = "TEAM_INFORME_2022.xlsx",
           sheetName = "MATCH_LOG", append = TRUE)
write.xlsx(goal_log, file = "TEAM_INFORME_2022.xlsx",
           sheetName = "GOAL_LOG", append = TRUE)

