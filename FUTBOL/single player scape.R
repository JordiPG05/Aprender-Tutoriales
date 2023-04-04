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

player<- 'https://fbref.com/en/players/1f44ac21/Erling-Haaland'

year= 2022

df <- fb_player_match_logs(player, season_end_year = year, stat_type = 'summary')

write.csv(df, file = "Player_Stat.csv")
