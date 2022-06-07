#### DRAFTKINGS MLB - BATTING ####

# libraries
library(rvest)
library(tidyverse)
library(data.table)
library(gsubfn)
library(magrittr)
library(rlist)
library(lubridate)
library(devtools)
require(baseballr)
library(coda)
library(rjags)
library(R2jags)
load.module("glm")
library(ggplot2)
library(MASS)
library(randomForest)

# names and functions
teams = c("NYY","TBR","BOS","TOR","BAL","MIN","CLE","CHW","KCR","DET","HOU","OAK","TEX","LAA","SEA",
          "ATL","WSN","NYM","PHI","MIA","STL","MIL","CHC","CIN","PIT","LAD","ARI","SFG","COL","SDP")
p_names = c("Rank","Game","Date","Home","Opponent","Result","IP","H","R","ER","UER","BB","SO","HR","HBP",
            "ERA","BF","Pit","Str","IR","IS","SB","CS","AB","2B","3B","IBB","SH","SF","ROE","GDP",
            "Indiv_Pitchers","Umpire","Pitchers_Used","Team")
mlb_fn = function(team, year, set){
  url = paste("https://www.baseball-reference.com/teams/tgl.cgi?team=", team, "&t=", set, "&year=", year, 
              sep = "")
  log = read_html(url) %>% html_nodes("table")
  log = log[[length(log)]] %>% html_table()
  log$Team = team
  return(log)
}

#### PAST SEASONS ####

## TEAM PITCHING STATS

pitching_box_17 = lapply(teams, mlb_fn, year = 2017, set = "p") %>%
  do.call("rbind", .)
colnames(pitching_box_17) = p_names
pitching_box_17 = pitching_box_17 %>% dplyr::select(-Rank)
pitching_box_17 = pitching_box_17[pitching_box_17$Opponent != "Opp",]
pitching_box_17$Home = ifelse(pitching_box_17$Home == "@", "Away", "Home")
cols = c(1,6:31)
pitching_box_17[,cols] = sapply(pitching_box_17[,cols], as.numeric)
pitching_box_17$Season = 2017
month = str_sub(pitching_box_17$Date, 1, 3)
month = ifelse(month == "Mar", 3, ifelse(month == "Apr", 4, ifelse(month == "May", 5, 
                                                                   ifelse(month == "Jun", 6, ifelse(month == "Jul", 7, ifelse(month == "Aug", 8, 
                                                                                                                              ifelse(month == "Sep", 9, ifelse(month == "Oct", 10, NA))))))))
day = str_sub(pitching_box_17$Date, 5, -1)
day = ifelse(str_length(day) == 4, str_sub(day, 1, 1),
             ifelse(str_length(day) == 5, str_sub(day, 1, 2),
                    day))
pitching_box_17$Date = as.Date(paste(pitching_box_17$Season, "-", month, "-", day, sep = ""))

pitching_box_18 = lapply(teams, mlb_fn, year = 2018, set = "p") %>%
  do.call("rbind", .)
colnames(pitching_box_18) = p_names
pitching_box_18 = pitching_box_18 %>% dplyr::select(-Rank)
pitching_box_18 = pitching_box_18[pitching_box_18$Opponent != "Opp",]
pitching_box_18$Home = ifelse(pitching_box_18$Home == "@", "Away", "Home")
cols = c(1,6:31)
pitching_box_18[,cols] = sapply(pitching_box_18[,cols], as.numeric)
pitching_box_18$Season = 2018
month = str_sub(pitching_box_18$Date, 1, 3)
month = ifelse(month == "Mar", 3, ifelse(month == "Apr", 4, ifelse(month == "May", 5, 
                                                                   ifelse(month == "Jun", 6, ifelse(month == "Jul", 7, ifelse(month == "Aug", 8, 
                                                                                                                              ifelse(month == "Sep", 9, ifelse(month == "Oct", 10, NA))))))))
day = str_sub(pitching_box_18$Date, 5, -1)
day = ifelse(str_length(day) == 4, str_sub(day, 1, 1),
             ifelse(str_length(day) == 5, str_sub(day, 1, 2),
                    day))
pitching_box_18$Date = as.Date(paste(pitching_box_18$Season, "-", month, "-", day, sep = ""))

pitching_box_19 = lapply(teams, mlb_fn, year = 2019, set = "p") %>%
  do.call("rbind", .)
colnames(pitching_box_19) = p_names
pitching_box_19 = pitching_box_19 %>% dplyr::select(-Rank)
pitching_box_19 = pitching_box_19[pitching_box_19$Opponent != "Opp",]
pitching_box_19$Home = ifelse(pitching_box_19$Home == "@", "Away", "Home")
cols = c(1,6:31)
pitching_box_19[,cols] = sapply(pitching_box_19[,cols], as.numeric)
pitching_box_19$Season = 2019
month = str_sub(pitching_box_19$Date, 1, 3)
month = ifelse(month == "Mar", 3, ifelse(month == "Apr", 4, ifelse(month == "May", 5, 
                                                                   ifelse(month == "Jun", 6, ifelse(month == "Jul", 7, ifelse(month == "Aug", 8, 
                                                                                                                              ifelse(month == "Sep", 9, ifelse(month == "Oct", 10, NA))))))))
day = str_sub(pitching_box_19$Date, 5, -1)
day = ifelse(str_length(day) == 4, str_sub(day, 1, 1),
             ifelse(str_length(day) == 5, str_sub(day, 1, 2),
                    day))
pitching_box_19$Date = as.Date(paste(pitching_box_19$Season, "-", month, "-", day, sep = ""))

pitching_box_20 = lapply(teams, mlb_fn, year = 2020, set = "p") %>%
  do.call("rbind", .)
colnames(pitching_box_20) = p_names
pitching_box_20 = pitching_box_20 %>% dplyr::select(-Rank)
pitching_box_20 = pitching_box_20[pitching_box_20$Opponent != "Opp",]
pitching_box_20$Home = ifelse(pitching_box_20$Home == "@", "Away", "Home")
cols = c(1,6:31)
pitching_box_20[,cols] = sapply(pitching_box_20[,cols], as.numeric)
pitching_box_20$Season = 2020
month = str_sub(pitching_box_20$Date, 1, 3)
month = ifelse(month == "Mar", 3, ifelse(month == "Apr", 4, ifelse(month == "May", 5, 
                                                                   ifelse(month == "Jun", 6, ifelse(month == "Jul", 7, ifelse(month == "Aug", 8, 
                                                                                                                              ifelse(month == "Sep", 9, ifelse(month == "Oct", 10, NA))))))))
day = str_sub(pitching_box_20$Date, 5, -1)
day = ifelse(str_length(day) == 4, str_sub(day, 1, 1),
             ifelse(str_length(day) == 5, str_sub(day, 1, 2),
                    day))
pitching_box_20$Date = as.Date(paste(pitching_box_20$Season, "-", month, "-", day, sep = ""))

pitching_box_21 = lapply(teams, mlb_fn, year = 2021, set = "p") %>%
  do.call("rbind", .)
colnames(pitching_box_21) = p_names
pitching_box_21 = pitching_box_21 %>% dplyr::select(-Rank)
pitching_box_21 = pitching_box_21[pitching_box_21$Opponent != "Opp",]
pitching_box_21$Home = ifelse(pitching_box_21$Home == "@", "Away", "Home")
cols = c(1,6:31)
pitching_box_21[,cols] = sapply(pitching_box_21[,cols], as.numeric)
pitching_box_21$Season = 2021
month = str_sub(pitching_box_21$Date, 1, 3)
month = ifelse(month == "Mar", 3, ifelse(month == "Apr", 4, ifelse(month == "May", 5, 
                                                                   ifelse(month == "Jun", 6, ifelse(month == "Jul", 7, ifelse(month == "Aug", 8, 
                                                                                                                              ifelse(month == "Sep", 9, ifelse(month == "Oct", 10, NA))))))))
day = str_sub(pitching_box_21$Date, 5, -1)
day = ifelse(str_length(day) == 4, str_sub(day, 1, 1),
             ifelse(str_length(day) == 5, str_sub(day, 1, 2),
                    day))
pitching_box_21$Date = as.Date(paste(pitching_box_21$Season, "-", month, "-", day, sep = ""))

# bind the years together
pitching_box = bind_rows(pitching_box_17, pitching_box_18, pitching_box_19, pitching_box_20,
                        pitching_box_21)
pitching_box$IP = ifelse(pitching_box$IP %% 1 == 0, pitching_box$IP,
                         ifelse(pitching_box$IP %% 1 > 0.15, floor(pitching_box$IP) + 0.67,
                                floor(pitching_box$IP) + 0.33))

# iterate through and get all team summaries for all years
pitching_sums = pitching_box

# team summaries for each season (label as opponent bc matching with pitcher's opponent)
seasons = unique(pitching_sums$Season)
pitching_save = data.frame()
for(i in seasons){
  for(j in teams){
    df = pitching_sums[pitching_sums$Team == j & pitching_sums$Season == i,]
    df = data.frame(as.numeric(i) + 1, j, mean(df$H), mean(df$R), mean(df$BB), mean(df$SO), mean(df$HR), 
                    mean(df$HBP), mean(df$BF), mean(df$Pit), mean(df$Str), mean(df$SB), mean(df$CS), 
                    mean(df$AB), mean(df$`2B`), mean(df$`3B`), mean(df$SF))
    pitching_save = bind_rows(pitching_save, df)
  }
}
colnames(pitching_save) = c("Season", "Opponent", "opp_prev_H", "opp_prev_R", "opp_prev_BB",
                           "opp_prev_SO", "opp_prev_HR", "opp_prev_HBP", "opp_prev_BF", "opp_prev_Pit",
                           "opp_prev_Str", "opp_prev_SB", "opp_prev_CS", "opp_prev_AB", "opp_prev_2B",
                           "opp_prev_3B", "opp_prev_SF")

# rolling stats
pitching_rolling = data.frame()
for(i in seasons[2:length(seasons)]){
  season = pitching_sums[pitching_sums$Season == i,]
  for(j in teams){
    team = season[season$Team == j,]
    for(k in team$Game){
      df = pitching_sums[pitching_sums$Season == i & pitching_sums$Team == j & pitching_sums$Game < k,]
      df2 = pitching_sums[pitching_sums$Season == i & pitching_sums$Team == j & pitching_sums$Game <= k,]
      df = data.frame(i, j, k, df2$Date[df2$Game == k], mean(df$H), mean(df$R), mean(df$BB), mean(df$SO), 
                      mean(df$HR), mean(df$HBP), mean(df$BF), mean(df$Pit), mean(df$Str), mean(df$SB), 
                      mean(df$CS), mean(df$AB), mean(df$`2B`), mean(df$`3B`), mean(df$SF))
      pitching_rolling = bind_rows(pitching_rolling, df)
    }
  }
}
colnames(pitching_rolling) = c("Season", "Opponent", "Opp_Game", "Date", "opp_rolling_H", "opp_rolling_R", 
                               "opp_rolling_BB", "opp_rolling_SO", "opp_rolling_HR", "opp_rolling_HBP", "opp_rolling_BF", 
                               "opp_rolling_Pit", "opp_rolling_Str", "opp_rolling_SB", "opp_rolling_CS", "opp_rolling_AB", 
                               "opp_rolling_2B", "opp_rolling_3B", "opp_rolling_SF")

# clean up
pitching_box = pitching_box %>% dplyr::select(Team, Game, Date, Opponent, Season)
pitching_box = pitching_box[pitching_box$Season != 2017,]

## ELO

mlb_elo = fread("https://projects.fivethirtyeight.com/mlb-api/mlb_elo.csv")
mlb_elo = mlb_elo[mlb_elo$season >= 2018,]
mlb_elo = mlb_elo[mlb_elo$playoff == "",]
mlb_elo$team1 = ifelse(mlb_elo$team1 == "TBD", "TBR", mlb_elo$team1)
mlb_elo$team1 = ifelse(mlb_elo$team1 == "ANA", "LAA", mlb_elo$team1)
mlb_elo$team1 = ifelse(mlb_elo$team1 == "FLA", "MIA", mlb_elo$team1)
mlb_elo$team2 = ifelse(mlb_elo$team2 == "TBD", "TBR", mlb_elo$team2)
mlb_elo$team2 = ifelse(mlb_elo$team2 == "ANA", "LAA", mlb_elo$team2)
mlb_elo$team2 = ifelse(mlb_elo$team2 == "FLA", "MIA", mlb_elo$team2)

# function to get game numbers for each team
mlb_elo = mlb_elo[order(mlb_elo$date,  decreasing = FALSE),]
mlb_elo_18 = mlb_elo[mlb_elo$season == 2018,]
mlb_elo_19 = mlb_elo[mlb_elo$season == 2019,]
mlb_elo_20 = mlb_elo[mlb_elo$season == 2020,]
mlb_elo_21 = mlb_elo[mlb_elo$season == 2021,]
game_num_fn = function(entry, data){
  df = data[1:entry,]
  t1 = df$team1[entry]
  t2 = df$team2[entry]
  team1_game = nrow(df[df$team1 == t1,]) + nrow(df[df$team2 == t1,])
  team2_game = nrow(df[df$team1 == t2,]) + nrow(df[df$team2 == t2,])
  games = data.frame(team1_game, team2_game)
  return(games)
}
game_num_18 = lapply(1:nrow(mlb_elo_18), game_num_fn, data = mlb_elo_18) %>% do.call("rbind", .)
mlb_elo_18 = bind_cols(mlb_elo_18, game_num_18)
game_num_19 = lapply(1:nrow(mlb_elo_19), game_num_fn, data = mlb_elo_19) %>% do.call("rbind", .)
mlb_elo_19 = bind_cols(mlb_elo_19, game_num_19)
game_num_20 = lapply(1:nrow(mlb_elo_20), game_num_fn, data = mlb_elo_20) %>% do.call("rbind", .)
mlb_elo_20 = bind_cols(mlb_elo_20, game_num_20)
game_num_21 = lapply(1:nrow(mlb_elo_21), game_num_fn, data = mlb_elo_21) %>% do.call("rbind", .)
mlb_elo_21 = bind_cols(mlb_elo_21, game_num_21)
mlb_elo = bind_rows(mlb_elo_21, mlb_elo_20, mlb_elo_19, mlb_elo_18)

away_elo = mlb_elo
away_elo$Team = away_elo$team2
mlb_elo$Team = mlb_elo$team1
mlb_elo = bind_rows(mlb_elo, away_elo)
mlb_elo$rating_diff = ifelse(mlb_elo$Team == mlb_elo$team1, mlb_elo$rating1_pre - mlb_elo$rating2_pre,
                             mlb_elo$rating2_pre - mlb_elo$rating1_pre) 
mlb_elo$Game = ifelse(mlb_elo$Team == mlb_elo$team1, mlb_elo$team1_game, mlb_elo$team2_game)
mlb_elo$Opp_Game = ifelse(mlb_elo$Team == mlb_elo$team1, mlb_elo$team2_game, mlb_elo$team1_game)
colnames(mlb_elo) = c("Date", "Season", "Neutral", "Playoff", colnames(mlb_elo)[5:32])

## TEAM PITCHING MATCHED WITH ELO

team_pitching = left_join(pitching_box, mlb_elo, by = c("Team", "Game", "Season"))
team_pitching = left_join(team_pitching, pitching_rolling, by = c("Opponent", "Opp_Game", "Season"))
team_pitching$Season = as.numeric(team_pitching$Season)
team_pitching = left_join(team_pitching, pitching_save, by = c("Opponent", "Season"))
team_pitching = team_pitching %>% dplyr::select(-c(Date.y, Neutral, Playoff, elo1_pre, elo2_pre, elo_prob1, 
                                                 elo_prob2, elo1_post, elo2_post, pitcher1_adj, 
                                                 pitcher2_adj, rating_prob1, rating_prob2, rating1_post, 
                                                 rating2_post, score1, score2, team1_game, team2_game, 
                                                 Date))
colnames(team_pitching)[3] = "Date"
team_pitching$Date = as.Date(team_pitching$Date)
team_pitching$Pitcher = ifelse(team_pitching$Team == team_pitching$team1, team_pitching$pitcher1, 
                              team_pitching$pitcher2)
team_pitching$RGS = ifelse(team_pitching$Team == team_pitching$team1, team_pitching$pitcher1_rgs, 
                          team_pitching$pitcher2_rgs)

# set NaN to 0
for(i in 1:length(team_pitching)){
  for(j in 1:nrow(team_pitching)){
    team_pitching[[i]][[j]] = ifelse(is.nan(team_pitching[[i]][[j]]), 0, team_pitching[[i]][[j]])
  }
}

# create weighted values
team_pitching$opp_weight_H = ifelse(team_pitching$Season == 2021,
                                    (60*team_pitching$opp_prev_H + exp((team_pitching$Opp_Game - 1)/3)*
                                       team_pitching$opp_rolling_H)/(60 + exp((team_pitching$Opp_Game - 1)/3)),
                                    (162*team_pitching$opp_prev_H + exp((team_pitching$Opp_Game - 1)/3)*
                                       team_pitching$opp_rolling_H)/(162 + exp((team_pitching$Opp_Game - 1)/3)))
team_pitching$opp_weight_R = ifelse(team_pitching$Season == 2021,
                                    (60*team_pitching$opp_prev_R + exp((team_pitching$Opp_Game - 1)/3)*
                                       team_pitching$opp_rolling_R)/(60 + exp((team_pitching$Opp_Game - 1)/3)),
                                    (162*team_pitching$opp_prev_R + exp((team_pitching$Opp_Game - 1)/3)*
                                       team_pitching$opp_rolling_R)/(162 + exp((team_pitching$Opp_Game - 1)/3)))
team_pitching$opp_weight_BB = ifelse(team_pitching$Season == 2021,
                                     (60*team_pitching$opp_prev_BB + exp((team_pitching$Opp_Game - 1)/3)*
                                        team_pitching$opp_rolling_BB)/(60 + exp((team_pitching$Opp_Game - 1)/3)),
                                     (162*team_pitching$opp_prev_BB + exp((team_pitching$Opp_Game - 1)/3)*
                                        team_pitching$opp_rolling_BB)/(162 + exp((team_pitching$Opp_Game - 1)/3)))
team_pitching$opp_weight_SO = ifelse(team_pitching$Season == 2021,
                                     (60*team_pitching$opp_prev_SO + exp((team_pitching$Opp_Game - 1)/3)*
                                        team_pitching$opp_rolling_SO)/(60 + exp((team_pitching$Opp_Game - 1)/3)),
                                     (162*team_pitching$opp_prev_SO + exp((team_pitching$Opp_Game - 1)/3)*
                                        team_pitching$opp_rolling_SO)/(162 + exp((team_pitching$Opp_Game - 1)/3)))
team_pitching$opp_weight_HR = ifelse(team_pitching$Season == 2021,
                                     (60*team_pitching$opp_prev_HR + exp((team_pitching$Opp_Game - 1)/3)*
                                        team_pitching$opp_rolling_HR)/(60 + exp((team_pitching$Opp_Game - 1)/3)),
                                     (162*team_pitching$opp_prev_HR + exp((team_pitching$Opp_Game - 1)/3)*
                                        team_pitching$opp_rolling_HR)/(162 + exp((team_pitching$Opp_Game - 1)/3)))
team_pitching$opp_weight_HBP = ifelse(team_pitching$Season == 2021,
                                      (60*team_pitching$opp_prev_HBP + exp((team_pitching$Opp_Game - 1)/3)*
                                         team_pitching$opp_rolling_HBP)/(60 + exp((team_pitching$Opp_Game - 1)/3)),
                                      (162*team_pitching$opp_prev_HBP + exp((team_pitching$Opp_Game - 1)/3)*
                                         team_pitching$opp_rolling_HBP)/(162 + exp((team_pitching$Opp_Game - 1)/3)))
team_pitching$opp_weight_BF = ifelse(team_pitching$Season == 2021,
                                      (60*team_pitching$opp_prev_BF + exp((team_pitching$Opp_Game - 1)/3)*
                                         team_pitching$opp_rolling_BF)/(60 + exp((team_pitching$Opp_Game - 1)/3)),
                                      (162*team_pitching$opp_prev_BF + exp((team_pitching$Opp_Game - 1)/3)*
                                         team_pitching$opp_rolling_BF)/(162 + exp((team_pitching$Opp_Game - 1)/3)))
team_pitching$opp_weight_Pit = ifelse(team_pitching$Season == 2021,
                                     (60*team_pitching$opp_prev_Pit + exp((team_pitching$Opp_Game - 1)/3)*
                                        team_pitching$opp_rolling_Pit)/(60 + exp((team_pitching$Opp_Game - 1)/3)),
                                     (162*team_pitching$opp_prev_Pit + exp((team_pitching$Opp_Game - 1)/3)*
                                        team_pitching$opp_rolling_Pit)/(162 + exp((team_pitching$Opp_Game - 1)/3)))
team_pitching$opp_weight_Str = ifelse(team_pitching$Season == 2021,
                                      (60*team_pitching$opp_prev_Str + exp((team_pitching$Opp_Game - 1)/3)*
                                         team_pitching$opp_rolling_Str)/(60 + exp((team_pitching$Opp_Game - 1)/3)),
                                      (162*team_pitching$opp_prev_Str + exp((team_pitching$Opp_Game - 1)/3)*
                                         team_pitching$opp_rolling_Str)/(162 + exp((team_pitching$Opp_Game - 1)/3)))
team_pitching$opp_weight_SB = ifelse(team_pitching$Season == 2021,
                                     (60*team_pitching$opp_prev_SB + exp((team_pitching$Opp_Game - 1)/3)*
                                        team_pitching$opp_rolling_SB)/(60 + exp((team_pitching$Opp_Game - 1)/3)),
                                     (162*team_pitching$opp_prev_SB + exp((team_pitching$Opp_Game - 1)/3)*
                                        team_pitching$opp_rolling_SB)/(162 + exp((team_pitching$Opp_Game - 1)/3)))
team_pitching$opp_weight_CS = ifelse(team_pitching$Season == 2021,
                                    (60*team_pitching$opp_prev_CS + exp((team_pitching$Opp_Game - 1)/3)*
                                       team_pitching$opp_rolling_CS)/(60 + exp((team_pitching$Opp_Game - 1)/3)),
                                    (162*team_pitching$opp_prev_CS + exp((team_pitching$Opp_Game - 1)/3)*
                                       team_pitching$opp_rolling_CS)/(162 + exp((team_pitching$Opp_Game - 1)/3)))
team_pitching$opp_weight_AB = ifelse(team_pitching$Season == 2021,
                                    (60*team_pitching$opp_prev_AB + exp((team_pitching$Opp_Game - 1)/3)*
                                       team_pitching$opp_rolling_AB)/(60 + exp((team_pitching$Opp_Game - 1)/3)),
                                    (162*team_pitching$opp_prev_AB + exp((team_pitching$Opp_Game - 1)/3)*
                                       team_pitching$opp_rolling_AB)/(162 + exp((team_pitching$Opp_Game - 1)/3)))
team_pitching$opp_weight_2B = ifelse(team_pitching$Season == 2021,
                                    (60*team_pitching$opp_prev_2B + exp((team_pitching$Opp_Game - 1)/3)*
                                       team_pitching$opp_rolling_2B)/(60 + exp((team_pitching$Opp_Game - 1)/3)),
                                    (162*team_pitching$opp_prev_2B + exp((team_pitching$Opp_Game - 1)/3)*
                                       team_pitching$opp_rolling_2B)/(162 + exp((team_pitching$Opp_Game - 1)/3)))
team_pitching$opp_weight_3B = ifelse(team_pitching$Season == 2021,
                                    (60*team_pitching$opp_prev_3B + exp((team_pitching$Opp_Game - 1)/3)*
                                       team_pitching$opp_rolling_3B)/(60 + exp((team_pitching$Opp_Game - 1)/3)),
                                    (162*team_pitching$opp_prev_3B + exp((team_pitching$Opp_Game - 1)/3)*
                                       team_pitching$opp_rolling_3B)/(162 + exp((team_pitching$Opp_Game - 1)/3)))
team_pitching$opp_weight_SF = ifelse(team_pitching$Season == 2021,
                                     (60*team_pitching$opp_prev_SF + exp((team_pitching$Opp_Game - 1)/3)*
                                        team_pitching$opp_rolling_SF)/(60 + exp((team_pitching$Opp_Game - 1)/3)),
                                     (162*team_pitching$opp_prev_SF + exp((team_pitching$Opp_Game - 1)/3)*
                                        team_pitching$opp_rolling_SF)/(162 + exp((team_pitching$Opp_Game - 1)/3)))
team_pitching$opp_weight_BA = team_pitching$opp_weight_H/team_pitching$opp_weight_AB
team_pitching$opp_weight_OBP = (team_pitching$opp_weight_H + team_pitching$opp_weight_BB + team_pitching$opp_weight_HBP)/(team_pitching$opp_weight_AB + team_pitching$opp_weight_BB + team_pitching$opp_weight_HBP + team_pitching$opp_weight_SF)
team_pitching$opp_weight_SLG = ((team_pitching$opp_weight_H - team_pitching$opp_weight_2B - team_pitching$opp_weight_3B - team_pitching$opp_weight_HR) + 2*team_pitching$opp_weight_2B + 3*team_pitching$opp_weight_3B + 4*team_pitching$opp_weight_HR)/team_pitching$opp_weight_AB
team_pitching$opp_weight_HR_pct = team_pitching$opp_weight_HR/(team_pitching$opp_weight_AB + team_pitching$opp_weight_BB + team_pitching$opp_weight_HBP + team_pitching$opp_weight_SF)
team_pitching$opp_weight_SO_pct = team_pitching$opp_weight_SO/(team_pitching$opp_weight_AB + team_pitching$opp_weight_BB + team_pitching$opp_weight_HBP + team_pitching$opp_weight_SF)
team_pitching$opp_weight_BB_pct = team_pitching$opp_weight_BB/(team_pitching$opp_weight_AB + team_pitching$opp_weight_BB + team_pitching$opp_weight_HBP + team_pitching$opp_weight_SF)


## BATTER STATS

# function to pull game logs
batter_fn = function(list, year){
  # URLs to try
  url1 = paste("https://www.baseball-reference.com/players/gl.fcgi?id=", 
              tolower(str_sub(word(list, 2), 1, 5)), tolower(str_sub(word(list, 1), 1, 2)), 
              "01&t=b&year=", year, sep = "")
  url2 = paste("https://www.baseball-reference.com/players/gl.fcgi?id=", 
               tolower(str_sub(word(list, 2), 1, 5)), tolower(str_sub(word(list, 1), 1, 2)), 
               "02&t=b&year=", year, sep = "")
  url3 = paste("https://www.baseball-reference.com/players/gl.fcgi?id=", 
               tolower(str_sub(word(list, 2), 1, 5)), tolower(str_sub(word(list, 1), 1, 2)), 
               "03&t=b&year=", year, sep = "")
  
  # try each URL to get stats table
  logs1 = url1 %>%
    read_html() %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  logs1 = logs1[[5]]
  logs2 = if(length(logs1) == 0){
    url2 %>%
      read_html() %>%
      html_nodes("table") %>%
      html_table(fill = TRUE)
    } else{
      c()
    }
  logs2 = logs2[[5]]
  logs3 = if(length(logs1) == 0 & length(logs2) == 0){
    url3 %>%
      read_html() %>%
      html_nodes("table") %>%
      html_table(fill = TRUE)
  } else{
    c()
  }
  logs3 = logs3[[5]]
  
  # return
  logs = rbind(logs1, logs2, logs3)
  return(logs)
}

batting_fn = function(year, set){
  url = paste("https://www.baseball-reference.com/leagues/MLB/", year, "-", set, "-batting.shtml",
              sep = "")
  log = read_html(url)
  # First get the commented nodes
  alt_tables = xml2::xml_find_all(log,"//comment()") %>% {
    #Find only commented nodes that contain the regex for html table markup
    raw_parts = as.character(.[grep("\\</?table", as.character(.))])
    # Remove the comment begin and end tags
    strip_html = stringi::stri_replace_all_regex(raw_parts, c("<\\!--","-->"),c("",""),
                                                  vectorize_all = FALSE)
    # Loop through the pieces that have tables within markup and 
    # apply the same functions
    lapply(grep("<table", strip_html, value = TRUE), function(i){
      rvest::html_table(xml_find_all(read_html(i), "//table")) %>% 
        .[[1]]
    })
  }
  alt_tables = alt_tables[[1]]
  if(set == "advanced"){
    colnames(alt_tables) = alt_tables[1,]
    alt_tables = alt_tables[-1,]
  }
  alt_tables$Name = str_remove_all(alt_tables$Name, "[*+]")
  return(alt_tables)
}

# function to remove duplicates
rm_dups = function(player, set){
  guy = set[set$Name == player,]
  total_stats = guy[1,]
  team = guy$Tm[nrow(guy)]
  total_stats$Tm = team
  return(total_stats)
}

# 2017
# summary
batting_17 = batting_fn(2017, "standard")
cols = c(1,3,6:29)
batting_17[,cols] = sapply(batting_17[,cols], as.numeric)
batting_17$Season = 2017
batting_17 = lapply(unique(batting_17$Name), rm_dups, set = batting_17) %>%
  do.call("rbind", .)
batting_17 = batting_17 %>% dplyr::select(-Rk)

# advanced
batting_adv_17 = batting_fn(2017, "advanced")
cols = c(10:12,14:17,19:21,23,25:27)
batting_adv_17[,cols] = sapply(cols, function(x) str_remove_all(batting_adv_17[,x], "[%]"))
cols = c(1,3,5:27)
batting_adv_17[,cols] = sapply(batting_adv_17[,cols], as.numeric)
batting_adv_17 = lapply(unique(batting_adv_17$Name), rm_dups, set = batting_adv_17) %>%
  do.call("rbind", .)
batting_adv_17 = batting_adv_17 %>% dplyr::select(-Rk)

# join
batting_17 = left_join(batting_17, batting_adv_17, by = c("Name", "Age", "Tm"))

# 2018
# summary
batting_18 = batting_fn(2018, "standard")
cols = c(1,3,6:29)
batting_18[,cols] = sapply(batting_18[,cols], as.numeric)
batting_18$Season = 2018
batting_18 = lapply(unique(batting_18$Name), rm_dups, set = batting_18) %>%
  do.call("rbind", .)
batting_18 = batting_18 %>% dplyr::select(-Rk)

# advanced
batting_adv_18 = batting_fn(2018, "advanced")
cols = c(10:12,14:17,19:21,23,25:27)
batting_adv_18[,cols] = sapply(cols, function(x) str_remove_all(batting_adv_18[,x], "[%]"))
cols = c(1,3,5:27)
batting_adv_18[,cols] = sapply(batting_adv_18[,cols], as.numeric)
batting_adv_18 = lapply(unique(batting_adv_18$Name), rm_dups, set = batting_adv_18) %>%
  do.call("rbind", .)
batting_adv_18 = batting_adv_18 %>% dplyr::select(-Rk)

# join
batting_18 = left_join(batting_18, batting_adv_18, by = c("Name", "Age", "Tm"))

# players to include
batters_18 = batting_18$Name
batters_18 = batters_18[!is.na(batters_18)]
batters_18 = unique(batters_18)

# fake space character to be removed
name_char = str_sub(batters_18[1],5,5)

# split names into multiple words
batters_18 = str_replace_all(batters_18, name_char, " ")

# all available game logs
games_18 = sapply(1:length(batters_18), function(i) try(batter_fn(batters_18[i], year = 2018), TRUE))
test = data.frame()
for(i in 1:length(games_18)){
  a = is.data.frame(games_18[[i]])
  test = rbind(test, a)
}
test$num = 1:length(games_18)
test = test[test[,1] == FALSE,]
games_18 = list.remove(games_18, test$num)
batters_18 = batters_18[-test$num]
for(j in 1:length(games_18)){
  games_18[[j]][1] = batters_18[j]
  games_18[[j]] = games_18[[j]][c(1:38)]
}
games_18 = games_18 %>%
  do.call("rbind", .)
games_18 = games_18[games_18$Tm %in% teams,]
games_18 = games_18[!is.na(games_18$Tm),]
colnames(games_18) = c("Batter", colnames(games_18)[2:4], "Team", "Home", "Opponent", 
                       colnames(games_18)[8:length(games_18)])
games_18$Season = 2018

# 2019
# summary
batting_19 = batting_fn(2019, "standard")
cols = c(1,3,6:29)
batting_19[,cols] = sapply(batting_19[,cols], as.numeric)
batting_19$Season = 2019
batting_19 = lapply(unique(batting_19$Name), rm_dups, set = batting_19) %>%
  do.call("rbind", .)
batting_19 = batting_19 %>% dplyr::select(-Rk)

# advanced
batting_adv_19 = batting_fn(2019, "advanced")
cols = c(10:12,14:17,19:21,23,25:27)
batting_adv_19[,cols] = sapply(cols, function(x) str_remove_all(batting_adv_19[,x], "[%]"))
cols = c(1,3,5:27)
batting_adv_19[,cols] = sapply(batting_adv_19[,cols], as.numeric)
batting_adv_19 = lapply(unique(batting_adv_19$Name), rm_dups, set = batting_adv_19) %>%
  do.call("rbind", .)
batting_adv_19 = batting_adv_19 %>% dplyr::select(-Rk)

# join
batting_19 = left_join(batting_19, batting_adv_19, by = c("Name", "Age", "Tm"))

# players to include
batters_19 = batting_19$Name
batters_19 = batters_19[!is.na(batters_19)]
batters_19 = unique(batters_19)

# split names into multiple words
batters_19 = str_replace_all(batters_19, name_char, " ")

# all available game logs
games_19 = sapply(1:length(batters_19), function(i) try(batter_fn(batters_19[i], year = 2019), TRUE))
test = data.frame()
for(i in 1:length(games_19)){
  a = is.data.frame(games_19[[i]])
  test = rbind(test, a)
}
test$num = 1:length(games_19)
test = test[test[,1] == FALSE,]
games_19 = list.remove(games_19, test$num)
batters_19 = batters_19[-test$num]
for(j in 1:length(games_19)){
  games_19[[j]][1] = batters_19[j]
  games_19[[j]] = games_19[[j]][c(1:38)]
}
games_19 = games_19 %>%
  do.call("rbind", .)
games_19 = games_19[games_19$Tm %in% teams,]
games_19 = games_19[!is.na(games_19$Tm),]
colnames(games_19) = c("Batter", colnames(games_19)[2:4], "Team", "Home", "Opponent", 
                       colnames(games_19)[8:length(games_19)])
games_19$Season = 2019

# 2020
# summary
batting_20 = batting_fn(2020, "standard")
cols = c(1,3,6:29)
batting_20[,cols] = sapply(batting_20[,cols], as.numeric)
batting_20$Season = 2020
batting_20 = lapply(unique(batting_20$Name), rm_dups, set = batting_20) %>%
  do.call("rbind", .)
batting_20 = batting_20 %>% dplyr::select(-Rk)

# advanced
batting_adv_20 = batting_fn(2020, "advanced")
cols = c(10:12,14:17,19:21,23,25:27)
batting_adv_20[,cols] = sapply(cols, function(x) str_remove_all(batting_adv_20[,x], "[%]"))
cols = c(1,3,5:27)
batting_adv_20[,cols] = sapply(batting_adv_20[,cols], as.numeric)
batting_adv_20 = lapply(unique(batting_adv_20$Name), rm_dups, set = batting_adv_20) %>%
  do.call("rbind", .)
batting_adv_20 = batting_adv_20 %>% dplyr::select(-Rk)

# join
batting_20 = left_join(batting_20, batting_adv_20, by = c("Name", "Age", "Tm"))

# players to include
batters_20 = batting_20$Name
batters_20 = batters_20[!is.na(batters_20)]
batters_20 = unique(batters_20)

# split names into multiple words
batters_20 = str_replace_all(batters_20, name_char, " ")

# all available game logs
games_20 = sapply(1:length(batters_20), function(i) try(batter_fn(batters_20[i], year = 2020), TRUE))
test = data.frame()
for(i in 1:length(games_20)){
  a = is.data.frame(games_20[[i]])
  test = rbind(test, a)
}
test$num = 1:length(games_20)
test = test[test[,1] == FALSE,]
games_20 = list.remove(games_20, test$num)
batters_20 = batters_20[-test$num]
for(j in 1:length(games_20)){
  games_20[[j]][1] = batters_20[j]
  games_20[[j]] = games_20[[j]][c(1:38)]
}
games_20 = games_20 %>%
  do.call("rbind", .)
games_20 = games_20[games_20$Tm %in% teams,]
games_20 = games_20[!is.na(games_20$Tm),]
colnames(games_20) = c("Batter", colnames(games_20)[2:4], "Team", "Home", "Opponent", 
                       colnames(games_20)[8:length(games_20)])
games_20$Season = 2020

# 2021
# summary
batting_21 = batting_fn(2021, "standard")
cols = c(1,3,6:29)
batting_21[,cols] = sapply(batting_21[,cols], as.numeric)
batting_21$Season = 2021
batting_21 = lapply(unique(batting_21$Name), rm_dups, set = batting_21) %>%
  do.call("rbind", .)
batting_21 = batting_21 %>% dplyr::select(-Rk)

# advanced
batting_adv_21 = batting_fn(2021, "advanced")
cols = c(10:12,14:17,19:21,23,25:27)
batting_adv_21[,cols] = sapply(cols, function(x) str_remove_all(batting_adv_21[,x], "[%]"))
cols = c(1,3,5:27)
batting_adv_21[,cols] = sapply(batting_adv_21[,cols], as.numeric)
batting_adv_21 = lapply(unique(batting_adv_21$Name), rm_dups, set = batting_adv_21) %>%
  do.call("rbind", .)
batting_adv_21 = batting_adv_21 %>% dplyr::select(-Rk)

# join
batting_21 = left_join(batting_21, batting_adv_21, by = c("Name", "Age", "Tm"))

# players to include
batters_21 = batting_21$Name
batters_21 = batters_21[!is.na(batters_21)]
batters_21 = unique(batters_21)

# split names into multiple words
batters_21 = str_replace_all(batters_21, name_char, " ")

# all available game logs
games_21 = sapply(1:length(batters_21), function(i) try(batter_fn(batters_21[i], year = 2021), TRUE))
test = data.frame()
for(i in 1:length(games_21)){
  a = is.data.frame(games_21[[i]])
  test = rbind(test, a)
}
test$num = 1:length(games_21)
test = test[test[,1] == FALSE,]
games_21 = list.remove(games_21, test$num)
batters_21 = batters_21[-test$num]
for(j in 1:length(games_21)){
  games_21[[j]][1] = batters_21[j]
  games_21[[j]] = games_21[[j]][c(1:38)]
}
games_21 = games_21 %>%
  do.call("rbind", .)
games_21 = games_21[games_21$Tm %in% teams,]
games_21 = games_21[!is.na(games_21$Tm),]
colnames(games_21) = c("Batter", colnames(games_21)[2:4], "Team", "Home", "Opponent", 
                       colnames(games_21)[8:length(games_21)])
games_21$Season = 2021

# all games
games = bind_rows(games_18, games_19, games_20, games_21)
gtm = str_split(games$Gtm, name_char) %>% 
  do.call("rbind", .) %>% 
  as.data.frame() %>% 
  dplyr::select(1) %>%
  unlist()
games$Gtm = gtm
cols = c(2:3,10:39)
games[,cols] = sapply(games[,cols], as.numeric)

# rolling stats
batters_rolling = data.frame()
for(i in seasons[2:length(seasons)]){
  season = games[games$Season == i,]
  for(j in unique(season$Batter)){
    batter = season[season$Batter == j,]
    batter = batter[!is.na(batter$Gtm),]
    min = min(batter$Gcar)
    for(k in batter$Gtm){
      df = games[games$Season == i & games$Batter == j & games$Gtm < k,]
      df = df[!is.na(df$Batter),]
      df2 = games[games$Season == i & games$Batter == j & games$Gtm == k,]
      df2 = df2[!is.na(df2$Batter),]
      df3 = data.frame(i, j, k, df2$Gcar, df2$Date, df2$Team, df2$Opponent, df2$BOP, df2$Gcar - min + 1, 
                       sum(df$PA), sum(df$AB), sum(df$R), sum(df$H), sum(df$`2B`), sum(df$`3B`),
                       sum(df$HR), sum(df$RBI), sum(df$BB), sum(df$SO), sum(df$HBP), sum(df$SF), 
                       sum(df$SB), sum(df$CS), sum(df$WPA))
      batters_rolling = bind_rows(batters_rolling, df3)
    }
  }
}
colnames(batters_rolling) = c("Season", "Batter", "Gtm", "Gcar", "Date", "Team", "Opponent", "BOP", "G", 
                               "PA", "AB", "R", "H", "2B", "3B", "HR", "RBI", "BB", "SO", "HBP", "SF", 
                               "SB", "CS", "WPA")
month = str_sub(batters_rolling$Date, 1, 3)
month = ifelse(month == "Mar", 3, ifelse(month == "Apr", 4, ifelse(month == "May", 5, 
           ifelse(month == "Jun", 6, ifelse(month == "Jul", 7, ifelse(month == "Aug", 8, 
           ifelse(month == "Sep", 9, ifelse(month == "Oct", 10, NA))))))))
day = str_sub(batters_rolling$Date, 5, 6)
day = str_remove_all(day, "[()]")
batters_rolling$Date = as.Date(paste(batters_rolling$Season, "-", month, "-", day, sep = ""))
batters_rolling$BA = batters_rolling$H/batters_rolling$AB
batters_rolling$OBP = (batters_rolling$H + batters_rolling$BB + batters_rolling$HBP)/(batters_rolling$AB + batters_rolling$BB + batters_rolling$HBP + batters_rolling$SF)
batters_rolling$SLG = ((batters_rolling$H - batters_rolling$`2B` - batters_rolling$`3B` - batters_rolling$HR) + 2*batters_rolling$`2B` + 3*batters_rolling$`3B` + 4*batters_rolling$HR)/batters_rolling$AB
batters_rolling$HR_pct = batters_rolling$HR/batters_rolling$PA
batters_rolling$SO_pct = batters_rolling$SO/batters_rolling$PA
batters_rolling$BB_pct = batters_rolling$BB/batters_rolling$PA

# join players' previous and current seasons
batters = bind_rows(batting_17, batting_18, batting_19, batting_20, batting_21)
batters$Season = as.numeric(batters$Season) + 1
colnames(batters) = c("Batter", colnames(batters)[2:length(batters)])
batters$Batter = str_replace_all(batters$Batter, name_char, " ")
batters$Batter = str_remove_all(batters$Batter, "[#]")
batters = left_join(batters_rolling, batters, by = c("Batter", "Season"))
batters = batters %>% dplyr::select(-c(SF.x, Age, Tm, Lg, OPS, `OPS+`, TB, GDP, SH, SF.y, IBB,
                                       paste0("Pos", name_char, "Summary"), PA.y, rOBA, `Rbat+`, BAbip, 
                                       ISO, EV, `HardH%`, `LD%`, `GB%`, `FB%`, `GB/FB`, `Pull%`,
                                       `Cent%`, `Oppo%`, cWPA, RE24, `RS%`, `SB%`, `XBT%`))
colnames(batters) = c(colnames(batters)[1:2], "Game", colnames(batters)[4:8], "current_G", "current_PA", 
                      "current_AB", "current_R", "current_H", "current_2B", "current_3B", "current_HR", 
                      "current_RBI", "current_BB", "current_SO", "current_HBP", "current_SB", 
                      "current_CS", "current_WPA", "current_BA", "current_OBP", "current_SLG",
                      "current_HR_pct", "current_SO_pct", "current_BB_pct", "prev_G", 
                      "prev_PA", "prev_AB", "prev_R", "prev_H", "prev_2B", "prev_3B", "prev_HR", 
                      "prev_RBI", "prev_SB", "prev_CS", "prev_BB", "prev_SO", "prev_BA", 
                      "prev_OBP", "prev_SLG", "prev_HBP", "prev_HR_pct", "prev_SO_pct", "prev_BB_pct", 
                      "prev_WPA")

# set NaN and NA to 0
for(i in 1:length(batters)){
  for(j in 1:nrow(batters)){
    batters[[i]][[j]] = ifelse(is.nan(batters[[i]][[j]]), 0, batters[[i]][[j]])
  }
}
batters[is.na(batters)] = 0

# create batters save df for current season
batters_save = bind_rows(batting_17, batting_18, batting_19, batting_20, batting_21)
batters_save$Season = as.numeric(batters_save$Season) + 1
colnames(batters_save) = c("Batter", colnames(batters_save)[2:length(batters_save)])
batters_save$Batter = str_replace_all(batters_save$Batter, name_char, " ")
batters_save$Batter = str_remove_all(batters_save$Batter, "[#]")
batters_save = batters_save %>% dplyr::select(-c(Age, Tm, Lg, OPS, `OPS+`, TB, GDP, SH, SF, IBB,
                                       paste0("Pos", name_char, "Summary"), PA.y, rOBA, `Rbat+`, BAbip, 
                                       ISO, EV, `HardH%`, `LD%`, `GB%`, `FB%`, `GB/FB`, `Pull%`,
                                       `Cent%`, `Oppo%`, cWPA, RE24, `RS%`, `SB%`, `XBT%`))
colnames(batters_save) = c("Batter", "prev_G", "prev_PA", "prev_AB", "prev_R", "prev_H", "prev_2B", 
                           "prev_3B", "prev_HR", "prev_RBI", "prev_SB", "prev_CS", "prev_BB", "prev_SO", 
                           "prev_BA", "prev_OBP", "prev_SLG", "prev_HBP", "Season", "prev_HR_pct", 
                           "prev_SO_pct", "prev_BB_pct", "prev_WPA")
batters_save = batters_save[batters_save$Batter != "Name",]

# set NaN and NA to 0
for(i in 1:length(batters_save)){
  for(j in 1:nrow(batters_save)){
    batters_save[[i]][[j]] = ifelse(is.nan(batters_save[[i]][[j]]), 0, batters_save[[i]][[j]])
  }
}
batters_save[is.na(batters_save)] = 0

# weighted values for batters
batters$weight_PA = ifelse(batters$current_G == 1, batters$prev_PA/batters$prev_G,
                            ifelse(batters$prev_G == 0, (batters$current_G-1)*(batters$current_PA/(batters$current_G - 1))/(batters$current_G - 1), 
                                   (batters$prev_G*(batters$prev_PA/batters$prev_G) + 
                                      exp((batters$current_G - 1)/3)*(batters$current_PA/(batters$current_G - 1)))/
                                     (batters$prev_G + exp((batters$current_G - 1)/3))))
batters$weight_AB = ifelse(batters$current_G == 1, batters$prev_AB/batters$prev_G,
                            ifelse(batters$prev_G == 0, (batters$current_G-1)*(batters$current_AB/(batters$current_G - 1))/(batters$current_G - 1), 
                                   (batters$prev_G*(batters$prev_AB/batters$prev_G) + 
                                      exp((batters$current_G - 1)/3)*(batters$current_AB/(batters$current_G - 1)))/
                                     (batters$prev_G + exp((batters$current_G - 1)/3))))
batters$weight_R = ifelse(batters$current_G == 1, batters$prev_R/batters$prev_G,
                            ifelse(batters$prev_G == 0, (batters$current_G-1)*(batters$current_R/(batters$current_G - 1))/(batters$current_G - 1), 
                                   (batters$prev_G*(batters$prev_R/batters$prev_G) + 
                                      exp((batters$current_G - 1)/3)*(batters$current_R/(batters$current_G - 1)))/
                                     (batters$prev_G + exp((batters$current_G - 1)/3))))
batters$weight_H = ifelse(batters$current_G == 1, batters$prev_H/batters$prev_G,
                            ifelse(batters$prev_G == 0, (batters$current_G-1)*(batters$current_H/(batters$current_G - 1))/(batters$current_G - 1), 
                                   (batters$prev_G*(batters$prev_H/batters$prev_G) + 
                                      exp((batters$current_G - 1)/3)*(batters$current_H/(batters$current_G - 1)))/
                                     (batters$prev_G + exp((batters$current_G - 1)/3))))
batters$weight_2B = ifelse(batters$current_G == 1, batters$prev_2B/batters$prev_G,
                            ifelse(batters$prev_G == 0, (batters$current_G-1)*(batters$current_2B/(batters$current_G - 1))/(batters$current_G - 1), 
                                   (batters$prev_G*(batters$prev_2B/batters$prev_G) + 
                                      exp((batters$current_G - 1)/3)*(batters$current_2B/(batters$current_G - 1)))/
                                     (batters$prev_G + exp((batters$current_G - 1)/3))))
batters$weight_3B = ifelse(batters$current_G == 1, batters$prev_3B/batters$prev_G,
                            ifelse(batters$prev_G == 0, (batters$current_G-1)*(batters$current_3B/(batters$current_G - 1))/(batters$current_G - 1), 
                                   (batters$prev_G*(batters$prev_3B/batters$prev_G) + 
                                      exp((batters$current_G - 1)/3)*(batters$current_3B/(batters$current_G - 1)))/
                                     (batters$prev_G + exp((batters$current_G - 1)/3))))
batters$weight_HR = ifelse(batters$current_G == 1, batters$prev_HR/batters$prev_G,
                            ifelse(batters$prev_G == 0, (batters$current_G-1)*(batters$current_HR/(batters$current_G - 1))/(batters$current_G - 1), 
                                   (batters$prev_G*(batters$prev_HR/batters$prev_G) + 
                                      exp((batters$current_G - 1)/3)*(batters$current_HR/(batters$current_G - 1)))/
                                     (batters$prev_G + exp((batters$current_G - 1)/3))))
batters$weight_RBI = ifelse(batters$current_G == 1, batters$prev_RBI/batters$prev_G,
                            ifelse(batters$prev_G == 0, (batters$current_G-1)*(batters$current_RBI/(batters$current_G - 1))/(batters$current_G - 1), 
                                   (batters$prev_G*(batters$prev_RBI/batters$prev_G) + 
                                      exp((batters$current_G - 1)/3)*(batters$current_RBI/(batters$current_G - 1)))/
                                     (batters$prev_G + exp((batters$current_G - 1)/3))))
batters$weight_BB = ifelse(batters$current_G == 1, batters$prev_BB/batters$prev_G,
                            ifelse(batters$prev_G == 0, (batters$current_G-1)*(batters$current_BB/(batters$current_G - 1))/(batters$current_G - 1), 
                                   (batters$prev_G*(batters$prev_BB/batters$prev_G) + 
                                      exp((batters$current_G - 1)/3)*(batters$current_BB/(batters$current_G - 1)))/
                                     (batters$prev_G + exp((batters$current_G - 1)/3))))
batters$weight_SO = ifelse(batters$current_G == 1, batters$prev_SO/batters$prev_G,
                           ifelse(batters$prev_G == 0, (batters$current_G-1)*(batters$current_SO/(batters$current_G - 1))/(batters$current_G - 1), 
                                  (batters$prev_G*(batters$prev_SO/batters$prev_G) + 
                                     exp((batters$current_G - 1)/3)*(batters$current_SO/(batters$current_G - 1)))/
                                    (batters$prev_G + exp((batters$current_G - 1)/3))))
batters$weight_HBP = ifelse(batters$current_G == 1, batters$prev_HBP/batters$prev_G,
                            ifelse(batters$prev_G == 0, (batters$current_G-1)*(batters$current_HBP/(batters$current_G - 1))/(batters$current_G - 1), 
                                   (batters$prev_G*(batters$prev_HBP/batters$prev_G) + 
                                      exp((batters$current_G - 1)/3)*(batters$current_HBP/(batters$current_G - 1)))/
                                     (batters$prev_G + exp((batters$current_G - 1)/3))))
batters$weight_SB = ifelse(batters$current_G == 1, batters$prev_SB/batters$prev_G,
                           ifelse(batters$prev_G == 0, (batters$current_G-1)*(batters$current_SB/(batters$current_G - 1))/(batters$current_G - 1), 
                                  (batters$prev_G*(batters$prev_SB/batters$prev_G) + 
                                     exp((batters$current_G - 1)/3)*(batters$current_SB/(batters$current_G - 1)))/
                                    (batters$prev_G + exp((batters$current_G - 1)/3))))
batters$weight_CS = ifelse(batters$current_G == 1, batters$prev_CS/batters$prev_G,
                           ifelse(batters$prev_G == 0, (batters$current_G-1)*(batters$current_CS/(batters$current_G - 1))/(batters$current_G - 1), 
                                  (batters$prev_G*(batters$prev_CS/batters$prev_G) + 
                                     exp((batters$current_G - 1)/3)*(batters$current_CS/(batters$current_G - 1)))/
                                    (batters$prev_G + exp((batters$current_G - 1)/3))))
batters$weight_WPA = ifelse(batters$current_G == 1, batters$prev_WPA/batters$prev_G,
                            ifelse(batters$prev_G == 0, (batters$current_G-1)*(batters$current_WPA/(batters$current_G - 1))/(batters$current_G - 1), 
                                   (batters$prev_G*(batters$prev_WPA/batters$prev_G) + 
                                      exp((batters$current_G - 1)/3)*(batters$current_WPA/(batters$current_G - 1)))/
                                     (batters$prev_G + exp((batters$current_G - 1)/3))))
batters$weight_BA = ifelse(batters$current_G == 1, batters$prev_BA,
                            ifelse(batters$prev_G == 0, batters$current_BA, 
                                   (batters$prev_G*batters$prev_BA + 
                                      exp((batters$current_G - 1)/3)*batters$current_BA)/
                                     (batters$prev_G + exp((batters$current_G - 1)/3))))
batters$weight_OBP = ifelse(batters$current_G == 1, batters$prev_OBP,
                            ifelse(batters$prev_G == 0, batters$current_OBP, 
                                   (batters$prev_G*batters$prev_OBP + 
                                      exp((batters$current_G - 1)/3)*batters$current_OBP)/
                                     (batters$prev_G + exp((batters$current_G - 1)/3))))
batters$weight_SLG = ifelse(batters$current_G == 1, batters$prev_SLG,
                            ifelse(batters$prev_G == 0, batters$current_SLG, 
                                   (batters$prev_G*batters$prev_SLG + 
                                      exp((batters$current_G - 1)/3)*batters$current_SLG)/
                                     (batters$prev_G + exp((batters$current_G - 1)/3))))
batters$weight_HR_pct = ifelse(batters$current_G == 1, batters$prev_HR_pct,
                            ifelse(batters$prev_G == 0, batters$current_HR_pct, 
                                   (batters$prev_G*batters$prev_HR_pct + 
                                      exp((batters$current_G - 1)/3)*batters$current_HR_pct)/
                                     (batters$prev_G + exp((batters$current_G - 1)/3))))
batters$weight_SO_pct = ifelse(batters$current_G == 1, batters$prev_SO_pct,
                            ifelse(batters$prev_G == 0, batters$current_SO_pct, 
                                   (batters$prev_G*batters$prev_SO_pct + 
                                      exp((batters$current_G - 1)/3)*batters$current_SO_pct)/
                                     (batters$prev_G + exp((batters$current_G - 1)/3))))
batters$weight_BB_pct = ifelse(batters$current_G == 1, batters$prev_BB_pct,
                            ifelse(batters$prev_G == 0, batters$current_BB_pct, 
                                   (batters$prev_G*batters$prev_BB_pct + 
                                      exp((batters$current_G - 1)/3)*batters$current_BB_pct)/
                                     (batters$prev_G + exp((batters$current_G - 1)/3))))

# BIND TOGETHER ALL SECTIONS

main = left_join(batters, team_pitching, by = c("Team", "Game", "Season", "Date", "Opponent"))
main = main[!is.na(main$Opp_Game),]

# add stat results
stat_results = games
stat_results = stat_results %>% dplyr::select(c(Batter, Gcar, H, `2B`, `3B`, HR, RBI, R, BB, HBP, SB))
main = left_join(main, stat_results, by = c("Batter", "Gcar"))
main = main[!is.na(main$weight_PA),]

# WRITE NECESSARY FILES TO CSV

write_csv(main, "draftkings_mlb_bat.csv")
write_csv(batters_save, "batters_save.csv")
write_csv(pitching_save, "pitching_save.csv")

