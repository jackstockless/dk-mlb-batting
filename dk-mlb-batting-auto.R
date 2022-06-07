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


## CURRENT SEASON (START HERE FOR NEW GAMES)

# YESTERDAY'S RESULTS

date_yest = master$Date[1]
players_yest = as.vector(master$Batter)
results = sapply(1:length(players_yest), function(i) try(batter_fn(players_yest[i], year = 2022), TRUE))
verify = data.frame()
for(i in 1:length(results)){
  a = ifelse(is.data.frame(results[[i]]), 1, 0)
  verify = rbind(verify, a)
}
verify$num = 1:length(results)
verify = verify[verify[,1] == 0,]
results = if(nrow(verify) == 0){results} else{list.remove(results, verify$num)}
players_yest = if(nrow(verify) == 0){players_yest} else{players_yest[-verify$num]}
for(i in 1:length(results)){
  results[[i]][1] = players_yest[i]
  hold = results[[i]]
  month = str_sub(hold$Date, 1, 3)
  month = ifelse(month == "Mar", 3, ifelse(month == "Apr", 4, ifelse(month == "May", 5, 
                                                                     ifelse(month == "Jun", 6, ifelse(month == "Jul", 7, ifelse(month == "Aug", 8, 
                                                                                                                                ifelse(month == "Sep", 9, ifelse(month == "Oct", 10, NA))))))))
  day = str_sub(hold$Date, 5, 6)
  day = str_remove_all(day, "[()]")
  hold$Date = as.Date(paste(2022, "-", month, "-", day, sep = ""))
  hold = hold[hold$Date == date_yest,]
  hold = data.frame(hold$Rk, hold$Date, hold$R, hold$H, hold$`2B`, hold$`3B`, hold$HR, hold$RBI,
                    hold$BB, hold$HBP, hold$SB)
  results[[i]] = hold
}
results = results %>%
  do.call("rbind", .)
colnames(results) = c("Batter", "Date", "R", "H", "2B", "3B", "HR", "RBI", "BB", "HBP", "SB")
results$Date = as.Date(results$Date)
cols = c(3:11)
results[,cols] = sapply(results[,cols], function(x) as.numeric(as.character(x)))
results = results[!is.na(results$Batter),]

# join with master
results = left_join(master, results, by = c("Batter", "Date"))

# fill in main file
main$R[(nrow(main) - nrow(master) + 1):nrow(main)] = results$R
main$H[(nrow(main) - nrow(master) + 1):nrow(main)] = results$H
main$`2B`[(nrow(main) - nrow(master) + 1):nrow(main)] = results$`2B`
main$`3B`[(nrow(main) - nrow(master) + 1):nrow(main)] = results$`3B`
main$HR[(nrow(main) - nrow(master) + 1):nrow(main)] = results$HR
main$RBI[(nrow(main) - nrow(master) + 1):nrow(main)] = results$RBI
main$BB[(nrow(main) - nrow(master) + 1):nrow(main)] = results$BB
main$HBP[(nrow(main) - nrow(master) + 1):nrow(main)] = results$HBP
main$SB[(nrow(main) - nrow(master) + 1):nrow(main)] = results$SB

# after filling in missing values manually
main = main[!is.na(main$H),]

# ELO

date_today = Sys.Date()
ratings = fread("https://projects.fivethirtyeight.com/mlb-api/mlb_elo_latest.csv")
ratings = ratings[ratings$date == date_today,]
ratings = ratings %>% dplyr::select(c(date, season, team1, team2, rating1_pre, rating2_pre, pitcher1,
                                      pitcher2, pitcher1_rgs, pitcher2_rgs))
colnames(ratings) = c("Date", "Season", colnames(ratings)[3:length(ratings)])
ratings$team1 = ifelse(ratings$team1 == "TBD", "TBR", ratings$team1)
ratings$team1 = ifelse(ratings$team1 == "ANA", "LAA", ratings$team1)
ratings$team1 = ifelse(ratings$team1 == "FLA", "MIA", ratings$team1)
ratings$team2 = ifelse(ratings$team2 == "TBD", "TBR", ratings$team2)
ratings$team2 = ifelse(ratings$team2 == "ANA", "LAA", ratings$team2)
ratings$team2 = ifelse(ratings$team2 == "FLA", "MIA", ratings$team2)

# function to get game numbers for each team
game_num_fn_2 = function(entry, data){
  t1 = data$team1[entry]
  t2 = data$team2[entry]
  team1_game = nrow(data[data$team1 == t1,]) + nrow(data[data$team2 == t1,])
  team2_game = nrow(data[data$team1 == t2,]) + nrow(data[data$team2 == t2,])
  games = data.frame(t1, t2, team1_game, team2_game)
  colnames(games) = c("team1", "team2", "team1_game", "team2_game")
  return(games)
}

game_num_22 = fread("https://projects.fivethirtyeight.com/mlb-api/mlb_elo_latest.csv")
game_num_22 = as.data.frame(game_num_22)
game_num_22$team1 = ifelse(game_num_22$team1 == "TBD", "TBR", game_num_22$team1)
game_num_22$team1 = ifelse(game_num_22$team1 == "ANA", "LAA", game_num_22$team1)
game_num_22$team1 = ifelse(game_num_22$team1 == "FLA", "MIA", game_num_22$team1)
game_num_22$team2 = ifelse(game_num_22$team2 == "TBD", "TBR", game_num_22$team2)
game_num_22$team2 = ifelse(game_num_22$team2 == "ANA", "LAA", game_num_22$team2)
game_num_22$team2 = ifelse(game_num_22$team2 == "FLA", "MIA", game_num_22$team2)
game_num_22 = game_num_22[game_num_22$date <= date_today,]
game_num_22 = lapply(1:nrow(ratings), game_num_fn_2, data = game_num_22) %>% do.call("rbind", .)
ratings = left_join(ratings, game_num_22, by = c("team1", "team2"))
ratings = distinct(ratings)

# correct game num for doubleheaders
game_num_fn_3 = function(tm, data){
  df = data[data$team1 == tm,]
  index = rep(nrow(df) - 1, nrow(df))
  df$team1_game = df$team1_game - index
  df$team2_game = df$team2_game - index
  return(df)
}
ratings = lapply(unique(ratings$team1), game_num_fn_3, data = ratings) %>% do.call("rbind", .)

# duplicate for away team, clean up
away_elo = ratings
away_elo$Team = away_elo$team2
away_elo$Opponent = away_elo$team1
ratings$Team = ratings$team1
ratings$Opponent = ratings$team2
ratings = bind_rows(ratings, away_elo)
ratings$rating_diff = ifelse(ratings$Team == ratings$team1, ratings$rating1_pre - ratings$rating2_pre,
                             ratings$rating2_pre - ratings$rating1_pre) 
ratings$Game = ifelse(ratings$Team == ratings$team1, ratings$team1_game, ratings$team2_game)
ratings$Opp_Game = ifelse(ratings$Team == ratings$team1, ratings$team2_game, ratings$team1_game)

# TEAM PITCHING STATS

pitching_box_22 = lapply(teams, mlb_fn, year = 2022, set = "p") %>%
  do.call("rbind", .)
colnames(pitching_box_22) = p_names
pitching_box_22 = pitching_box_22 %>% dplyr::select(-Rank)
pitching_box_22 = pitching_box_22[pitching_box_22$Opponent != "Opp",]
pitching_box_22$Home = ifelse(pitching_box_22$Home == "@", "Away", "Home")
cols = c(1,6:31)
pitching_box_22[,cols] = sapply(pitching_box_22[,cols], as.numeric)
pitching_box_22$Season = 2022
month = str_sub(pitching_box_22$Date, 1, 3)
month = ifelse(month == "Mar", 3, ifelse(month == "Apr", 4, ifelse(month == "May", 5, 
                                                                   ifelse(month == "Jun", 6, ifelse(month == "Jul", 7, ifelse(month == "Aug", 8, 
                                                                                                                              ifelse(month == "Sep", 9, ifelse(month == "Oct", 10, NA))))))))
day = str_sub(pitching_box_22$Date, 5, -1)
day = ifelse(str_length(day) == 4, str_sub(day, 1, 1),
             ifelse(str_length(day) == 5, str_sub(day, 1, 2),
                    day))
pitching_box_22$Date = as.Date(paste(pitching_box_22$Season, "-", month, "-", day, sep = ""))

team_stats = data.frame()
for(j in teams){
  df = pitching_box_22[pitching_box_22$Team == j,]
  games = max(df$Game)
  df = data.frame(games + 1, j, mean(df$H), mean(df$R), mean(df$BB), mean(df$SO), mean(df$HR), 
                  mean(df$HBP), mean(df$BF), mean(df$Pit), mean(df$Str), mean(df$SB), mean(df$CS),
                  mean(df$AB), mean(df$`2B`), mean(df$`3B`), mean(df$SF))
  team_stats = bind_rows(team_stats, df)
}
colnames(team_stats) = c("Opp_Game", "Opponent", "opp_rolling_H", "opp_rolling_R", "opp_rolling_BB", 
                         "opp_rolling_SO", "opp_rolling_HR", "opp_rolling_HBP", "opp_rolling_BF", 
                         "opp_rolling_Pit", "opp_rolling_Str", "opp_rolling_SB", "opp_rolling_CS", 
                         "opp_rolling_AB", "opp_rolling_2B", "opp_rolling_3B", "opp_rolling_SF")

# match everything up
team_pitching_22 = left_join(ratings, team_stats, by = c("Opponent", "Opp_Game"))
team_pitching_22$Season = as.numeric(team_pitching_22$Season)
team_pitching_22 = team_pitching_22 %>% dplyr::select(-c(team1_game, team2_game))
team_pitching_22$Pitcher = ifelse(team_pitching_22$Team == team_pitching_22$team1, team_pitching_22$pitcher1, 
                              team_pitching_22$pitcher2)
team_pitching_22$RGS = ifelse(team_pitching_22$Team == team_pitching_22$team1, team_pitching_22$pitcher1_rgs, 
                          team_pitching_22$pitcher2_rgs)

# BATTER STATS

# 2022
# summary
batting_22 = batting_fn(2022, "standard")
cols = c(1,3,6:29)
batting_22[,cols] = sapply(batting_22[,cols], as.numeric)
batting_22$Season = 2022
batting_22 = lapply(unique(batting_22$Name), rm_dups, set = batting_22) %>%
  do.call("rbind", .)
batting_22 = batting_22 %>% dplyr::select(-Rk)

# advanced
batting_adv_22 = batting_fn(2022, "advanced")
cols = c(10:12,14:17,19:21,23,25:27)
batting_adv_22[,cols] = sapply(cols, function(x) str_remove_all(batting_adv_22[,x], "[%]"))
cols = c(1,3,5:27)
batting_adv_22[,cols] = sapply(batting_adv_22[,cols], as.numeric)
batting_adv_22 = lapply(unique(batting_adv_22$Name), rm_dups, set = batting_adv_22) %>%
  do.call("rbind", .)
batting_adv_22 = batting_adv_22 %>% dplyr::select(-Rk)

# join
batting_22 = left_join(batting_22, batting_adv_22, by = c("Name", "Age", "Tm"))

# pare down
batting_22 = batting_22 %>% dplyr::select(c(Name, Tm, G, PA.x, AB, R, H, `2B`, `3B`, HR, RBI, BB, SO, HBP,
                                            SB, CS, WPA, BA, OBP, SLG, `HR%`, `SO%`, `BB%`, Season))
colnames(batting_22) = c("Batter", "Team", "current_G", "current_PA", "current_AB", "current_R", "current_H", 
                          "current_2B", "current_3B", "current_HR", "current_RBI", "current_BB",
                          "current_SO", "current_HBP", "current_SB", "current_CS",
                         "current_WPA", "current_BA", "current_OBP", "current_SLG", "current_HR_pct", 
                         "current_SO_pct", "current_BB_pct", "Season")
batting_22$Batter = str_replace_all(batting_22$Batter, name_char, " ")
batting_22$Batter = str_remove_all(batting_22$Batter, "[#]")

# BIND ALL SECTIONS TOGETHER; ATTACH TO MAIN SHEET

master = left_join(batting_22, team_pitching_22, by = c("Team", "Season"))
master = master[!is.na(master$rating_diff),]

# join teams' previous and current seasons
master = left_join(master, pitching_save, by = c("Opponent", "Season"))

# join players' previous and current seasons
master = left_join(master, batters_save, by = c("Batter", "Season"))

# set NaN and NA to 0
for(i in 1:length(master)){
  for(j in 1:nrow(master)){
    master[[i]][[j]] = ifelse(is.nan(master[[i]][[j]]), 0, master[[i]][[j]])
  }
}
master$pitcher1 = ifelse(is.na(master$pitcher1), "Blank", master$pitcher1)
master$pitcher2 = ifelse(is.na(master$pitcher2), "Blank", master$pitcher2)
master = master[!is.na(master$Pitcher),]
master[is.na(master)] = 0

# adjust current_GS
master$current_G = master$current_G + 1

# weighted values for teams
master$opp_weight_H = ifelse(master$Season == 2021,
                                    (60*master$opp_prev_H + exp((master$Opp_Game - 1)/3)*
                                       master$opp_rolling_H)/(60 + exp((master$Opp_Game - 1)/3)),
                                    (162*master$opp_prev_H + exp((master$Opp_Game - 1)/3)*
                                       master$opp_rolling_H)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_R = ifelse(master$Season == 2021,
                                    (60*master$opp_prev_R + exp((master$Opp_Game - 1)/3)*
                                       master$opp_rolling_R)/(60 + exp((master$Opp_Game - 1)/3)),
                                    (162*master$opp_prev_R + exp((master$Opp_Game - 1)/3)*
                                       master$opp_rolling_R)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_BB = ifelse(master$Season == 2021,
                                     (60*master$opp_prev_BB + exp((master$Opp_Game - 1)/3)*
                                        master$opp_rolling_BB)/(60 + exp((master$Opp_Game - 1)/3)),
                                     (162*master$opp_prev_BB + exp((master$Opp_Game - 1)/3)*
                                        master$opp_rolling_BB)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_SO = ifelse(master$Season == 2021,
                                     (60*master$opp_prev_SO + exp((master$Opp_Game - 1)/3)*
                                        master$opp_rolling_SO)/(60 + exp((master$Opp_Game - 1)/3)),
                                     (162*master$opp_prev_SO + exp((master$Opp_Game - 1)/3)*
                                        master$opp_rolling_SO)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_HR = ifelse(master$Season == 2021,
                                     (60*master$opp_prev_HR + exp((master$Opp_Game - 1)/3)*
                                        master$opp_rolling_HR)/(60 + exp((master$Opp_Game - 1)/3)),
                                     (162*master$opp_prev_HR + exp((master$Opp_Game - 1)/3)*
                                        master$opp_rolling_HR)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_HBP = ifelse(master$Season == 2021,
                                      (60*master$opp_prev_HBP + exp((master$Opp_Game - 1)/3)*
                                         master$opp_rolling_HBP)/(60 + exp((master$Opp_Game - 1)/3)),
                                      (162*master$opp_prev_HBP + exp((master$Opp_Game - 1)/3)*
                                         master$opp_rolling_HBP)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_BF = ifelse(master$Season == 2021,
                                     (60*master$opp_prev_BF + exp((master$Opp_Game - 1)/3)*
                                        master$opp_rolling_BF)/(60 + exp((master$Opp_Game - 1)/3)),
                                     (162*master$opp_prev_BF + exp((master$Opp_Game - 1)/3)*
                                        master$opp_rolling_BF)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_Pit = ifelse(master$Season == 2021,
                                      (60*master$opp_prev_Pit + exp((master$Opp_Game - 1)/3)*
                                         master$opp_rolling_Pit)/(60 + exp((master$Opp_Game - 1)/3)),
                                      (162*master$opp_prev_Pit + exp((master$Opp_Game - 1)/3)*
                                         master$opp_rolling_Pit)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_Str = ifelse(master$Season == 2021,
                                      (60*master$opp_prev_Str + exp((master$Opp_Game - 1)/3)*
                                         master$opp_rolling_Str)/(60 + exp((master$Opp_Game - 1)/3)),
                                      (162*master$opp_prev_Str + exp((master$Opp_Game - 1)/3)*
                                         master$opp_rolling_Str)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_SB = ifelse(master$Season == 2021,
                                     (60*master$opp_prev_SB + exp((master$Opp_Game - 1)/3)*
                                        master$opp_rolling_SB)/(60 + exp((master$Opp_Game - 1)/3)),
                                     (162*master$opp_prev_SB + exp((master$Opp_Game - 1)/3)*
                                        master$opp_rolling_SB)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_CS = ifelse(master$Season == 2021,
                                     (60*master$opp_prev_CS + exp((master$Opp_Game - 1)/3)*
                                        master$opp_rolling_CS)/(60 + exp((master$Opp_Game - 1)/3)),
                                     (162*master$opp_prev_CS + exp((master$Opp_Game - 1)/3)*
                                        master$opp_rolling_CS)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_AB = ifelse(master$Season == 2021,
                                     (60*master$opp_prev_AB + exp((master$Opp_Game - 1)/3)*
                                        master$opp_rolling_AB)/(60 + exp((master$Opp_Game - 1)/3)),
                                     (162*master$opp_prev_AB + exp((master$Opp_Game - 1)/3)*
                                        master$opp_rolling_AB)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_2B = ifelse(master$Season == 2021,
                                     (60*master$opp_prev_2B + exp((master$Opp_Game - 1)/3)*
                                        master$opp_rolling_2B)/(60 + exp((master$Opp_Game - 1)/3)),
                                     (162*master$opp_prev_2B + exp((master$Opp_Game - 1)/3)*
                                        master$opp_rolling_2B)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_3B = ifelse(master$Season == 2021,
                                     (60*master$opp_prev_3B + exp((master$Opp_Game - 1)/3)*
                                        master$opp_rolling_3B)/(60 + exp((master$Opp_Game - 1)/3)),
                                     (162*master$opp_prev_3B + exp((master$Opp_Game - 1)/3)*
                                        master$opp_rolling_3B)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_SF = ifelse(master$Season == 2021,
                                     (60*master$opp_prev_SF + exp((master$Opp_Game - 1)/3)*
                                        master$opp_rolling_SF)/(60 + exp((master$Opp_Game - 1)/3)),
                                     (162*master$opp_prev_SF + exp((master$Opp_Game - 1)/3)*
                                        master$opp_rolling_SF)/(162 + exp((master$Opp_Game - 1)/3)))
master$opp_weight_BA = master$opp_weight_H/master$opp_weight_AB
master$opp_weight_OBP = (master$opp_weight_H + master$opp_weight_BB + master$opp_weight_HBP)/(master$opp_weight_AB + master$opp_weight_BB + master$opp_weight_HBP + master$opp_weight_SF)
master$opp_weight_SLG = ((master$opp_weight_H - master$opp_weight_2B - master$opp_weight_3B - master$opp_weight_HR) + 2*master$opp_weight_2B + 3*master$opp_weight_3B + 4*master$opp_weight_HR)/master$opp_weight_AB
master$opp_weight_HR_pct = master$opp_weight_HR/(master$opp_weight_AB + master$opp_weight_BB + master$opp_weight_HBP + master$opp_weight_SF)
master$opp_weight_SO_pct = master$opp_weight_SO/(master$opp_weight_AB + master$opp_weight_BB + master$opp_weight_HBP + master$opp_weight_SF)
master$opp_weight_BB_pct = master$opp_weight_BB/(master$opp_weight_AB + master$opp_weight_BB + master$opp_weight_HBP + master$opp_weight_SF)

# weighted values for batters
master$weight_PA = ifelse(master$current_G == 1, master$prev_PA/master$prev_G,
                           ifelse(master$prev_G == 0, (master$current_G-1)*(master$current_PA/(master$current_G - 1))/(master$current_G - 1), 
                                  (master$prev_G*(master$prev_PA/master$prev_G) + 
                                     exp((master$current_G - 1)/3)*(master$current_PA/(master$current_G - 1)))/
                                    (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_AB = ifelse(master$current_G == 1, master$prev_AB/master$prev_G,
                           ifelse(master$prev_G == 0, (master$current_G-1)*(master$current_AB/(master$current_G - 1))/(master$current_G - 1), 
                                  (master$prev_G*(master$prev_AB/master$prev_G) + 
                                     exp((master$current_G - 1)/3)*(master$current_AB/(master$current_G - 1)))/
                                    (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_R = ifelse(master$current_G == 1, master$prev_R/master$prev_G,
                          ifelse(master$prev_G == 0, (master$current_G-1)*(master$current_R/(master$current_G - 1))/(master$current_G - 1), 
                                 (master$prev_G*(master$prev_R/master$prev_G) + 
                                    exp((master$current_G - 1)/3)*(master$current_R/(master$current_G - 1)))/
                                   (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_H = ifelse(master$current_G == 1, master$prev_H/master$prev_G,
                          ifelse(master$prev_G == 0, (master$current_G-1)*(master$current_H/(master$current_G - 1))/(master$current_G - 1), 
                                 (master$prev_G*(master$prev_H/master$prev_G) + 
                                    exp((master$current_G - 1)/3)*(master$current_H/(master$current_G - 1)))/
                                   (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_2B = ifelse(master$current_G == 1, master$prev_2B/master$prev_G,
                           ifelse(master$prev_G == 0, (master$current_G-1)*(master$current_2B/(master$current_G - 1))/(master$current_G - 1), 
                                  (master$prev_G*(master$prev_2B/master$prev_G) + 
                                     exp((master$current_G - 1)/3)*(master$current_2B/(master$current_G - 1)))/
                                    (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_3B = ifelse(master$current_G == 1, master$prev_3B/master$prev_G,
                           ifelse(master$prev_G == 0, (master$current_G-1)*(master$current_3B/(master$current_G - 1))/(master$current_G - 1), 
                                  (master$prev_G*(master$prev_3B/master$prev_G) + 
                                     exp((master$current_G - 1)/3)*(master$current_3B/(master$current_G - 1)))/
                                    (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_HR = ifelse(master$current_G == 1, master$prev_HR/master$prev_G,
                           ifelse(master$prev_G == 0, (master$current_G-1)*(master$current_HR/(master$current_G - 1))/(master$current_G - 1), 
                                  (master$prev_G*(master$prev_HR/master$prev_G) + 
                                     exp((master$current_G - 1)/3)*(master$current_HR/(master$current_G - 1)))/
                                    (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_RBI = ifelse(master$current_G == 1, master$prev_RBI/master$prev_G,
                            ifelse(master$prev_G == 0, (master$current_G-1)*(master$current_RBI/(master$current_G - 1))/(master$current_G - 1), 
                                   (master$prev_G*(master$prev_RBI/master$prev_G) + 
                                      exp((master$current_G - 1)/3)*(master$current_RBI/(master$current_G - 1)))/
                                     (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_BB = ifelse(master$current_G == 1, master$prev_BB/master$prev_G,
                           ifelse(master$prev_G == 0, (master$current_G-1)*(master$current_BB/(master$current_G - 1))/(master$current_G - 1), 
                                  (master$prev_G*(master$prev_BB/master$prev_G) + 
                                     exp((master$current_G - 1)/3)*(master$current_BB/(master$current_G - 1)))/
                                    (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_SO = ifelse(master$current_G == 1, master$prev_SO/master$prev_G,
                           ifelse(master$prev_G == 0, (master$current_G-1)*(master$current_SO/(master$current_G - 1))/(master$current_G - 1), 
                                  (master$prev_G*(master$prev_SO/master$prev_G) + 
                                     exp((master$current_G - 1)/3)*(master$current_SO/(master$current_G - 1)))/
                                    (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_HBP = ifelse(master$current_G == 1, master$prev_HBP/master$prev_G,
                            ifelse(master$prev_G == 0, (master$current_G-1)*(master$current_HBP/(master$current_G - 1))/(master$current_G - 1), 
                                   (master$prev_G*(master$prev_HBP/master$prev_G) + 
                                      exp((master$current_G - 1)/3)*(master$current_HBP/(master$current_G - 1)))/
                                     (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_SB = ifelse(master$current_G == 1, master$prev_SB/master$prev_G,
                           ifelse(master$prev_G == 0, (master$current_G-1)*(master$current_SB/(master$current_G - 1))/(master$current_G - 1), 
                                  (master$prev_G*(master$prev_SB/master$prev_G) + 
                                     exp((master$current_G - 1)/3)*(master$current_SB/(master$current_G - 1)))/
                                    (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_CS = ifelse(master$current_G == 1, master$prev_CS/master$prev_G,
                           ifelse(master$prev_G == 0, (master$current_G-1)*(master$current_CS/(master$current_G - 1))/(master$current_G - 1), 
                                  (master$prev_G*(master$prev_CS/master$prev_G) + 
                                     exp((master$current_G - 1)/3)*(master$current_CS/(master$current_G - 1)))/
                                    (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_WPA = ifelse(master$current_G == 1, master$prev_WPA/master$prev_G,
                            ifelse(master$prev_G == 0, (master$current_G-1)*(master$current_WPA/(master$current_G - 1))/(master$current_G - 1), 
                                   (master$prev_G*(master$prev_WPA/master$prev_G) + 
                                      exp((master$current_G - 1)/3)*(master$current_WPA/(master$current_G - 1)))/
                                     (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_BA = ifelse(master$current_G == 1, master$prev_BA,
                           ifelse(master$prev_G == 0, master$current_BA, 
                                  (master$prev_G*master$prev_BA + 
                                     exp((master$current_G - 1)/3)*master$current_BA)/
                                    (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_OBP = ifelse(master$current_G == 1, master$prev_OBP,
                            ifelse(master$prev_G == 0, master$current_OBP, 
                                   (master$prev_G*master$prev_OBP + 
                                      exp((master$current_G - 1)/3)*master$current_OBP)/
                                     (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_SLG = ifelse(master$current_G == 1, master$prev_SLG,
                            ifelse(master$prev_G == 0, master$current_SLG, 
                                   (master$prev_G*master$prev_SLG + 
                                      exp((master$current_G - 1)/3)*master$current_SLG)/
                                     (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_HR_pct = ifelse(master$current_G == 1, master$prev_HR_pct,
                               ifelse(master$prev_G == 0, master$current_HR_pct, 
                                      (master$prev_G*master$prev_HR_pct + 
                                         exp((master$current_G - 1)/3)*master$current_HR_pct)/
                                        (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_SO_pct = ifelse(master$current_G == 1, master$prev_SO_pct,
                               ifelse(master$prev_G == 0, master$current_SO_pct, 
                                      (master$prev_G*master$prev_SO_pct + 
                                         exp((master$current_G - 1)/3)*master$current_SO_pct)/
                                        (master$prev_G + exp((master$current_G - 1)/3))))
master$weight_BB_pct = ifelse(master$current_G == 1, master$prev_BB_pct,
                               ifelse(master$prev_G == 0, master$current_BB_pct, 
                                      (master$prev_G*master$prev_BB_pct + 
                                         exp((master$current_G - 1)/3)*master$current_BB_pct)/
                                        (master$prev_G + exp((master$current_G - 1)/3))))

# remove brand new batters
master = master[!is.na(master$weight_PA),]

# numberFire PAs
get_nF = function(){
  url = "https://www.numberfire.com/mlb/daily-fantasy/daily-baseball-projections/batters"
  log = read_html(url) %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  nF = log[[4]]
  colnames(nF) = nF[2,]
  nF = nF[-c(1:2),]
  nF = nF %>% dplyr::select(-Salary)
  cols = c(2:length(nF))
  nF[,cols] = sapply(nF[,cols], as.numeric)
  return(nF)
}
number_fire = get_nF()
nF_players = str_split(number_fire$Player, "\n")
nF_players = do.call("rbind", nF_players)
nF_players = trimws(nF_players[,3])
number_fire$Player = nF_players

nF_PA = number_fire %>% dplyr::select(c(Player, PA)) %>% transmute("Batter" = Player, "nF_PA" = PA)
pas = master %>% dplyr::select(c(Batter, weight_PA))
pas = left_join(nF_PA, pas, by = "Batter")
nF_PA = nF_PA %>% filter(!is.na(nF_PA))

# pare down master, get rid of incompatible entries
master = left_join(master, nF_PA, by = "Batter")
master$Date = as.Date(master$Date)
master = master %>% filter(!is.na(master$Date))
master = distinct(master)

## COMBINE MAIN AND MASTER

main = bind_rows(main, master)


#### PREDICTIONS ####

# dependent variables
single = main$H - main$`2B` - main$`3B` - main$HR
double = main$`2B`
triple = main$`3B`
HR = main$HR
RBI = main$RBI
R = main$R
BB = main$BB
HBP = main$HBP
SB = main$SB

# independent variables
rating_diff = as.vector(scale(main$rating_diff, center = T))
RGS = as.vector(scale(main$RGS, center = T))
BOP = as.vector(scale(main$BOP, center = T))
weight_PA = as.vector(scale(main$weight_PA, center = T))
weight_2B = as.vector(scale(main$weight_2B, center = T))
weight_3B = as.vector(scale(main$weight_3B, center = T))
weight_RBI = as.vector(scale(main$weight_RBI, center = T))
weight_SB = as.vector(scale(main$weight_SB, center = T))
weight_CS = as.vector(scale(main$weight_CS, center = T))
weight_WPA = as.vector(scale(main$weight_WPA, center = T))
weight_BA = as.vector(scale(main$weight_BA, center = T))
weight_OBP = as.vector(scale(main$weight_OBP, center = T))
weight_SLG = as.vector(scale(main$weight_SLG, center = T))
weight_HR_pct = as.vector(scale(main$weight_HR_pct, center = T))
weight_SO_pct = as.vector(scale(main$weight_SO_pct, center = T))
weight_BB_pct = as.vector(scale(main$weight_BB_pct, center = T))
weight_HBP = as.vector(scale(main$weight_HBP, center = T))
opp_weight_2B = as.vector(scale(main$opp_weight_2B, center = T))
opp_weight_3B = as.vector(scale(main$opp_weight_3B, center = T))
opp_weight_SB = as.vector(scale(main$opp_weight_SB, center = T))
opp_weight_CS = as.vector(scale(main$opp_weight_CS, center = T))
opp_weight_BA = as.vector(scale(main$opp_weight_BA, center = T))
opp_weight_OBP = as.vector(scale(main$opp_weight_OBP, center = T))
opp_weight_SLG = as.vector(scale(main$opp_weight_SLG, center = T))
opp_weight_HR_pct = as.vector(scale(main$opp_weight_HR_pct, center = T))
opp_weight_SO_pct = as.vector(scale(main$opp_weight_SO_pct, center = T))
opp_weight_BB_pct = as.vector(scale(main$opp_weight_BB_pct, center = T))
opp_weight_HBP = as.vector(scale(main$opp_weight_HBP, center = T))
opp_weight_Str_pct = main$opp_weight_Str/main$opp_weight_Pit
opp_weight_Str_pct = as.vector(scale(opp_weight_Str_pct, center = T))
n = nrow(main)


## DRAFTKINGS POINTS

# Singles
single_preds = single_summary$statistics[,1]*t(data.frame(rep(1, nrow(master)),
                                                          rating_diff[(nrow(main) - nrow(master) +1):nrow(main)],
                                                          RGS[(nrow(main) - nrow(master) +1):nrow(main)],
                                                          weight_PA[(nrow(main) - nrow(master) +1):nrow(main)],
                                                          weight_BA[(nrow(main) - nrow(master) +1):nrow(main)],
                                                          weight_SLG[(nrow(main) - nrow(master) +1):nrow(main)],
                                                          weight_SO_pct[(nrow(main) - nrow(master) +1):nrow(main)],
                                                          opp_weight_BA[(nrow(main) - nrow(master) +1):nrow(main)],
                                                          opp_weight_SO_pct[(nrow(main) - nrow(master) +1):nrow(main)]))
single_preds = sapply(data.frame(single_preds), sum)
single_preds = sapply(data.frame(single_preds), exp)

# Doubles
double_preds = double_summary$statistics[,1]*t(data.frame(rep(1, nrow(master)),
                                                          weight_PA[(nrow(main) - nrow(master) +1):nrow(main)],
                                                          weight_BA[(nrow(main) - nrow(master) +1):nrow(main)],
                                                          weight_2B[(nrow(main) - nrow(master) +1):nrow(main)],
                                                          opp_weight_2B[(nrow(main) - nrow(master) +1):nrow(main)],
                                                          opp_weight_SLG[(nrow(main) - nrow(master) +1):nrow(main)]
                                                          ))
double_preds = sapply(data.frame(double_preds), sum)
double_preds = sapply(data.frame(double_preds), exp)

# Triples
triple_preds = triple_summary$statistics[,1]*t(data.frame(rep(1, nrow(master)),
                                                          weight_PA[(nrow(main) - nrow(master) +1):nrow(main)],
                                                          weight_SB[(nrow(main) - nrow(master) +1):nrow(main)],
                                                          weight_3B[(nrow(main) - nrow(master) +1):nrow(main)],
                                                          opp_weight_3B[(nrow(main) - nrow(master) +1):nrow(main)],
                                                          opp_weight_SLG[(nrow(main) - nrow(master) +1):nrow(main)]
                                                          ))
triple_preds = sapply(data.frame(triple_preds), sum)
triple_preds = sapply(data.frame(triple_preds), exp)

# HR
hr_preds = hr_summary$statistics[,1]*t(data.frame(rep(1, nrow(master)),
                                                  rating_diff[(nrow(main) - nrow(master) +1):nrow(main)],
                                                  weight_PA[(nrow(main) - nrow(master) +1):nrow(main)],
                                                  weight_SLG[(nrow(main) - nrow(master) +1):nrow(main)],
                                                  weight_HR_pct[(nrow(main) - nrow(master) +1):nrow(main)],
                                                  opp_weight_HR_pct[(nrow(main) - nrow(master) +1):nrow(main)]
                                                  ))
hr_preds = sapply(data.frame(hr_preds), sum)
hr_preds = sapply(data.frame(hr_preds), exp)

# RBI
rbi_preds = rbi_summary$statistics[,1]*t(data.frame(rep(1, nrow(master)),
                                                    rating_diff[(nrow(main) - nrow(master) +1):nrow(main)],
                                                    weight_PA[(nrow(main) - nrow(master) +1):nrow(main)],
                                                    weight_RBI[(nrow(main) - nrow(master) +1):nrow(main)],
                                                    weight_SLG[(nrow(main) - nrow(master) +1):nrow(main)],
                                                    opp_weight_SLG[(nrow(main) - nrow(master) +1):nrow(main)],
                                                    opp_weight_SO_pct[(nrow(main) - nrow(master) +1):nrow(main)],
                                                    opp_weight_BB_pct[(nrow(main) - nrow(master) +1):nrow(main)]
                                                    ))
rbi_preds = sapply(data.frame(rbi_preds), sum)
rbi_preds = sapply(data.frame(rbi_preds), exp)

# R
r_preds = r_summary$statistics[,1]*t(data.frame(rep(1, nrow(master)),
                                                rating_diff[(nrow(main) - nrow(master) +1):nrow(main)],
                                                weight_PA[(nrow(main) - nrow(master) +1):nrow(main)],
                                                weight_SLG[(nrow(main) - nrow(master) +1):nrow(main)],
                                                opp_weight_SLG[(nrow(main) - nrow(master) +1):nrow(main)],
                                                opp_weight_BB_pct[(nrow(main) - nrow(master) +1):nrow(main)]
                                                ))
r_preds = sapply(data.frame(r_preds), sum)
r_preds = sapply(data.frame(r_preds), exp)

# BB
bb_preds = bb_summary$statistics[,1]*t(data.frame(rep(1, nrow(master)),
                                                  rating_diff[(nrow(main) - nrow(master) +1):nrow(main)],
                                                  weight_PA[(nrow(main) - nrow(master) +1):nrow(main)],
                                                  weight_OBP[(nrow(main) - nrow(master) +1):nrow(main)],
                                                  weight_BB_pct[(nrow(main) - nrow(master) +1):nrow(main)],
                                                  opp_weight_BB_pct[(nrow(main) - nrow(master) +1):nrow(main)],
                                                  opp_weight_Str_pct[(nrow(main) - nrow(master) +1):nrow(main)]))
bb_preds = sapply(data.frame(bb_preds), sum)
bb_preds = sapply(data.frame(bb_preds), exp)

# HBP
hbp_preds = hbp_summary$statistics[,1]*t(data.frame(rep(1, nrow(master)),
                                                    rating_diff[(nrow(main) - nrow(master) +1):nrow(main)],
                                                    weight_PA[(nrow(main) - nrow(master) +1):nrow(main)],
                                                    weight_HBP[(nrow(main) - nrow(master) +1):nrow(main)],
                                                    opp_weight_HBP[(nrow(main) - nrow(master) +1):nrow(main)],
                                                    opp_weight_Str_pct[(nrow(main) - nrow(master) +1):nrow(main)]))
hbp_preds = sapply(data.frame(hbp_preds), sum)
hbp_preds = sapply(data.frame(hbp_preds), exp)

# SB
sb_preds = sb_summary$statistics[,1]*t(data.frame(rep(1, nrow(master)),
                                                  weight_PA[(nrow(main) - nrow(master) +1):nrow(main)],
                                                  weight_SB[(nrow(main) - nrow(master) +1):nrow(main)],
                                                  weight_CS[(nrow(main) - nrow(master) +1):nrow(main)],
                                                  opp_weight_SB[(nrow(main) - nrow(master) +1):nrow(main)],
                                                  opp_weight_CS[(nrow(main) - nrow(master) +1):nrow(main)]
                                                  ))
sb_preds = sapply(data.frame(sb_preds), sum)
sb_preds = sapply(data.frame(sb_preds), exp)

dkpt_preds = data.frame(3*single_preds + 5*double_preds + 8*triple_preds + 10*hr_preds + 2*rbi_preds + 
                    2*r_preds + 2*bb_preds + 2*hbp_preds + 5*sb_preds)
dkpt_preds = unlist(dkpt_preds)
final_preds = data.frame(master$Batter, dkpt_preds)
colnames(final_preds) = c("Name", "Prediction")
View(final_preds[order(final_preds$Prediction, decreasing = T),])

# SAVE MAIN SET WITH PREDICTIONS
main$my_preds[(nrow(main) - nrow(master) + 1):nrow(main)] = dkpt_preds
write_csv(main, 'draftkings_mlb_bat.csv')

# DRAFTKINGS DASHBOARD
prices = read_csv("~/Downloads/DKSalaries.csv")
prices = prices %>% dplyr::select(c(Position, Name, Salary, TeamAbbrev, AvgPointsPerGame, `Roster Position`))

dashboard = left_join(final_preds, prices, by = "Name")
dashboard = dashboard[!is.na(dashboard$Position),]
dashboard$Prediction = ifelse(dashboard$`Roster Position` == "CPT", dashboard$Prediction*1.5,
                              dashboard$Prediction)
dashboard$`Dollars per Point` = dashboard$Salary/dashboard$Prediction
dashboard$`Prediction Difference` = dashboard$Prediction - dashboard$AvgPointsPerGame
dashboard = dashboard[order(dashboard$`Dollars per Point`),]
rownames(dashboard) = 1:nrow(dashboard)
View(dashboard)


#### MODEL DEVELOPMENT ####

# Singles
single_mod = "model{

  # Likelihood
  for(i in 1:n){
    single[i] ~ dpois(lambda[i])
    
    log(lambda[i]) <- beta[1] + beta[2]*rating_diff[i] + beta[3]*RGS[i] + beta[4]*weight_PA[i] + 
    beta[5]*weight_BA[i] + beta[6]*weight_SLG[i] + beta[7]*weight_SO_pct[i] +
    beta[8]*opp_weight_BA[i] + beta[9]*opp_weight_SO_pct[i]
  }

  # Priors
  for(j in 1:9){
    beta[j] ~ dnorm(0, 0.0001)
  }

}"

vars = list(single = single, rating_diff = rating_diff, RGS = RGS, weight_PA = weight_PA,
            weight_BA = weight_BA, weight_SLG = weight_SLG, weight_SO_pct = weight_SO_pct,
            opp_weight_BA = opp_weight_BA, opp_weight_SO_pct = opp_weight_SO_pct, n = n)

jags_single = jags.model(textConnection(single_mod), data = vars, n.chains = 1, n.adapt = 100)

mcmc_out_single = coda.samples(jags_single, c("beta[1]", "beta[2]", "beta[3]", "beta[4]", "beta[5]",
                                              "beta[6]", "beta[7]", "beta[8]", "beta[9]"), 
                               n.iter = 1000, thin = 10, n.burnin = 100)

single_summary = summary(mcmc_out_single)

# Doubles
double_mod = "model{

  # Likelihood
  for(i in 1:n){
    double[i] ~ dpois(lambda[i])
    
    log(lambda[i]) <- beta[1] + beta[2]*weight_PA[i] + beta[3]*weight_BA[i] + beta[4]*weight_2B[i] + 
    beta[5]*opp_weight_2B[i] + beta[6]*opp_weight_SLG[i]
  }

  # Priors
  for(j in 1:6){
    beta[j] ~ dnorm(0, 0.0001)
  }

}"

vars = list(double = double, weight_PA = weight_PA, weight_BA = weight_BA, weight_2B = weight_2B, 
            opp_weight_2B = opp_weight_2B, opp_weight_SLG = opp_weight_SLG, n = n)

jags_double = jags.model(textConnection(double_mod), data = vars, n.chains = 1, n.adapt = 100)

mcmc_out_double = coda.samples(jags_double, c("beta[1]", "beta[2]", "beta[3]", "beta[4]", "beta[5]",
                                              "beta[6]"), 
                               n.iter = 1000, thin = 10, n.burnin = 100)

double_summary = summary(mcmc_out_double)

# Triples
triple_mod = "model{

  # Likelihood
  for(i in 1:n){
    triple[i] ~ dpois(lambda[i])
    
    log(lambda[i]) <- beta[1] + beta[2]*weight_PA[i] + beta[3]*weight_SB[i] + beta[4]*weight_3B[i] + 
    beta[5]*opp_weight_3B[i] + beta[6]*opp_weight_SLG[i]
  }

  # Priors
  for(j in 1:6){
    beta[j] ~ dnorm(0, 0.0001)
  }

}"

vars = list(triple = triple, weight_PA = weight_PA, weight_SB = weight_SB, weight_3B = weight_3B, 
            opp_weight_3B = opp_weight_3B, opp_weight_SLG = opp_weight_SLG, n = n)

jags_triple = jags.model(textConnection(triple_mod), data = vars, n.chains = 1, n.adapt = 100)

mcmc_out_triple = coda.samples(jags_triple, c("beta[1]", "beta[2]", "beta[3]", "beta[4]", "beta[5]",
                                              "beta[6]"), 
                               n.iter = 1000, thin = 10, n.burnin = 100)

triple_summary = summary(mcmc_out_triple)

# HR
hr_mod = "model{

  # Likelihood
  for(i in 1:n){
    HR[i] ~ dpois(lambda[i])
    
    log(lambda[i]) = beta[1] + beta[2]*rating_diff[i] + beta[3]*weight_PA[i] + beta[4]*weight_SLG[i] +
    beta[5]*weight_HR_pct[i] + beta[6]*opp_weight_HR_pct[i]
  }

  # Priors
  for(j in 1:6){
    beta[j] ~ dnorm(0, 0.0001)
  }

}"

vars = list(HR = HR, rating_diff = rating_diff, weight_PA = weight_PA, weight_SLG = weight_SLG,
            weight_HR_pct = weight_HR_pct, opp_weight_HR_pct = opp_weight_HR_pct, n = n)

jags_hr = jags.model(textConnection(hr_mod), data = vars, n.chains = 1, n.adapt = 100)

mcmc_out_hr = coda.samples(jags_hr, c("beta[1]", "beta[2]", "beta[3]", "beta[4]", "beta[5]",
                                      "beta[6]"), 
                           n.iter = 1000, thin = 10, n.burnin = 100)

hr_summary = summary(mcmc_out_hr)

# RBI
rbi_mod = "model{

  # Likelihood
  for(i in 1:n){
    RBI[i] ~ dpois(lambda[i])
    
    log(lambda[i]) = beta[1] + beta[2]*rating_diff[i] + beta[3]*weight_PA[i] + beta[4]*weight_RBI[i] + 
    beta[5]*weight_SLG[i] + beta[6]*opp_weight_SLG[i] + beta[7]*opp_weight_SO_pct[i] +
    beta[8]*opp_weight_BB_pct[i]
  }

  # Priors
  for(j in 1:8){
    beta[j] ~ dnorm(0, 0.0001)
  }

}"

vars = list(RBI = RBI, rating_diff = rating_diff, weight_PA = weight_PA, weight_RBI = weight_RBI,
            weight_SLG = weight_SLG, opp_weight_SLG = opp_weight_SLG, opp_weight_SO_pct = opp_weight_SO_pct, 
            opp_weight_BB_pct = opp_weight_BB_pct, n = n)

jags_rbi = jags.model(textConnection(rbi_mod), data = vars, n.chains = 1, n.adapt = 100)

mcmc_out_rbi = coda.samples(jags_rbi, c("beta[1]", "beta[2]", "beta[3]", "beta[4]", "beta[5]",
                                        "beta[6]", "beta[7]", "beta[8]"), 
                            n.iter = 1000, thin = 10, n.burnin = 100)

rbi_summary = summary(mcmc_out_rbi)

# R
r_mod = "model{

  # Likelihood
  for(i in 1:n){
    R[i] ~ dpois(lambda[i])
    
    log(lambda[i]) = beta[1] + beta[2]*rating_diff[i] + beta[3]*weight_PA[i] + beta[4]*weight_SLG[i] + 
    beta[5]*opp_weight_SLG[i] + beta[6]*opp_weight_BB_pct[i]
  }

  # Priors
  for(j in 1:6){
    beta[j] ~ dnorm(0, 0.0001)
  }

}"

vars = list(R = R, rating_diff = rating_diff, weight_PA = weight_PA, weight_SLG = weight_SLG,
            opp_weight_SLG = opp_weight_SLG, opp_weight_BB_pct = opp_weight_BB_pct, n = n)

jags_r = jags.model(textConnection(r_mod), data = vars, n.chains = 1, n.adapt = 100)

mcmc_out_r = coda.samples(jags_r, c("beta[1]", "beta[2]", "beta[3]", "beta[4]", "beta[5]",
                                    "beta[6]"), 
                          n.iter = 1000, thin = 10, n.burnin = 100)

r_summary = summary(mcmc_out_r)

# BB
bb_mod = "model{

  # Likelihood
  for(i in 1:n){
    BB[i] ~ dpois(lambda[i])
    
    log(lambda[i]) = beta[1] + beta[2]*rating_diff[i] + beta[3]*weight_PA[i] + beta[4]*weight_OBP[i] + 
    beta[5]*weight_BB_pct[i] + beta[6]*opp_weight_BB_pct[i] + beta[7]*opp_weight_Str_pct[i]
  }

  # Priors
  for(j in 1:7){
    beta[j] ~ dnorm(0, 0.0001)
  }

}"

vars = list(BB = BB, rating_diff = rating_diff, weight_PA = weight_PA, weight_OBP = weight_OBP,
            weight_BB_pct = weight_BB_pct, opp_weight_BB_pct = opp_weight_BB_pct,
            opp_weight_Str_pct = opp_weight_Str_pct, n = n)

jags_bb = jags.model(textConnection(bb_mod), data = vars, n.chains = 1, n.adapt = 100)

mcmc_out_bb = coda.samples(jags_bb, c("beta[1]", "beta[2]", "beta[3]", "beta[4]", "beta[5]",
                                      "beta[6]", "beta[7]"), 
                           n.iter = 1000, thin = 10, n.burnin = 100)

bb_summary = summary(mcmc_out_bb)

# HBP
hbp_mod = "model{

  # Likelihood
  for(i in 1:n){
    HBP[i] ~ dpois(lambda[i])
    
    log(lambda[i]) = beta[1] + beta[2]*rating_diff[i] + beta[3]*weight_PA[i] + beta[4]*weight_HBP[i] +
    beta[5]*opp_weight_HBP[i] + beta[6]*opp_weight_Str_pct[i]
  }

  # Priors
  for(j in 1:6){
    beta[j] ~ dnorm(0, 0.0001)
  }

}"

vars = list(HBP = HBP, rating_diff = rating_diff, weight_PA = weight_PA, weight_HBP = weight_HBP, 
            opp_weight_HBP = opp_weight_HBP, opp_weight_Str_pct = opp_weight_Str_pct, n = n)

jags_hbp = jags.model(textConnection(hbp_mod), data = vars, n.chains = 1, n.adapt = 100)

mcmc_out_hbp = coda.samples(jags_hbp, c("beta[1]", "beta[2]", "beta[3]", "beta[4]", "beta[5]",
                                        "beta[6]"), n.iter = 1000, thin = 10, n.burnin = 100)

hbp_summary = summary(mcmc_out_hbp)

# SB
sb_mod = "model{

  # Likelihood
  for(i in 1:n){
    SB[i] ~ dpois(lambda[i])
    
    log(lambda[i]) = beta[1] + beta[2]*weight_PA[i] + beta[3]*weight_SB[i] + beta[4]*weight_CS[i] +
    beta[5]*opp_weight_SB[i] + beta[6]*opp_weight_CS[i]
  }

  # Priors
  for(j in 1:6){
    beta[j] ~ dnorm(0, 0.0001)
  }

}"

vars = list(SB = SB, weight_PA = weight_PA, weight_SB = weight_SB, weight_CS = weight_CS, 
            opp_weight_SB = opp_weight_SB, opp_weight_CS = opp_weight_CS, n = n)

jags_sb = jags.model(textConnection(sb_mod), data = vars, n.chains = 1, n.adapt = 100)

mcmc_out_sb = coda.samples(jags_sb, c("beta[1]", "beta[2]", "beta[3]", "beta[4]", "beta[5]",
                                      "beta[6]"), 
                           n.iter = 1000, thin = 10, n.burnin = 100)

sb_summary = summary(mcmc_out_sb)


single = main$H - main$`2B` - main$`3B` - main$HR
summary(glm((H - `2B` - `3B` - HR) ~ rating_diff + RGS + weight_PA + weight_BA + weight_SLG +
              weight_SO_pct + opp_weight_BA + opp_weight_SO_pct,
            data = main, family = poisson(link = "log")))
summary(glm(`2B` ~ weight_2B + weight_PA + weight_BA +
              opp_weight_2B + opp_weight_SLG,
            data = main, family = poisson(link = "log")))
summary(glm(`3B` ~ weight_3B + weight_PA + weight_SB +
              opp_weight_3B + opp_weight_SLG,
            data = main, family = poisson(link = "log")))
summary(glm(HR ~ rating_diff + weight_PA + weight_SLG + weight_HR_pct +
              opp_weight_HR_pct,
            data = main, family = poisson(link = "log")))
summary(glm(RBI ~ rating_diff + weight_PA + weight_RBI + weight_SLG +
              opp_weight_SLG + opp_weight_SO_pct + 
              opp_weight_BB_pct, data = main, family = poisson(link = "log")))
summary(glm(R ~ rating_diff + weight_PA + weight_SLG +
         opp_weight_SLG + opp_weight_BB_pct,
        data = main, family = poisson(link = "log")))
summary(glm(BB ~ rating_diff + weight_PA + weight_OBP +
              weight_BB_pct +
              opp_weight_BB_pct + opp_weight_Str_pct, data = main,
            family = poisson(link = "log")))
summary(glm(HBP ~ rating_diff + weight_PA + weight_HBP +
              opp_weight_HBP + opp_weight_Str_pct, data = main,
            family = poisson(link = "log")))
summary(glm(SB ~ weight_PA + weight_SB + weight_CS +
              opp_weight_SB + opp_weight_CS, data = main, 
            family = poisson(link = "log")))


