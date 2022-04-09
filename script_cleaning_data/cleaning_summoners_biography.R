library(dplyr)
library(tidyverse)
library(readxl)

setwd("C:/Users/remil/Desktop/M2/shiny/app_lol/LoLProject/row_data/")

data <- read.csv(file = "shibasa.csv", fileEncoding = "UTF-8")

role <- read_excel("role_perso_lol.xlsx")




# Cleaning data

str(data)

## Saison

data$saison <- str_replace_all(data$saison, '\\D', "")
data <- data %>% 
  mutate(saison = replace(saison, saison == 2020, 10)) %>% 
  mutate(saison = replace(saison, saison == 2021, 11)) %>% 
  mutate(saison = replace(saison, saison == 2022, 12))

## Tot_win, tot_lose et création colonne winrate

data$tot_win <- str_replace_all(data$tot_win, '\\D', "")
data$tot_lose <- str_replace_all(data$tot_lose, '\\D', "")

data$tot_win <- as.numeric(data$tot_win)
data$tot_lose <- as.numeric(data$tot_lose)


data <- data %>% 
  mutate(winrate = tot_win / (tot_win + tot_lose) * 100)

data$winrate <- as.integer(data$winrate)


## avg stats

data$avg_kills <- str_replace_all(data$avg_kills, "/", "")
data$avg_deaths <- str_replace_all(data$avg_deaths, "/", "")
data$avg_assists <- str_replace_all(data$avg_assists, "/", "")


data$avg_kills <- as.numeric(data$avg_kills)
data$avg_deaths <- as.numeric(data$avg_deaths)
data$avg_assists <- as.numeric(data$avg_assists)


## kill mob

data$nb_kill_mobs <- str_replace_all(data$nb_kill_mobs, "\\(.*", "")

str(data)


data$gold <- str_replace_all(data$gold, ",", "")
data$nb_kill_mobs <- str_replace_all(data$nb_kill_mobs, ",", "")
data$avg_dmg_dealt <- str_replace_all(data$avg_dmg_dealt, ",", "")
data$avg_dmg_taken <- str_replace_all(data$avg_dmg_taken, ",", "")


data$gold <- as.numeric(data$gold)
data$nb_kill_mobs <- as.numeric(data$nb_kill_mobs)
data$avg_dmg_dealt <- as.numeric(data$avg_dmg_dealt)
data$avg_dmg_taken <- as.numeric(data$avg_dmg_taken)


unlist(lapply(data, function(x) any(is.na(x))))
data <- data %>% replace(is.na(.), 0)

data$saison <- as.factor(data$saison)
data$nb_games <- data$tot_lose + data$tot_win
data$champion <- as.factor(data$champion)
data$kd_rate <- (data$avg_kills + data$avg_assists)/data$avg_deaths

#valeur Inf
data <- data %>% 
  filter(kd_rate != "Inf")

data <- data %>% 
  inner_join(role, by = "champion")
data$role <- as.factor(data$role)

write.csv(data, "C:/Users/remil/Desktop/M2/shiny/app_lol/LoLProject/new_data/clean_data.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Cleaning df_bio

df_bio <- read_csv("bio_champ.csv")

df_bio$race <- str_replace_all(df_bio$race, 'RACE', "")
df_bio$type <- str_replace_all(df_bio$type, 'RÔLE', "")
df_bio$region <- str_replace_all(df_bio$region, 'RÉGION', "")
df_bio <- df_bio[, -1]

df_bio$race <- as.factor(df_bio$race)
df_bio$type <- as.factor(df_bio$type)
df_bio$region <- as.factor(df_bio$region)

write.csv(df_bio, "C:/Users/remil/Desktop/M2/shiny/app_lol/LoLProject/new_data/clean_data_bio.csv", row.names = FALSE, fileEncoding = "UTF-8")