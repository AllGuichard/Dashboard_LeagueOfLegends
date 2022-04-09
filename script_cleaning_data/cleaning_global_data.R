


library(tidyverse)
library(dplyr)
library(data.table)
library(readr)
library(readxl)
library(here)


# Importation des dataframe

## Import



setwd("C:/Users/remil/Desktop/M2/shiny/app_lol/LoLProject/row_data/")





df_iron <- read.csv("stat_iron.csv", 
                         header = TRUE, 
                         sep = ",", fileEncoding = "utf-8")

df_bronze <- read.csv("stat_bronze.csv", 
                         header = TRUE, 
                         sep = ",", fileEncoding = "utf-8")

df_silver <- read.csv("stat_silver.csv", 
                         header = TRUE, 
                         sep = ",", fileEncoding = "utf-8")

df_gold <- read.csv("stat_gold.csv", 
                         header = TRUE, 
                         sep = ",", fileEncoding = "utf-8")

df_platinium <- read.csv("stat_platinium.csv", 
                         header = TRUE, 
                         sep = ",", fileEncoding = "utf-8")

df_diamond <- read.csv("stat_diamond.csv", 
                         header = TRUE, 
                         sep = ",", fileEncoding = "utf-8")

df_master <- read.csv("stat_master.csv", 
                         header = TRUE, 
                         sep = ",", fileEncoding = "utf-8")

df_grandmaster <- read.csv("stat_grandmaster.csv", 
                         header = TRUE, 
                         sep = ",", fileEncoding = "utf-8")

df_challenger <- read.csv("stat_challenger.csv", 
                         header = TRUE, 
                         sep = ",", fileEncoding = "utf-8")

df_pickrate <- read.csv("stat_pickrate.csv", 
                         header = TRUE, 
                         sep = ",", fileEncoding = "utf-8")

df_banrate <- read.csv("stat_banrate.csv", 
                         header = TRUE, 
                         sep = ",", fileEncoding = "utf-8")

df_role <- read_excel("role_perso_lol.xlsx")





## Création de `df_stat`


df_iron$ligue <- "Iron"
df_bronze$ligue <- "Bronze"
df_silver$ligue <- "Silver"
df_gold$ligue <- "Gold"
df_platinium$ligue <- "Platinium"
df_diamond$ligue <- "Diamond"
df_master$ligue <- "Master"
df_grandmaster$ligue <- "Grandmaster"
df_challenger$ligue <- "Challenger"

df_stat <- rbind(df_iron, df_bronze, df_silver, df_gold, df_platinium, df_diamond, df_master, df_grandmaster, df_challenger)


 

# Correction des données 

## `df_stat`

df_stat$winrate <- gsub(".{1}$", "", df_stat$winrate)
df_stat$winrate <- as.numeric(df_stat$winrate)

df_stat$ligue <- as.factor(df_stat$ligue)
df_stat$ligue <- factor(df_stat$ligue,
                        levels=c("Iron", "Bronze", "Silver", "Gold", "Platinium", "Diamond", "Master", "Grandmaster", "Challenger" ))
df_stat <- df_stat %>% 
  inner_join(df_role, by = "champion")
df_stat$role <- as.factor(df_stat$role)

df_stat <- df_stat[, c(-1:-2)]



## Correction de la base de données `df_pickrate`


df_pickrate <- df_pickrate[, c(-1,-2)]
df_pickrate$winrate <- gsub(".{1}$", "", df_pickrate$winrate)
df_pickrate$winrate <- as.numeric(df_pickrate$winrate)

df_pickrate$pickrate <- df_pickrate$winrate

df_pickrate <- df_pickrate[, -2]



## Correction de la base de données `df_banrate`


df_banrate <- df_banrate[, -1]

df_banrate$banrate <- gsub(".{1}$", "", df_banrate$banrate)
df_banrate$banrate <- as.numeric(df_banrate$banrate)



# Création de la base de données `df_global`


df_global <- df_banrate[, -1]

df_global <- df_global %>% 
  left_join(df_pickrate, by = c("champion" = "champion"))



# Exportation des dataframe en csv pour le shiny



write.csv(df_stat, "C:/Users/remil/Desktop/M2/shiny/app_lol/LoLProject/new_data/df_stat.csv", row.names = FALSE, fileEncoding = "UTF-8")

write.csv(df_global, "C:/Users/remil/Desktop/M2/shiny/app_lol/LoLProject/new_data/df_global.csv", row.names = FALSE, fileEncoding = "UTF-8")







