setwd("C:/Users/remil/Desktop/M2/shiny/app_lol/LoLProject/")

source('function_load.R')
source('package_load.R')

# Importer df_stat + modification du dataframe

setwd("C:/Users/remil/Desktop/M2/shiny/app_lol/LoLProject/new_data/")

df_stat <- read.csv("df_stat.csv", fileEncoding = "UTF-8")
df_stat$ligue <- as.factor(df_stat$ligue)

# Fusionner df_banrate et df_pickrate

df_global <- read.csv("df_global.csv",  fileEncoding = "UTF-8")


# Importation clean_data

df_summoners <- read.csv("clean_data.csv", fileEncoding = "UTF-8")
df_summoners$saison <- as.factor(df_summoners$saison)
# Importation de data_role



# Modification df_stat et df_summoners:



# Importation df_bio_clean

df_bio <- read.csv("clean_data_bio.csv", fileEncoding = "UTF-8")





