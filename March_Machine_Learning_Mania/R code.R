setwd("/home/john86/Kaggle/March Machine Learning Mania")
regular_season_results <- read.csv("regular_season_results.csv")
seasons <- read.csv("seasons.csv")
teams <- read.csv("teams.csv")
tourney_results <- read.csv("tourney_results.csv")
tourney_seeds <- read.csv("tourney_seeds.csv")
tourney_slots <- read.csv("tourney_slots.csv")

sqldf("select * from regular_season_results where wteam=853 and season = 'M' ")

tourney_slots[c(1:63),]
tourney_seeds[c(1:63),]
tourney_results[c(1:63),]
