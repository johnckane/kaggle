library(sqldf)
library(stringr)

setwd("/home/john/Kaggle/Machine Learning Mania 2015/kaggle data")

data <- read.csv("tourney_2015.csv",header=TRUE,stringsAsFactors = FALSE)
## only need teams and seeds from 2011 - 2014


tourney_matchups <- sqldf('
                          select
                            a.team as team,
                            b.team as opp
                          from
                            data as a,
                            data as b
                          where
                            a.team < b.team
                          ')
tourney_matchups$id <- paste("2015",
                             tourney_matchups$team,
                             tourney_matchups$opp,
                             sep = "_")   
tourney_matchups2 <- sqldf('
                          select
                            a.id,
                            a.team,
                            a.opp,
                            b.bpi as team_bpi
                          from
                            tourney_matchups as a,
                            data as b
                          where
                            a.team = b.team
                           ')
tourney_matchups3 <- sqldf('
                           select
                            a.id,
                            a.team,
                            a.opp,
                            a.team_bpi,
                            b.bpi as opp_bpi
                           from
                            tourney_matchups2 as a,
                            data as b
                           where
                            a.opp = b.team')



tourney_matchups4 <- tourney_matchups3 %>%
  mutate(pred = exp(0.08731827*(team_bpi-opp_bpi))/(1+exp(0.08731827*(team_bpi-opp_bpi)))) %>%
  select(id,pred)

head(tourney_matchups4)

write.csv(tourney_matchups4,"submission1_2015.csv")
