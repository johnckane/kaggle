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
                            b.seed2 as team_seed
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
                            a.team_seed,
                            b.seed2 as opp_seed
                           from
                            tourney_matchups2 as a,
                            data as b
                           where
                            a.opp = b.team')

seed_probs <- read.csv("empirical_seed_probs.csv",header=TRUE,stringsAsFactors = FALSE)
tourney_pred <- sqldf('
                      select
                        a.id,
                        b.wprob
                      from
                        tourney_matchups3 as a,
                        seed_probs as b
                      where
                        a.team_seed = b.team
                      and a.opp_seed = b.opp
                      ')
head(tourney_pred)

colnames(tourney_pred) <- c("id","pred")
write.csv(tourney_pred,"submission2_2015.csv")
