# This is identical to submission 2, except we're going to use only 1985 - 2010 data, so we're not using
# data from the prediction period to make predictions about that period

library(sqldf)
library(stringr)

setwd("/home/john/Kaggle/Machine Learning Mania 2015/kaggle data")
tourney_results <- read.csv("tourney_compact_results.csv",header=TRUE,stringsAsFactors=FALSE)
tourney_seeds <- read.csv("tourney_seeds.csv",header=TRUE,stringsAsFactors=FALSE)

## only need teams and seeds from 2011 - 2014

tourney_results_11_14 <- filter(tourney_results,season >= 2011)
tourney_seeds_11_14 <- filter(tourney_seeds,season >= 2011)
tourney_seeds_11_14$seed2 <- str_sub(tourney_seeds_11_14$seed,2,3)

tourney_teams <- sqldf('
                       select distinct
                       season,
                       team
                       from
                       (select distinct
                       season,
                       wteam as team
                       from
                       tourney_results_11_14
                       union
                       select distinct
                       season,
                       lteam as team
                       from
                       tourney_results_11_14)
                       order by
                       season,
                       team
                       ')

tourney_matchups <- sqldf('
                          select
                          a.season,
                          a.team as team,
                          b.team as opp
                          from
                          tourney_teams as a,
                          tourney_teams as b
                          where
                          a.season = b.season
                          and a.team < b.team
                          ')
tourney_matchups$id <- paste(tourney_matchups$season,
                             tourney_matchups$team,
                             tourney_matchups$opp,
                             sep = "_")   
tourney_matchups2 <- sqldf('
                           select
                           a.id,
                           a.season,
                           a.team,
                           a.opp,
                           b.seed2 as team_seed
                           from
                           tourney_matchups as a,
                           tourney_seeds_11_14 as b
                           where
                           a.season = b.season
                           and a.team = b.team
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
                           tourney_seeds_11_14 as b
                           where
                           a.season = b.season
                           and a.opp = b.team')

seed_probs <- read.csv("empirical_seed_probs_1985_2010.csv",header=TRUE,stringsAsFactors = FALSE)
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
write.csv(tourney_pred,"submission3.csv")
