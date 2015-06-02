library(sqldf)
library(dplyr)

setwd("/home/john/Kaggle/Machine Learning Mania 2015/kaggle data")

bpi_tourney <- read.csv("bpi_tourney.csv",header=TRUE,stringsAsFactors=FALSE)
head(bpi_tourney)

bpi_teams <- sqldf('
                   select distinct
                    season,
                    teamid,
                    bpi
                   from
                    (select distinct
                      season,
                      teamid,
                      team_bpi as bpi
                    from
                      bpi_tourney
                    union
                    select distinct
                      season,
                      opp as teamid,
                      opp_bpi as bpi
                    from
                      bpi_tourney)
                   ')
                    

teams <- sqldf('
                    select distinct
                      season,
                      team_id
                    from
                      (select distinct
                        teamid as team_id,
                        season
                       from
                        bpi_tourney
                       union
                       select distinct
                        opp as team_id,
                        season
                       from
                        bpi_tourney)
                    order by
                      1,2
                    ')
matchups <- sqldf('
                  select
                    a.season,
                    a.team_id as teamid,
                    b.team_id as opp
                  from
                    teams as a,
                    teams as b
                  where 
                    a.season = b.season
                  and a.team_id < b.team_id')

matchups$id <- paste(matchups$season,
                     matchups$teamid,
                     matchups$opp,
                     sep = "_")

## bring in bpi info
matchups2 <- sqldf('
                   select 
                    a.id,
                    a.season,
                    a.teamid,
                    a.opp,
                    b.bpi as team_bpi
                   from
                    matchups as a,
                    bpi_teams as b
                   where
                    a.teamid = b.teamid
                   and a.season = b.season')
matchups3 <- sqldf('
                   select
                    a.*,
                    b.bpi as opp_bpi
                   from
                    matchups2 as a,
                    bpi_teams as b
                   where
                    a.opp = b.teamid
                   and a.season = b.season')
matchups4 <- matchups3 %>%
  mutate(pred = exp(0.08731827*(team_bpi-opp_bpi))/(1+exp(0.08731827*(team_bpi-opp_bpi)))) %>%
  select(id,pred)

###################################
## Pull in 2011 teams
###################################

tourney_results <- read.csv("tourney_compact_results.csv",header=TRUE,stringsAsFactors=FALSE)
head(tourney_results)

for(i in 1:dim(tourney_results)[1]){
  tourney_results$gameid[i] <- paste(tourney_results$season[i],
                                     min(tourney_results$wteam[i],tourney_results$lteam[i]),
                                     max(tourney_results$wteam[i],tourney_results$lteam[i]),
                                     sep="_")
  if(tourney_results$wteam[i] < tourney_results$lteam[i]){
    tourney_results$teamid[i] <- tourney_results$wteam[i]
    tourney_results$opp[i] <- tourney_results$lteam[i]
    tourney_results$win[i] <- 1
  }
  else{
    tourney_results$teamid[i] <- tourney_results$lteam[i]
    tourney_results$opp[i] <- tourney_results$wteam[i]
    tourney_results$win[i] <- 0
  }
}

head(tourney_results)

tourney_2011 <- select(tourney_results,season,gameid,teamid,opp,win) %>%
  filter(season == 2011) %>%
  select(season,teamid,opp)
teams_2011 <- teams <- sqldf('
                    select distinct
                      season,
                      team_id
                    from
                      (select distinct
                        teamid as team_id,
                        season
                       from
                        tourney_2011
                       union
                       select distinct
                        opp as team_id,
                        season
                       from
                        tourney_2011)
                    order by
                      1,2
                    ')
matchups_2011 <- sqldf('
                  select
                    a.season,
                    a.team_id as teamid,
                    b.team_id as opp,
                    0.5 as pred
                  from
                    teams as a,
                    teams as b
                  where 
                    a.season = b.season
                  and a.team_id < b.team_id')
matchups_2011$id <- paste(matchups_2011$season,
                          matchups_2011$teamid,
                          matchups_2011$opp,
                          sep = '_')
matchups_2011 <- matchups_2011 %>%
  select(id,pred)

##################################
## Union the two files together ##
##################################

submission1 <- sqldf('
                     select
                      *
                     from 
                      matchups_2011
                     union
                     select
                      *
                     from
                      matchups4')

write.csv(submission1,"bpi_submission_historical.csv")
