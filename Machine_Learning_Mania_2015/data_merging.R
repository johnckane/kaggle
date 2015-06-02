library(dplyr)
library(sqldf)
library(stringr)

setwd("/home/john/Kaggle/Machine Learning Mania 2015/kaggle data")




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

tourney_results <- select(tourney_results,season,gameid,teamid,opp,win) %>%
  filter(season >= 2012)

teams_in_use <- c(tourney_results$teamid,tourney_results$opp)
teams_in_use <- data.frame(unique(teams_in_use))
colnames(teams_in_use) = "team"
teams_in_use$team <- tolower(teams_in_use$team)

########## Bring in team and BPI data ##########3
teams <- read.csv("teams.csv",header=TRUE,stringsAsFactors=FALSE)
teams_to_use <- sqldf('
                      select
                        a.*
                      from
                        teams as a,
                        teams_in_use as b
                      where
                        a.team_id = b.team')
teams_to_use$team_name <- tolower(teams_to_use$team_name)
teams_to_use$team_name <- sapply(teams_to_use$team_name,str_replace,"state","st")

bpi_12 <- read.csv("/home/john/Kaggle/Machine Learning Mania 2015/my data/data_2012.csv",
                   header=TRUE,
                   stringsAsFactors=FALSE)
bpi_13 <- read.csv("/home/john/Kaggle/Machine Learning Mania 2015/my data/data_2013.csv",
                   header=TRUE,
                   stringsAsFactors=FALSE)
bpi_14 <- read.csv("/home/john/Kaggle/Machine Learning Mania 2015/my data/data_2014.csv",
                   header=TRUE,
                   stringsAsFactors=FALSE)

bpi_12$year = 2012
bpi_13$year = 2013
bpi_14$year = 2014

bpi <- rbind(bpi_12,bpi_13,bpi_14)
head(bpi)

bpi$team_name <- tolower(bpi$team) 
bpi$team_name <- sapply(bpi$team_name,str_replace,"state","st")
bpi$team_name <- sapply(bpi$team_name,str_replace,"saint","st.")

bpi_teams <- sqldf('
                   select
                    a.team_id,
                    b.*
                   from
                    teams_to_use as a
                   left join
                    bpi as b
                   on
                    a.team_name = b.team_name')

missing_teams <- c(bpi_teams[c(is.na(bpi_teams$team)),]$team_id)


filter(teams_to_use,team_id %in% missing_teams)

bpi <- bpi[order(bpi$team_name),]


bpi <- bpi %>%
  mutate(team_name = ifelse(team_name == "albany","albany ny",team_name)) %>%
  mutate(team_name = ifelse(team_name == "american u","american univ",team_name)) %>%
  mutate(team_name = ifelse(team_name == "cal poly","cal poly slo",team_name)) %>%
  mutate(team_name = ifelse(team_name == "coast carolina","coastal car",team_name)) %>%
  mutate(team_name = ifelse(team_name == "fgcu","fl gulf coast",team_name)) %>%
  mutate(team_name = ifelse(team_name == "g. washington","g washington",team_name)) %>%
  mutate(team_name = ifelse(team_name == "liu brooklyn","long island",team_name)) %>%
  mutate(team_name = ifelse(team_name == "loyola (md)","loyola md",team_name)) %>%
  mutate(team_name = ifelse(team_name == "miami (fl)","miami fl", team_name)) %>%
  mutate(team_name = ifelse(team_name == "ole miss","mississippi",team_name)) %>%
  mutate(team_name = ifelse(team_name == "miss valley st","ms valley st", team_name)) %>%
  mutate(team_name = ifelse(team_name == "mount st mary's","mt st mary's",team_name)) %>%
  mutate(team_name = ifelse(team_name == "middle tennessee", "mtsu", team_name)) %>%
  mutate(team_name = ifelse(team_name == "north dakota st", "n dakota st", team_name)) %>%
  mutate(team_name = ifelse(team_name == "n carolina a&t", "nc a&t", team_name)) %>%
  mutate(team_name = ifelse(team_name == "n carolina cent", "nc central", team_name)) %>%
  mutate(team_name = ifelse(team_name == "northwestern st", "northwestern la", team_name)) %>%
  mutate(team_name = ifelse(team_name == "south dakota st", "s dakota st", team_name)) %>%
  mutate(team_name = ifelse(team_name == "southern", "southern univ", team_name)) %>%
  mutate(team_name = ifelse(team_name == "st. bonaventure", "st bonaventure", team_name)) %>%
  mutate(team_name = ifelse(team_name == "st. joseph's","st joseph's pa", team_name)) %>%
  mutate(team_name = ifelse(team_name == "st. louis", "st louis", team_name)) %>%
  mutate(team_name = ifelse(team_name == "st. mary's","st mary's ca", team_name)) %>%
  mutate(team_name = ifelse(team_name == "texas southern", "tx southern", team_name)) %>%
  mutate(team_name = ifelse(team_name == "la-lafayette", "ull", team_name)) %>%
  mutate(team_name = ifelse(team_name == "vcu", "va commonwealth", team_name)) %>%
  mutate(team_name = ifelse(team_name == "milwaukee", "wi milwaukee", team_name))

## now try again
bpi_teams <- sqldf('
                   select
                    a.team_id,
                   b.*
                   from
                   teams_to_use as a
                   left join
                   bpi as b
                   on
                   a.team_name = b.team_name')

missing_teams <- c(bpi_teams[c(is.na(bpi_teams$team_name)),]$team_id)
filter(teams_to_use,team_id %in% missing_teams)

## Finally!

## Now merge BPI with tourney results on team_id variable
bpi_team <- sqldf('
                     select
                      a.*,
                      b.bpi as team_bpi
                     from
                      tourney_results as a,
                      bpi_teams as b
                     where
                      a.teamid = b.team_id
                     and a.season = b.year')
bpi_opp <- sqldf('
                 select
                  a.*,
                  b.bpi as opp_bpi,
                  a.team_bpi - b.bpi as bpi_diff
                 from
                  bpi_team as a,
                  bpi_teams as b
                 where
                  a.opp = b.team_id
                 and a.season = b.year')

write.csv(bpi_opp,"bpi_tourney.csv")

## quick and dirty logit model
dirty.fit <- glm(win~team_bpi+bpi_diff,data=bpi_opp)
summary(dirty.fit)

dirty.fit2 <- glm(win~bpi_diff,data=bpi_opp,family="binomial")
summary(dirty.fit2)

str(dirty.fit2)
dirty.fit2$fitted.values
str(dirty.fit2)
n <- dirty.fit2$df.null + 1
log_loss <- -1/n * sum(((dirty.fit2$data$win*log(dirty.fit2$fitted.values)) + 
                      +(1-dirty.fit2$data$win)*log(1-dirty.fit2$fitted.values)))

