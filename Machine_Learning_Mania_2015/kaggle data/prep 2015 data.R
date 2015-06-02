library(dplyr)
library(stringr)
library(sqldf)

setwd("/home/john/Kaggle/Machine Learning Mania 2015/my data")
bpi <- read.csv("data_2015_16MAR2015.csv")
head(bpi)

bpi$team_name <- tolower(bpi$team) 
bpi$team_name <- sapply(bpi$team_name,str_replace,"state","st")
bpi$team_name <- sapply(bpi$team_name,str_replace,"saint","st.")

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
  mutate(team_name = ifelse(team_name == "milwaukee", "wi milwaukee", team_name)) %>%
  mutate(team_name = ifelse(team_name == "nc st", "nc state", team_name)) %>%
  mutate(team_name = ifelse(team_name == "st. john's", "st john's", team_name))

setwd("/home/john/Kaggle/Machine Learning Mania 2015/kaggle data")
teams <- read.csv("teams.csv",header=TRUE,stringsAsFactors=FALSE)

teams$team_name2 <- tolower(teams$team_name)


seeds <- read.csv("tourney_seeds_2015_Monday.csv",header=TRUE,stringsAsFactors=FALSE)
seeds$seed2 <- str_sub(seeds$seed,2,3)

seeds_team <- sqldf('
                    select
                      a.team,
                      a.seed2,
                      b.team_name2
                    from
                      seeds as a
                    left join
                      teams as b
                    on a.team = b.team_id')

### Now bring in BPI data

seeds_team_bpi <- sqldf('
                        select
                          a.*,
                          b.bpi
                        from
                          seeds_team as a
                        left join
                          bpi as b
                        on
                          a.team_name2 = b.team_name')
write.csv(seeds_team_bpi,"tourney_2015.csv")
