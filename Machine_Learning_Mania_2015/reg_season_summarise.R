library(dplyr)
library(sqldf)
library(stringr)

setwd("/home/john/Kaggle/Machine Learning Mania 2015/kaggle data")

reg_season <- read.csv("regular_season_detailed_results.csv",
                       header = TRUE,
                       stringsAsFactors = FALSE)
reg_season <- tbl_df(reg_season)

head(reg_season)
colnames(reg_season)

## need to summarize possessions for the winning and losing teams, then find a 
## way to neatly aggregate those over a span of a few months (Jan-Feb) then
## train on March data. 

## first summarise possessions.
## I thought of three statistics I'd like to investigate
## 1) three point field goal attempts as a percentage of total shot attempts
## 2) turn overs as a percentage of total possessions
## 3) rebound percentage
## 4) free throw attempts per possession
## 5) turnovers forced as percentage of defensive plays played
## 6) personal fouls

reg_season <- reg_season %>%
  mutate(wteam_3pa_pct       = wfga3/wfga,
         lteam_3pa_pct       = lfga3/lfga,
         wteam_to_pct        = wto/(wfga+wto),
         lteam_to_pct        = lto/(lfga+lto),
         wteam_reb_pct       = (wor+wdr)/(wor+wdr+lor+ldr),
         lteam_reb_pct       = (lor+ldr)/(wor+wdr+lor+ldr),
         wteam_ft_pct        = wfta/(wfga + wto),
         lteam_ft_pct        = lfta/(lfga + lto),
         wteam_to_forced_pct = lto/(lfga+lto), 
         lteam_to_forced_pct = wto/(wfga+wto)) %>%
  select(season,daynum,wteam,lteam,wteam_3pa_pct,lteam_3pa_pct,wteam_to_pct,
         lteam_to_pct,wteam_reb_pct,lteam_reb_pct,wteam_ft_pct,lteam_ft_pct,
         wteam_to_forced_pct,lteam_to_forced_pct,wpf,lpf)

reg_season
