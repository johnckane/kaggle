## make sure submission2 and submission3 are different
s2 <- read.csv("submission2.csv",header=TRUE,stringsAsFactors=FALSE)
s3 <- read.csv("submission3.csv",header=TRUE,stringsAsFactors=FALSE)

sqldf('
      select
        a.id
      from
        s2 as a,
        s3 as b
      where
        a.id = b.id
      and a.pred != b.pred')

seed_probs2 <- read.csv("empirical_seed_probs_1985_2010.csv",header=TRUE,stringsAsFactors = FALSE)
seed_probs3 <- read.csv("empirical_seed_probs.csv",header=TRUE,stringsAsFactors = FALSE)


sqldf('
      select
        a.team,
        a.opp,
        a.wprob,
        b.wprob
      from
        seed_probs2 as a,
        seed_probs3 as b
      where
        a.team = b.team
      and a.opp = b.opp
      and a.wprob != b.wprob')
