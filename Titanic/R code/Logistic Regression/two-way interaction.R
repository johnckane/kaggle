require(ggplot2)

# load training_set
data <- read.csv("/home/john86/Kaggle/Titanic/Data/training_set.csv")

attach(data)

#class/sex
p1 <- ggplot(data=data,aes(x=as.factor(survived),y=sex))
p1 + geom_jitter() + facet_grid(.~pclass)
table(survived,pclass,sex)
g1 <- glm(data=data,as.factor(survived)~as.factor(pclass)+sex+as.factor(pclass)*sex
            ,family=binomial)
summary(g1)
anova(g1,test="Chisquare")

#class/sibsp
p2 <- ggplot(data=data,aes(x=as.factor(survived),y=sibsp))
p2 + geom_jitter() + facet_grid(.~pclass)
table(survived,sibsp,pclass)

for(i in 1:length(data$survived)){
  if(data$sibsp[i] == 1){data$one_sibsp[i] <- 1}
  if(data$sibsp[i] != 1){data$one_sibsp[i] <- 0}
}
table(data$sibsp,data$one_sibsp)

write.csv(data,"/home/john86/Kaggle/Titanic/Data/training_set.csv")

g2 <- glm(data=data,as.factor(survived)~as.factor(pclass)+as.factor(one_sibsp)+
            as.factor(pclass)*as.factor(one_sibsp),family=binomial)
summary(g2)
anova(g2,test="Chisq")

#class/sibsp
p3 <- ggplot(data=data,aes(x=as.factor(survived),y=parch))
p3 + geom_jitter() + facet_grid(.~pclass)
table(survived,parch,pclass)

for(i in 1:length(data$survived)){
  if(data$parch[i] == 1 |data$parch[i] == 2){data$one_two_parch[i] <- 1}
  if(data$parch[i] != 1 & data$parch[i] != 2){data$one_two_parch[i] <- 0}
}

table(data$parch,data$one_two_parch)

g3 <- glm(data=data,as.factor(survived)~as.factor(pclass)+as.factor(one_two_parch)+
            as.factor(pclass)*as.factor(one_two_parch),family=binomial)
summary(g3)
anova(g3,test="Chisq")

# class/fare
p4 <- ggplot(data=data,aes(y=fare,x=pclass,col=as.factor(survived)))
p4 + geom_jitter()
g4 <- glm(data=data,as.factor(survived)~as.factor(pclass) + fare + fare*as.factor(pclass),
          family=binomial)
summary(g4)
anova(g4,test="Chisq")

g4.alt1 <- glm(data=data,as.factor(survived)~as.factor(pclass) + fare,family=binomial)
anova(g4.alt1,test="Chisq")

g4.alt2 <- glm(data=data,as.factor(survived)~fare + as.factor(pclass),family=binomial)
anova(g4.alt2,test="Chisq")

# class/cabin_listed
p5 <- ggplot(data=data,aes(x=as.factor(survived),y=as.factor(cabin_listed)))
p5 + geom_jitter() + facet_grid(.~pclass)
attach(data)
table(survived,pclass,cabin_listed)
g5 <- glm(data=data,as.factor(survived)~as.factor(pclass)+cabin_listed+as.factor(pclass)*cabin_listed
          ,family=binomial)
summary(g5)
anova(g5,test="Chisq")

g5.alt1 <- glm(data=data,as.factor(survived)~as.factor(pclass)+cabin_listed,family=binomial)
g5.alt2 <- glm(data=data,as.factor(survived)~cabin_listed+as.factor(pclass),family=binomial)
anova(g5.alt1,test="Chisq")
anova(g5.alt2,test="Chisq")


# sex/sibsp
p6 <- ggplot(data=data,aes(x=as.factor(survived),y=sibsp))
p6 + geom_jitter() + facet_grid(.~sex)
table(survived,sibsp,sex)

g6 <- glm(data=data,as.factor(survived)~sex+as.factor(sibsp)+
            sex*as.factor(sibsp),family=binomial,y=TRUE)
summary(g6)
anova(g6,test="Chisq")


p6.alt <- ggplot(data=data,aes(x=as.factor(survived),y=one_sibsp))
p6.alt + geom_jitter() +facet_grid(.~sex)
table(survived,one_sibsp,sex)
g6.alt <- glm(data=data,as.factor(survived)~sex+as.factor(one_sibsp)+
                sex*as.factor(one_sibsp),family=binomial)
summary(g6.alt)
anova(g6.alt,test="Chisq")

# sex/parch
p7 <- ggplot(data=data,aes(x=as.factor(survived),y=parch))
p7 + geom_jitter() + facet_grid(.~sex)
table(survived,parch,sex)

g7 <- glm(data=data,as.factor(survived)~sex+as.factor(parch)+
            sex*as.factor(parch),family=binomial,y=TRUE)
summary(g7)
anova(g7,test="Chisq")


p7.alt <- ggplot(data=data,aes(x=as.factor(survived),y=one_two_parch))
p7.alt + geom_jitter() +facet_grid(.~sex)
table(survived,one_two_parch,sex)
g7.alt <- glm(data=data,as.factor(survived)~sex+as.factor(one_two_parch)+
                sex*as.factor(one_two_parch),family=binomial)
summary(g7.alt)
anova(g7.alt,test="Chisq")

# sex/fare
p8 <- ggplot(data=data,aes(y=fare,x=sex,col=as.factor(survived)))
p8 + geom_jitter()
g8 <- glm(data=data,as.factor(survived)~as.factor(sex) + fare + fare*as.factor(sex),
          family=binomial)
summary(g8)
anova(g8,test="Chisq")

# sex/cabin_listed
p9 <- ggplot(data=data,aes(x=as.factor(survived),y=as.factor(cabin_listed)))
p9 + geom_jitter() + facet_grid(.~pclass)

table(survived,sex,cabin_listed)
g9 <- glm(data=data,as.factor(survived)~as.factor(sex)+cabin_listed+as.factor(sex)*cabin_listed
          ,family=binomial)
summary(g9)
anova(g9,test="Chisq")

#sibsp/parch
p10 <- ggplot(data=data,aes(color=as.factor(survived),x=parch, y=sibsp))
p10 + geom_jitter()

table(survived,one_sibsp,one_two_parch)

g10 <- glm(data=data,as.factor(survived)~as.factor(one_sibsp)+as.factor(one_two_parch)
           + as.factor(one_sibsp)*as.factor(one_two_parch),family=binomial,y=TRUE)
summary(g10)
anova(g10,test="Chisq")

g10.alt1 <- glm(data=data,as.factor(survived)~as.factor(one_sibsp)+as.factor(one_two_parch),
                family=binomial,y=TRUE)
g10.alt2 <- glm(data=data,as.factor(survived)~as.factor(one_two_parch)+as.factor(one_sibsp),
                family=binomial,y=TRUE)
anova(g10.alt1,test="Chisq")
anova(g10.alt2,test="Chisq")

#sibsp/fare
p11 <- ggplot(data=data,aes(color=as.factor(survived),x=fare, y=sibsp))
p11 + geom_jitter()

g11 <- glm(data=data,as.factor(survived)~as.factor(one_sibsp)+fare
           + as.factor(one_sibsp)*fare,family=binomial,y=TRUE)
summary(g11)
anova(g11,test="Chisq")

#sibsp/cabin_listed
p12 <- ggplot(data=data,aes(color=as.factor(survived),x=as.factor(cabin_listed), y=sibsp))
p12 + geom_jitter()

g12 <- glm(data=data,as.factor(survived)~as.factor(one_sibsp)+as.factor(cabin_listed)
           + as.factor(one_sibsp)*as.factor(cabin_listed),family=binomial,y=TRUE)
summary(g12)
anova(g12,test="Chisq")

#parch/fare
p13 <- ggplot(data=data,aes(color=as.factor(survived),x=parch, y=fare))
p13 + geom_jitter()

p13.alt <- ggplot(data=data,aes(color=as.factor(survived),x=one_two_parch, y=fare))
p13.alt + geom_jitter()
g13 <- glm(data=data,as.factor(survived)~as.factor(one_two_parch)+fare
           + as.factor(one_two_parch)*fare,family=binomial,y=TRUE)
summary(g13)
anova(g13,test="Chisq")
g13$coef

#parch/cabin_listed
p14 <- ggplot(data=data,aes(color=as.factor(survived),x=as.factor(cabin_listed), y=parch))
p14 + geom_jitter()

g14 <- glm(data=data,as.factor(survived)~as.factor(one_two_parch)+as.factor(cabin_listed)
           + as.factor(one_two_parch)*as.factor(cabin_listed),family=binomial,y=TRUE)
summary(g14)
anova(g14,test="Chisq")

#fare/cabin_listed
p15 <- ggplot(data=data,aes(color=as.factor(survived),x=as.factor(cabin_listed), y=fare))
p15 + geom_jitter()

g15 <- glm(data=data,as.factor(survived)~fare+as.factor(cabin_listed)
           + fare*as.factor(cabin_listed),family=binomial,y=TRUE)
summary(g15)
anova(g15,test="Chisq")


#traveling alone
for(i in 1:length(data$survived)){
  if(data$parch[i] == 0 & data$sibsp[i] == 0){data$alone[i] <- 1}
  if(data$parch[i] != 0 | data$sibsp[i] != 0){data$alone[i] <- 0}
}
table(sibsp,parch,data$alone)

write.csv(data,"/home/john86/Kaggle/Titanic/Data/training_set.csv")

  #glm to evaluate deviance explained by traveling alone
g16 <- glm(data=data,as.factor(survived)~as.factor(alone),family=binomial)
summary(g16)

  #glm to evaluate deviance explained by parch and sibsp
g17 <- glm(data=data,as.factor(survived)~as.factor(one_sibsp)+as.factor(one_two_parch),
           family=binomial)
summary(g17)

#class/alone
attach(data)
table(survived,pclass,alone)

g18 <- glm(data=data,as.factor(survived)~as.factor(alone)+as.factor(pclass)
          +as.factor(alone)*as.factor(pclass),
           family=binomial)
summary(g18)
anova(g18,test="Chisq")

#alone/sex
table(survived,sex,alone)

g19 <- glm(data=data,as.factor(survived)~as.factor(alone)+sex
           +as.factor(alone)*sex,
           family=binomial)
summary(g19)
anova(g19,test="Chisq")

#alone/fare
p20 <- ggplot(data=data,aes(color=as.factor(survived),x=alone, y=fare))
p20 + geom_jitter()

g20 <- glm(data=data,as.factor(survived)~as.factor(alone)+fare
           +as.factor(alone)*fare,
           family=binomial)
summary(g20)
anova(g20,test="Chisq")
g20$coef

# class/sex/alone
table(survived,pclass,sex,alone)

g21 <- glm(data=data,as.factor(survived)~as.factor(alone)+sex+as.factor(pclass)
           +as.factor(alone)*sex + as.factor(alone)*as.factor(pclass) + sex*as.factor(pclass)+
           +as.factor(alone)*sex*as.factor(pclass) ,
          family=binomial)
anova(g21,test="Chisq")

#sex/alone/fare

g22 <- glm(data=data,as.factor(survived)~as.factor(alone)+sex+fare
           +as.factor(alone)*sex + as.factor(alone)*fare + sex*fare+
             +as.factor(alone)*sex*fare ,
           family=binomial)
anova(g22,test="Chisq")
hist(fare)
hist(log(fare+1),breaks=40)

p23 <- ggplot(data=data,aes(x=fare,fill=as.factor(survived))) +geom_dotplot(stackgroups=TRUE,
  binpositions="all",method="histodot",binwidth=10)
p23 + facet_grid(.~pclass)
