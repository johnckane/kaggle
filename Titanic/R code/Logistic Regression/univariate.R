require(ggplot2)

# load training_set
data <- read.csv("/home/john86/Kaggle/Titanic/Data/training_set.csv")

attach(data)

table(survived,pclass)
table(survived,sex)
table(survived,sibsp)
table(survived,parch)
table(survived,cabin_listed)

p1 <- ggplot(data=data,aes(x=as.factor(survived),y=fare))
p1 + geom_boxplot()
glm <- glm(data=data,as.factor(survived)~fare,family=binomial)
summary(glm)
glm$coef
exp(glm$coef[2])

hist(fare,breaks=20)

#look at fare within class and how that relates to survival

p2 <- ggplot(data=data,aes(x=as.factor(survived),y=fare))
p2 + geom_jitter() + facet_grid(.~pclass)
hist(fare,breaks=20)
p2 + geom_boxplot()


p3 <- ggplot(data=data,aes(x=as.factor(pclass),y=fare))
p3 + geom_boxplot()

p4 <- ggplot(data=data,aes(x=fare))
p4 + geom_histogram() + facet_grid(.~pclass)
