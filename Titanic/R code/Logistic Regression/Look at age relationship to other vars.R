require(ggplot2)

# load training_set
data <- read.csv("/home/john86/Kaggle/Titanic/Data/training_set.csv")

attach(data)


# examine missinginess
table(survived)
table(pclass)
table(sex)
summary(age)
145/712
summary(sibsp)
summary(parch)
summary(fare)
is.na(fare)
table(cabin_listed)

# Try various models for age

p1 <- ggplot(data=data,aes(x=as.factor(pclass),y=age))
p1 + geom_boxplot()
summary(lm(age~as.factor(pclass)))


p2 <- ggplot(data=data,aes(x=sex,y=age))
p2 + geom_boxplot()
summary(lm(age~sex))

p3 <- ggplot(data=data,aes(x=as.factor(sibsp),y=age))
p3 + geom_boxplot()
cor(age,sibsp,use="pairwise.complete.obs")
p3.alt <- ggplot(data=data,aes(x=sibsp,y=age))
p3.alt + geom_point()

p4 <- ggplot(data=data,aes(x=as.factor(parch),y=age))
p4 + geom_boxplot()

p4.alt <- ggplot(data=data,aes(x=parch,y=age))
p4.alt + geom_point()

p5 <- ggplot(data=data,aes(x=fare,y=age))
p5 + geom_point()
summary(lm(age~fare))

p6 <- ggplot(data=data,aes(x=as.factor(cabin_listed),y=age))
p6 + geom_boxplot()
summary(lm(age~as.factor(cabin_listed)))


age.fit <- lm(age~as.factor(pclass)+sibsp+as.factor(cabin_listed),y=TRUE)
summary(age.fit)
plot(x=age.fit$fitted.values,y=age.fit$y)


