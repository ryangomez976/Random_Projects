# Homework Set No. 3, Problem #
# Project from: http://bit.ly/1h1hGV4

titanic <- read.csv("http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/titanic3.csv")

titanic.survival.train <- glm(survived ~ pclass + sex + pclass:sex + age + sibsp, family = binomial(logit), data = titanic)
summary(titanic.survival.train)

# RandomForest
library(randomForest)
# impute missing values
# Author used a regression analysis to determine missing values
# I used the rfImpute function available in the randomForest package
titanic.impute <- rfImpute(survived ~ pclass + sex + age + sibsp, data = titanic, ntree = 5000, importance = TRUE)
# create random forest
titanic.survival.train.rf <- randomForest(survived ~ pclass + sex + age + sibsp, data = titanic.impute, ntree = 5000, importance = TRUE)
titanic.survival.train.rf
# Conditional Tree
library(party)
titanic.survival.train.ctree <- ctree(survived ~ pclass + sex + age + sibsp, data = titanic.impute)
titanic.survival.train.ctree
plot(titanic.survival.train.ctree)
# confusionMatrix
library(caret)
confusionMatrix(titanic.survival.train.rf)