library(readxl)
library(PerformanceAnalytics)
library(nnet)
library(Amelia)
library(mlbench)
library(caTools)

#Sets working directory for syncronization with GitHub
setwd("~/Downloads/CM4T2")

#Creates inital data frame fot T2
initdf <- read_excel("BDLOGIST_GRUPO_11.xlsx")
View(initdf)

#Finding interaction between variables
pairs(initdf[2:24])
chart.Correlation(initdf, method="pearson", histogram=TRUE, pch=16)

#Splitting the df in two to analyse separately after correlation identified
splitdf <- split(initdf, initdf$sbdc)
naosbdc <- splitdf[[1]]
simsbdc <- splitdf[[2]]

#Bartlett test for homocedasticity
resultsListBart <- list()
for (i in colnames(initdf)[2:23]) {
  #name <- sprintf("%s ~ %s","lmfit obito", i) 
  bart <- bartlett.test(substitute(obito ~ i, list(i = as.name(i))), data = initdf)
  resultsList[[i]] <- summary(bart)
  print(summary(bart))
}

#Univariate linear regression for all variables 
resultsList <- list()
for (i in colnames(initdf)[2:23]) {
  #name <- sprintf("%s ~ %s","lmfit obito", i) 
  fit <- glm(substitute(obito ~ i, list(i = as.name(i))),family = binomial(logit), data = initdf)
  resultsList[[i]] <- summary(fit)
  print(summary(fit))
}

#Multiple linear regression of ALL variables
fitall <- glm(obito ~ .,family = binomial(logit), data = initdf[2:24])
summary(fitall)

#Multiple linear regression of SELECTED variables
fitselect <- glm(obito ~ idade+biapos+sbdc+bic_cat+creat_cat+album+gao2_cat+tcec,family = binomial(logit) , data = initdf)
summary(fitselect)

#Multiple linear regression of SELECTED AGAIN variables
fitselect2 <- glm(obito ~ sbdc+bic_cat+tcec,family = binomial(logit) , data = initdf)
summary(fitselect2)

fitselect3 <- glm(obito ~ bic_cat+tcec,family = binomial(logit) , data = naosbdc)
summary(fitselect3)

fitselect4 <- glm(obito ~ bic_cat+tcec,family = binomial(logit) , data = simsbdc)
summary(fitselect4)

fitselect20 <- glm(obito ~ sbdc+bic_cat+tcec+biapos,family = binomial(logit) , data = initdf)
summary(fitselect20)