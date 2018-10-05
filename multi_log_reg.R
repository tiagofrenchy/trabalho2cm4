library(readxl)
library(PerformanceAnalytics)
library(nnet)

#Sets working directory for syncronization with GitHub
setwd("/users/TSiqueira/Documents/trabalho2cm4")

#Creates inital data frame fot T2
initdf <- read_excel("BDLOGIST_GRUPO_11.xlsx")
View(initdf)

#Finding interaction between variables
pairs(initdf[2:24])

#### travail en cours
chart.Correlation(initdf, method="pearson", histogram=TRUE, pch=16)

#Univariate linear regression for all variables 
resultsList <- list()
for (i in colnames(initdf)[2:23]) {
  #name <- sprintf("%s ~ %s","lmfit obito", i) 
  fit <- lm(substitute(obito ~ i, list(i = as.name(i))), data = initdf)
  resultsList[[i]] <- summary(fit)
}

#Multiple linear regression of ALL variables
fitall <- lm(obito ~ ., data = initdf[2:24])
summary(fitall)

#Multiple linear regression of SELECTED variables
select <- ""
fitselect <- lm(substitute(obito ~ select, list(select = as.name(select))), data = initdf)
summary(fitselect)
