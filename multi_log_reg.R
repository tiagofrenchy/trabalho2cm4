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

#Multiple linear regression of all variables
fitall <- lm(obito ~ ., data = initdf[2:24])
summary(fitall)

#Univariate linear regression for all variables 
for (nayme in colnames(initdf)[2:24]) {
  sprintf("%s ~ %s","fit obito", nayme) <- lm(obito ~ nayme, data = initdf[2:24])
  summary(sprintf("%s ~ %s","fit obito", nayme))
}


fit <- lm(obito ~ sexo, data = initdf[2:24])
summary(fit)

