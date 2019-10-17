library(pacman)
p_load(tidyverse, caret, GGally, treemap,
       ggplot2, lattice, corrplot, purrr,
       e1071, randomForest, interplot, effects,
       DMwR, fmsb, dplyr, corrplot, coreSim, gridExtra)
setwd('/home/nicoluarte/employee_leave/')
data <- read.csv('IBM_dataset.csv')
colnames(data)

## data is fairly unbalanced so we need to correct that
prop.table(table(data$Attrition))
sqrt(ncol(data))
balancedData <- SMOTE(Attrition ~ ., data = data, perc.over = 100,
                      k = sqrt(ncol(data)), perc.under = 200)
prop.table(table(balancedData$Attrition))

## check factor linear correlations
subSet <- dplyr::select(data, PercentSalaryHike, WorkLifeBalance, JobInvolvement,
                        EnvironmentSatisfaction)
subSet <- as.numeric(subSet)
M <- cor(subSet)
corrplot::corrplot(M, type = "upper", order = "hclust",
                   col = c("black", "white"),
                   bg = "lightblue",
                   tl.col = "black",
                   method = "number")


## train the model
factors <- c("PercentSalaryHike", "WorkLifeBalance", "JobInvolvement", "EnvironmentSatisfaction",
             "Age:PercentSalaryHike", "Age:WorkLifeBalance", "Age:JobInvolvement", "Age:EnvironmentSatisfaction",
             "MonthlyIncome:PercentSalaryHike", "MonthlyIncome:WorkLifeBalance", "MonthlyIncome:JobInvolvement", "MonthlyIncome:EnvironmentSatisfaction")
mdl0Formula <- as.formula(paste("Attrition~", paste(factors, collapse="+")))
controlParams <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3)
mdl0 <- train(mdl0Formula,
              method = "glm",
              family = "binomial",
              data = balancedData)
summary(mdl0)
confusionMatrix(mdl0)

## model simulation
factors2 <- c("PercentSalaryHike", "WorkLifeBalance", "JobInvolvement", "EnvironmentSatisfaction")
mdl0Formula2 <- as.formula(paste("Attrition ~", paste(factors2, collapse="+")))
prFun <- function(x) 1 / (1 + exp(-x))
mdlSim <- glm(mdl0Formula, data = balancedData, family = 'binomial')
# mdlfitted <- expand.grid(percentsalaryhike = seq(1, 100, by = 1),
#                          worklifebalance = c(1,2,3,4),
#                          jobinvolvement = c(1,2,3,4),
#                          environmentsatisfaction = c(1,2,3,4))
mdlfitted <- expand.grid(EnvironmentSatisfaction = c(3),
                         JobInvolvement = seq(1, 4, by = 0.1),
                         WorkLifeBalance = c(1),
                         PercentSalaryHike = c(15))
mdlfitted1 <- expand.grid(EnvironmentSatisfaction = c(2),
                         JobInvolvement = seq(1, 4, by = 0.1),
                         WorkLifeBalance = c(2),
                         PercentSalaryHike = c(15))
mdlfitted2 <- expand.grid(EnvironmentSatisfaction = c(4),
                         JobInvolvement = seq(1, 4, by = 1),
                         WorkLifeBalance = c(4),
                         PercentSalaryHike = c(15))
logisticQI <- qi_builder(mdlSim, mdlfitted, FUN = prFun, slim = TRUE)
logisticQI1 <- qi_builder(mdlSim, mdlfitted1, FUN = prFun, slim = TRUE)
logisticQI2 <- qi_builder(mdlSim, mdlfitted2, FUN = prFun, slim = TRUE)
View(logisticQI)

p1 <- ggplot(logisticQI, aes(JobInvolvement, qi_median)) +
    geom_ribbon(aes(ymin = qi_min, ymax = qi_max), alpha = 0.3) +
    geom_line() +
    ylab('Attrition') +
    ggtitle("WorkLifeBalance = 1") +
    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    coord_cartesian(ylim = c(0, 1))
p2 <- ggplot(logisticQI1, aes(JobInvolvement, qi_median)) +
    geom_ribbon(aes(ymin = qi_min, ymax = qi_max), alpha = 0.3) +
    geom_line() +
    ylab('Attrition') +
    ggtitle("WorkLifeBalance = 2") +
    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    coord_cartesian(ylim = c(0, 1))
p3 <- ggplot(logisticQI2, aes(JobInvolvement, qi_median)) +
    geom_ribbon(aes(ymin = qi_min, ymax = qi_max), alpha = 0.3) +
    geom_line() +
    ylab('Attrition') +
    ggtitle("WorkLifeBalance = 4") +
    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    coord_cartesian(ylim = c(0, 1))
grid.arrange(p1, p2, p3, nrow = 1)
