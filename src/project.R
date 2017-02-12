##
# Jace Robinson
# Machine Learning
# Project: College Scorecard
# 10-8-16
##

library(ggplot2)
library(reshape)
library(grid)
library(gridExtra)

library(pscl)
library(ROCR)
library(MCMCpack)

performAnalysis <- function(equation, target = "CONTROL", labels=c("Private", "Public"), target2 = 1) {
  dataTrain <- read.csv("trainingData2012-13.csv")
  dataValidate <- read.csv("validationData2012-13.csv")
  dataTest <- read.csv("testingData2012-13.csv")
  
  if(target == "CONTROL") {
    
  } else if(target =="REGION") {
    dataTrain <- read.csv("trainingData2012-13REGION.csv")
    dataValidate <- read.csv("validationData2012-13REGION.csv")
    dataTest <- read.csv("testingData2012-13REGION.csv")
    
    
    # tmpInit <- paste(dataScaled$INSTNM)
    # tmpTrain <- paste(dataTrain$INSTNM)
    # tmpValidate <- paste(dataValidate$INSTNM)
    # tmpTest <- paste(dataTest$INSTNM)
    # 
    # trainRows <- unlist(sapply(1:nrow(dataTrain), function(row) {
    #   which((tmpInit == tmpTrain[row]) & (abs(dataScaled$UGDS-dataTrain$UGDS[row]) < 10^-8))
    # }))
    # validateRows <- unlist(sapply(1:nrow(dataValidate), function(row) {
    #   which((tmpInit == tmpValidate[row]) & (abs(dataScaled$UGDS-dataValidate$UGDS[row]) < 10^-8))
    # }))
    # testRows <- unlist(sapply(1:nrow(dataTest), function(row) {
    #   which((tmpInit == tmpTest[row]) & (abs(dataScaled$UGDS-dataTest$UGDS[row]) < 10^-8))
    # }))
    
    # dataTrain$REGION <- dataScaled$REGION[trainRows]
    # dataValidate$REGION <- dataScaled$REGION[validateRows]
    # dataTest$REGION <- dataScaled$REGION[testRows]
    
    dataTrain$CONTROL = dataTrain$REGION
    dataValidate$CONTROL = dataValidate$REGION
    dataTest$CONTROL = dataTest$REGION
  } 
  
  dataTrain$CONTROL2 <- ifelse(dataTrain$CONTROL==target2,1,0)
  dataTrain$CONTROL3 <- factor(dataTrain$CONTROL2, labels=labels)
  dataValidate$CONTROL2 <- ifelse(dataValidate$CONTROL==target2,1,0)
  dataValidate$CONTROL3 <- factor(dataValidate$CONTROL2, labels=labels)
  dataTest$CONTROL2 <- ifelse(dataTest$CONTROL==target2,1,0)
  dataTest$CONTROL3 <- factor(dataTest$CONTROL2, labels=labels)
  
  # Logistic Regression
  # Following example from https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
  model <- glm(equation,family=binomial(link='logit'),data=dataTrain)
  print(summary(model))
  
  model.results <- predict(model, newdata=dataValidate, type='response')
  dataValidate$RESULTS <- model.results
  aic <- calcAIC(dataValidate$RESULTS, dataValidate$CONTROL2, length(model$coefficients))
  print(paste("AIC:",aic))
  
  anova(model, test="Chisq")
  print(pR2(model))
  model.results <- predict(model, newdata=dataTest, type='response')
  dataTest$RESULTS <- model.results
  dataTest$RESULTS2 <- ifelse(dataTest$RESULTS > .5,1,0)
  misClasificError <- mean(dataTest$RESULTS2 != dataTest$CONTROL2)
  print(paste("Accuracy:", 1-misClasificError))
  confMat <- table(dataTest$RESULTS2, dataTest$CONTROL2, dnn=c("Predicted","Actual"))
  print(confMat)
  
  if(nrow(confMat) == 2 && ncol(confMat) == 2) {
  prec <- confMat[2,2]/(confMat[2,2] + confMat[2,1])
  recall <- confMat[2,2]/(confMat[2,2] + confMat[1,2])
  
  if(is.na(prec) || is.na(recall)) {
    F1 <- 0
  } else {
    F1 <- (2*prec*recall)/(prec + recall) 
  }
  print(paste("F1", F1))
  }
  
  
  pr <- prediction(dataTest$RESULTS, dataTest$CONTROL2)
  # print(pr)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  # plot(prf, main="ROC Curve for Tuition vs. Control")
  
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  print(paste("AUC:",auc))
  
  # perf1 <- performance(pr, "prec", "rec")
  # plot(perf1, main= "Precision-Recall Curve for Tuition vs. Control")
}

performAnalysisBayes <- function(equation, b0=0, B0=0, features,target = "CONTROL", labels=c("Private", "Public"),target2=1) {
  dataTrain <- read.csv("trainingData2012-13.csv")
  dataValidate <- read.csv("validationData2012-13.csv")
  dataTest <- read.csv("testingData2012-13.csv")
  
  if(target == "CONTROL") {
    
  } else if(target =="REGION") {
    dataTrain <- read.csv("trainingData2012-13REGION.csv")
    dataValidate <- read.csv("validationData2012-13REGION.csv")
    dataTest <- read.csv("testingData2012-13REGION.csv")
    
    
    # tmpInit <- paste(dataScaled$INSTNM)
    # tmpTrain <- paste(dataTrain$INSTNM)
    # tmpValidate <- paste(dataValidate$INSTNM)
    # tmpTest <- paste(dataTest$INSTNM)
    # 
    # trainRows <- unlist(sapply(1:nrow(dataTrain), function(row) {
    #   which((tmpInit == tmpTrain[row]) & (abs(dataScaled$UGDS-dataTrain$UGDS[row]) < 10^-8))
    # }))
    # validateRows <- unlist(sapply(1:nrow(dataValidate), function(row) {
    #   which((tmpInit == tmpValidate[row]) & (abs(dataScaled$UGDS-dataValidate$UGDS[row]) < 10^-8))
    # }))
    # testRows <- unlist(sapply(1:nrow(dataTest), function(row) {
    #   which((tmpInit == tmpTest[row]) & (abs(dataScaled$UGDS-dataTest$UGDS[row]) < 10^-8))
    # }))
    
    # dataTrain$REGION <- dataScaled$REGION[trainRows]
    # dataValidate$REGION <- dataScaled$REGION[validateRows]
    # dataTest$REGION <- dataScaled$REGION[testRows]
    
    dataTrain$CONTROL = dataTrain$REGION
    dataValidate$CONTROL = dataValidate$REGION
    dataTest$CONTROL = dataTest$REGION
  } 
  
  dataTrain$CONTROL2 <- ifelse(dataTrain$CONTROL==target2,1,0)
  dataTrain$CONTROL3 <- factor(dataTrain$CONTROL2, labels=labels)
  dataValidate$CONTROL2 <- ifelse(dataValidate$CONTROL==target2,1,0)
  dataValidate$CONTROL3 <- factor(dataValidate$CONTROL2, labels=labels)
  dataTest$CONTROL2 <- ifelse(dataTest$CONTROL==target2,1,0)
  dataTest$CONTROL3 <- factor(dataTest$CONTROL2, labels=labels)
  
  # Uniform Prior, bo is mean, BO is variance of gaussian priors
  model1.posterior <- MCMClogit(equation, data=dataTrain, b0=b0, B0=B0,mcmc=100000)
  
  stats <- summary(model1.posterior)
  print(stats)
  # plot(model1.posterior)
  
  # Let the mean of each parameters be my estimate of coefficients (NOTE MAY CHANGE THIS)
  thetas <- stats$statistics[,1]
  print(thetas)
  
  dataValidate$RESULTS <- t(sigmoid(thetas = thetas,x = dataValidate[,features]))
  aic <- calcAIC(dataValidate$RESULTS, dataValidate$CONTROL2, length(model$coefficients))
  print(paste("AIC:",aic))
  
  dataTest$RESULTS <- t(sigmoid(thetas = thetas,x = dataTest[,features]))
  
  dataTest$RESULTS2 <- ifelse(dataTest$RESULTS > .5,1,0)
  misClasificError <- mean(dataTest$RESULTS2 != dataTest$CONTROL2)
  print(paste("Accuracy:", 1-misClasificError))
  confMat <- table(dataTest$RESULTS2, dataTest$CONTROL2, dnn=c("Predicted","Actual"))
  print(confMat)
  
  if(nrow(confMat) == 2 && ncol(confMat) == 2) {
    prec <- confMat[2,2]/(confMat[2,2] + confMat[2,1])
    recall <- confMat[2,2]/(confMat[2,2] + confMat[1,2])
    
    if(is.na(prec) || is.na(recall)) {
      F1 <- 0
    } else {
      F1 <- (2*prec*recall)/(prec + recall) 
    }
    print(paste("F1", F1))
  }
  
  pr <- prediction(dataTest$RESULTS, dataTest$CONTROL2)
  # print(pr)
  # prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  # plot(prf, main="ROC Curve for Tuition vs. Control")
  
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  print(paste("AUC:",auc))
}

sigmoid <- function(thetas, x) {
  return(1/(1+exp(-(thetas[1] + thetas[2:length(thetas)]%*%t(x)))))
}
sigmoid2 <- function(x) {
  return(1/(1+exp(-x)))
}

calcAIC <- function(YFit, YTruth, numParams) {
  likelihood <- sum(YTruth*log(YFit) + (1-YTruth)*log(1-YFit))
  logLik <- likelihood
  
  aic <- -2*likelihood + 2*numParams
  
  return(aic)
}

# Now partition the data into training, validation, and testing
partitionDataToFile <- function(data,sizes=c(.8,.2)) {
  numRows <- length(data[,1])
  numCols <- length(data[1,])
  
  # Create randomized list of the indices
  randIndices <- sample(1:numRows,numRows)
  # randIndices <- 1:m
  
  # Divide data is 80% training, 20% testing
  trainCount <- ceiling(sizes[1]*numRows)
  
  # Further divide the training data into 80% training, 20% validation
  validationCount <- ceiling(sizes[2]*trainCount)
  trainCount <- trainCount - validationCount
  
  testCount <- numRows-trainCount - validationCount
  
  dataTrain <- data[randIndices[1:trainCount],]
  
  dataValidation <- data[randIndices[(trainCount+1):(trainCount+validationCount)],]
  
  dataTest <- data[randIndices[(trainCount+validationCount+1):(trainCount+validationCount+testCount)],]
  
  write.table(dataTrain,file="trainingData2012-13REGION.csv", sep=",", row.names=F)
  write.table(dataValidation,file="validationData2012-13REGION.csv", sep=",", row.names=F)
  write.table(dataTest,file="testingData2012-13REGION.csv", sep=",", row.names=F)
}


setwd("/home/jace/SchoolFall2016/MachineLearning/project/")
# dataFile <- "CollegeScorecard_Raw_Data/MERGED2014_15_PP.csv"
dataFile <- "CollegeScorecard_Raw_Data/MERGED2012_13_PP.csv"
dataInit <- read.csv(dataFile)

name <- c("INSTNM")
target <- c("CONTROL")
# target <- c("REGION")
# target <- c("HIGHDEG")
# target <- c("MAIN")
# target <- c("ST_FIPS")

# Only available 2014-15
# target <- c("LOCALE")
# target <- c("LOCALE2")
# target <- c("CCBASIC")
# target <- c("CCUGPROF")
# target <- c("CCSIZSET")
# target <- c("HBCU")
# target <- c("PBI")
# target <- c("ANNHI")
# target <- c("TRIBAL")
# target <- c("AANAPII")
# target <- c("HSI")
# target <- c("NANTI")
# target <- c("MENONLY")
# target <- c("WOMENONLY")
# target <- c("RELAFFIL")

if(target == "CONTROL") {
  labels <- c("Public", "Private NonProft", "Private For-Profit")
} else if(target == "REGION") {
  labels <- c("New England", "Mid East", "Great Lakes", "Plains", "Southeast", "Southwest", "Rocky Mountains", "Far West", "Outlying Areas")
} else if(target == "CCUGPROF") {
  
} else if(target == "CCSIZSET") {
  
}


# features <- c("ADM_RATE","SATMTMID","ACTMTMID","UGDS", "TUITIONFEE_IN", "TUITIONFEE_OUT","TUITFTE","AVGFACSAL","C150_4","COMPL_RPY_5YR_RT", "PAR_ED_PCT_1STGEN", "DEP_INC_AVG", "IND_INC_AVG","GRAD_DEBT_MDN","WDRAW_DEBT_MDN","LO_INC_DEBT_MDN","MD_INC_DEBT_MDN","HI_INC_DEBT_MDN","FAMINC","MD_FAMINC","COUNT_NWNE_P10","COUNT_WNE_P10", "MN_EARN_WNE_P10", "MD_EARN_WNE_P10")

# 2012-13 data
if(dataFile == "CollegeScorecard_Raw_Data/MERGED2012_13_PP.csv") {
  features <- c("UGDS", "TUITIONFEE_IN", "TUITIONFEE_OUT","TUITFTE","AVGFACSAL","C150_4","COMPL_RPY_5YR_RT", "PAR_ED_PCT_1STGEN", "FAMINC", "DEBT_MDN", "GRAD_DEBT_MDN","WDRAW_DEBT_MDN","CDR2", "MN_EARN_WNE_P10") 
} else if (dataFile == "CollegeScorecard_Raw_Data/MERGED2012_13_PP.csv") {
  features <- c("UGDS", "TUITIONFEE_IN", "TUITIONFEE_OUT","TUITFTE","AVGFACSAL","C150_4","COMPL_RPY_5YR_RT", "PAR_ED_PCT_1STGEN","PAR_ED_PCT_PS", "FAMINC", "DEBT_MDN", "GRAD_DEBT_MDN","WDRAW_DEBT_MDN")
}
# admission rate, SAT math midpoint, ACT math midpoint, undergraduate enrollment, tuition fees in state, tuition fees out of state, net tuition revenue per full-time student, completition rates for first-time, full-time students at 4 year inst, five-year repayment rate for completers, percentage of first-generation students, average family income for depent students, avg family income for indepent students, The median debt for students who have completed, The median debt for students who have not completed, The median debt for students with family income between $0-$30,000, The median debt for students with family income between $30,001-$75,000, The median debt for TUITFTEstudents with family income $75,001+, average family income, median family income, Number of students not working and not enrolled 10 years after entry, Number of students not working and not enrolled 10 years after entry, Number of students working and not enrolled 10 years after entry,Mean earnings of students working and not enrolled 10 years after entry,Median earnings of students working and not enrolled 10 years after entry

dataSubset <- dataInit[,c(name, target, features)]

# Replace data elements of NULL and PrivacySuppressed with NA.
is.na(dataSubset) <- dataSubset == "NULL"
is.na(dataSubset) <- dataSubset == "PrivacySuppressed"


# remove data with na's
dataSubset <- na.omit(dataSubset)
# Remove university of pheonix as it is major outlier in enrollment category
if(length(which(dataSubset[,name] == "University of Phoenix-Online Campus")) > 0) {
  dataSubset <- dataSubset[-which(dataSubset[,name] == "University of Phoenix-Online Campus"),]
}

# Convert datatype of factors to numeric data: TODO fix warning on this
dataSubset[,features] <- sapply(features, function(col) {
  as.numeric(levels(dataSubset[,col]))[dataSubset[,col]]
})

if(exists(labels)) {
  dataSubset[,target] <- factor(dataSubset[,target], labels=labels) 
} else {
  dataSubset[,target] <- factor(dataSubset[,target]) 
}

# normalize each feature
dataScaled <- dataSubset
dataScaled[,features] <- sapply(features, function(x) {
  (dataSubset[,x] - mean(dataSubset[,x]))/(max(dataSubset[,x]) - min(dataSubset[,x]))
})

# partitionDataToFile(dataScaled)

# initially exam relationship of binary recurrent to each feature
data.melted1 <- melt(dataScaled[,c(2,3:7)], id = target)
data.melted2 <- melt(dataScaled[,c(2,8:12)], id = target)
p1 <- ggplot(data = data.melted1, aes(x = data.melted1[,1], y = value, col=data.melted1[,1])) +
  geom_point() + facet_grid(. ~ variable) + theme(axis.text.x=element_blank()) + labs(x = target, col=target)
p2 <- ggplot(data = data.melted2, aes(x = data.melted2[,1], y = value, col=data.melted2[,1])) +
  geom_point() + facet_grid(. ~ variable) + theme(axis.text.x=element_blank()) + labs(x = target, col=target)
grid.arrange(p1, p2)

data.melted1 <- melt(dataScaled[,c(2,13:(length(features)+2))], id = target)
p1 <- ggplot(data = data.melted1, aes(x = data.melted1[,1], y = value, col=data.melted1[,1])) +
  geom_point() + facet_grid(. ~ variable) + theme(axis.text.x=element_blank()) + labs(x = target, col=target)
print(p1)

########################################
dataTrain <- read.csv("trainingData2012-13.csv")
dataValidate <- read.csv("validationData2012-13.csv")
dataTest <- read.csv("testingData2012-13.csv")

dataTrain$CONTROL2 <- ifelse(dataTrain$CONTROL==1,1,0)
dataTrain$CONTROL3 <- factor(dataTrain$CONTROL2, labels=c("Private","Public"))
dataValidate$CONTROL2 <- ifelse(dataValidate$CONTROL==1,1,0)
dataValidate$CONTROL3 <- factor(dataValidate$CONTROL2, labels=c("Private","Public"))
dataTest$CONTROL2 <- ifelse(dataTest$CONTROL==1,1,0)
dataTest$CONTROL3 <- factor(dataTest$CONTROL2, labels=c("Private","Public"))


# Logistic Regression
# Following example from https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/

model <- glm(CONTROL3 ~ TUITIONFEE_IN + TUITIONFEE_OUT,family=binomial(link='logit'),data=dataTrain)
summary(model)

anova(model, test="Chisq")
pR2(model)
model.results <- predict(model, newdata=dataTest, type='response')
dataTest$RESULTS <- model.results
model.results <- ifelse(model.results > .5,1,0)
misClasificError <- mean(model.results != dataTest$CONTROL2)
print(paste("Accuracy:", 1-misClasificError))
table(model.results, dataTest$CONTROL2, dnn=c("Predicted","Actual"))

# 1d plots for each variable
p1 <- ggplot(data=dataTest) + geom_point(size=1, aes(x=TUITIONFEE_IN, y=CONTROL2, col=CONTROL3)) + labs(x="In-State Tuition", y="Control", color="Control")
p2 <- ggplot(data=dataTest) + geom_point(size=1, aes(x=TUITIONFEE_IN, y=RESULTS, col=CONTROL3),shape=4) + geom_abline(slope=0,intercept=.5,linetype=2) + geom_text(data = dataTest[(dataTest$RESULTS <= .91 & dataTest$RESULTS >= .39) | dataTest$INSTNM == "University of Minnesota-Duluth",], aes(x=TUITIONFEE_IN, y=RESULTS, label=INSTNM), size=2,nudge_x = 0.12) + labs(x="In-State Tuition", y="Control", color="Control")
grid.arrange(p1, p2)

p1 <- ggplot(data=dataTest) + geom_point(size=1, aes(x=TUITIONFEE_OUT, y=CONTROL2, col=CONTROL3)) + labs(x="Out-State Tuition", y="Control", color="Control")
p2 <- ggplot(data=dataTest) + geom_point(size=1, aes(x=TUITIONFEE_OUT, y=RESULTS, col=CONTROL3),shape=4) + geom_abline(slope=0,intercept=.5,linetype=2) + geom_text(data = dataTest[dataTest$RESULTS <= .91 & dataTest$RESULTS >= .39 | dataTest$INSTNM == "University of Minnesota-Duluth",], aes(x=TUITIONFEE_OUT, y=RESULTS, label=INSTNM), size=2,nudge_x = 0.12) + labs(x="Out-State Tuition", y="Control", color="Control")
grid.arrange(p1, p2)

# Generate a 2d plot 1/2 decision boundary
tuitIn <- seq(-.3,.6,.01)
tuitOut <- (-model$coefficients[1] - model$coefficients[2]*tuitIn)/model$coefficients[3]
tmpData <- data.frame(tuitIn = tuitIn, tuitOut = tuitOut)

# General 2d plot 
pVanilla <- ggplot(data=dataTest) + geom_point(aes(x=TUITIONFEE_IN, y=TUITIONFEE_OUT, col=CONTROL3)) + ggtitle("Tuition Costs for Public vs. Private Universities") + geom_line(data=tmpData,aes(x=tuitIn, y=tuitOut),linetype=3) + labs(x="In-state tuition", y="Out-of-state tuition", color="Control")

pr <- prediction(model.results, dataTest$CONTROL2)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, main="ROC Curve for Tuition vs. Control")

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

perf1 <- performance(pr, "prec", "rec")
plot(perf1, main= "Precision-Recall Curve for Tuition vs. Control")

model.results <- predict(model, newdata=dataValidate, type='response')
dataValidate$RESULTS <- model.results
print(paste("AIC:",calcAIC(dataValidate$RESULTS, dataValidate$CONTROL2, 3)))

######################################
## Bayesian Examples
dataTrain <- read.csv("trainingData2012-13.csv")
dataValidate <- read.csv("validationData2012-13.csv")
dataTest <- read.csv("testingData2012-13.csv")

dataTrain$CONTROL2 <- ifelse(dataTrain$CONTROL==1,1,0)
dataTrain$CONTROL3 <- factor(dataTrain$CONTROL2, labels=c("Private","Public"))
dataValidate$CONTROL2 <- ifelse(dataValidate$CONTROL==1,1,0)
dataValidate$CONTROL3 <- factor(dataValidate$CONTROL2, labels=c("Private","Public"))
dataTest$CONTROL2 <- ifelse(dataTest$CONTROL==1,1,0)
dataTest$CONTROL3 <- factor(dataTest$CONTROL2, labels=c("Private","Public"))

# Uniform Prior, bo is mean, BO is variance of gaussian priors
model1.posterior <- MCMClogit(CONTROL2 ~ TUITIONFEE_IN + TUITIONFEE_OUT, data=dataTrain, b0=0, B0=0, mcmc = 100000)

stats <- summary(model1.posterior)
print(stats)
plot(model1.posterior)

# Let the mean of each parameters be my estimate of coefficients (NOTE MAY CHANGE THIS)
thetas <- stats$statistics[,1]

dataTest$RESULTS <- t(sigmoid(thetas = thetas,x = dataTest[,c("TUITIONFEE_IN","TUITIONFEE_OUT")]))

dataTest$RESULTS2 <- ifelse(dataTest$RESULTS > .5,1,0)
misClasificError <- mean(dataTest$RESULTS2 != dataTest$CONTROL2)
print(paste("Accuracy:", 1-misClasificError))
table(dataTest$RESULTS2, dataTest$CONTROL2, dnn=c("Predicted","Actual"))

# General 2d plot 
# pBayes <- ggplot(data=dataTest) + geom_point(aes(x=TUITIONFEE_IN, y=TUITIONFEE_OUT, col=CONTROL3)) + ggtitle("Tuition Costs for Public vs. Private Universities") + geom_line(data=tmpData,aes(x=tuitIn, y=tuitOut),linetype=3) + labs(x="In-state tuition", y="Out-of-state tuition", color="Control")

tuitIn <- seq(-.3,.6,.01)
tuitOut <- (-thetas[1] - thetas[2]*tuitIn)/thetas[3]
tmpData <- data.frame(tuitIn = tuitIn, tuitOut = tuitOut)

pBayes <- geom_line(data=tmpData,aes(x=tuitIn, y=tuitOut),linetype=4,col="Green")
pVanilla + pBayes


pr <- prediction(dataTest$RESULTS, dataTest$CONTROL2)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, main="ROC Curve for Tuition vs. Control")

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
print(paste("AUC:",auc))

perf1 <- performance(pr, "prec", "rec")
plot(perf1, main= "Precision-Recall Curve for Tuition vs. Control")

model.results <- predict(model, newdata=dataValidate, type='response')
dataValidate$RESULTS <- model.results
print(paste("AIC:",calcAIC(dataValidate$RESULTS, dataValidate$CONTROL2, 3)))

##############################
# informative and good
dataTrain <- read.csv("trainingData2012-13.csv")
dataValidate <- read.csv("validationData2012-13.csv")
dataTest <- read.csv("testingData2012-13.csv")

dataTrain$CONTROL2 <- ifelse(dataTrain$CONTROL==1,1,0)
dataTrain$CONTROL3 <- factor(dataTrain$CONTROL2, labels=c("Private","Public"))
dataValidate$CONTROL2 <- ifelse(dataValidate$CONTROL==1,1,0)
dataValidate$CONTROL3 <- factor(dataValidate$CONTROL2, labels=c("Private","Public"))
dataTest$CONTROL2 <- ifelse(dataTest$CONTROL==1,1,0)
dataTest$CONTROL3 <- factor(dataTest$CONTROL2, labels=c("Private","Public"))

# Uniform Prior, bo is mean, BO is variance of gaussian priors
model1.posterior <- MCMClogit(CONTROL2 ~ TUITIONFEE_IN + TUITIONFEE_OUT, data=dataTrain, b0=c(0,-.75,.5), B0=diag(c(1,.1,.1)), mcmc=100000)

stats <- summary(model1.posterior)
print(stats)
plot(model1.posterior)

# Let the mean of each parameters be my estimate of coefficients (NOTE MAY CHANGE THIS)
thetas <- stats$statistics[,1]

dataTest$RESULTS <- t(sigmoid(thetas = thetas,x = dataTest[,c("TUITIONFEE_IN","TUITIONFEE_OUT")]))

dataTest$RESULTS2 <- ifelse(dataTest$RESULTS > .5,1,0)
misClasificError <- mean(dataTest$RESULTS2 != dataTest$CONTROL2)
print(paste("Accuracy:", 1-misClasificError))
table(dataTest$RESULTS2, dataTest$CONTROL2, dnn=c("Predicted","Actual"))

# General 2d plot 
tuitIn <- seq(-.3,.6,.01)
tuitOut <- (-thetas[1] - thetas[2]*tuitIn)/thetas[3]
tmpData <- data.frame(tuitIn = tuitIn, tuitOut = tuitOut)

pBayes2 <- geom_line(data=tmpData,aes(x=tuitIn, y=tuitOut),linetype=5,col="Brown")
pVanilla + pBayes + pBayes2


pr <- prediction(dataTest$RESULTS, dataTest$CONTROL2)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, main="ROC Curve for Tuition vs. Control")

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
print(paste("AUC:",auc))

perf1 <- performance(pr, "prec", "rec")
plot(perf1, main= "Precision-Recall Curve for Tuition vs. Control")

model.results <- predict(model, newdata=dataValidate, type='response')
dataValidate$RESULTS <- model.results
print(paste("AIC:",calcAIC(dataValidate$RESULTS, dataValidate$CONTROL2, 3)))

##############################
# informative and bad
dataTrain <- read.csv("trainingData2012-13.csv")
dataValidate <- read.csv("validationData2012-13.csv")
dataTest <- read.csv("testingData2012-13.csv")

dataTrain$CONTROL2 <- ifelse(dataTrain$CONTROL==1,1,0)
dataTrain$CONTROL3 <- factor(dataTrain$CONTROL2, labels=c("Private","Public"))
dataValidate$CONTROL2 <- ifelse(dataValidate$CONTROL==1,1,0)
dataValidate$CONTROL3 <- factor(dataValidate$CONTROL2, labels=c("Private","Public"))
dataTest$CONTROL2 <- ifelse(dataTest$CONTROL==1,1,0)
dataTest$CONTROL3 <- factor(dataTest$CONTROL2, labels=c("Private","Public"))

# Uniform Prior, bo is mean, BO is variance of gaussian priors
model1.posterior <- MCMClogit(CONTROL2 ~ TUITIONFEE_IN + TUITIONFEE_OUT, data=dataTrain, b0=c(1,5,-5), B0=diag(c(1,10,10)), mcmc=100000)

stats <- summary(model1.posterior)
print(stats)
plot(model1.posterior)

# Let the mean of each parameters be my estimate of coefficients (NOTE MAY CHANGE THIS)
thetas <- stats$statistics[,1]

dataTest$RESULTS <- t(sigmoid(thetas = thetas,x = dataTest[,c("TUITIONFEE_IN","TUITIONFEE_OUT")]))

dataTest$RESULTS2 <- ifelse(dataTest$RESULTS > .5,1,0)
misClasificError <- mean(dataTest$RESULTS2 != dataTest$CONTROL2)
print(paste("Accuracy:", 1-misClasificError))
table(dataTest$RESULTS2, dataTest$CONTROL2, dnn=c("Predicted","Actual"))

# General 2d plot 
tuitIn <- seq(-.3,.6,.01)
tuitOut <- (-thetas[1] - thetas[2]*tuitIn)/thetas[3]
tmpData <- data.frame(tuitIn = tuitIn, tuitOut = tuitOut)

pBayes3 <- geom_line(data=tmpData,aes(x=tuitIn, y=tuitOut),linetype=5,col="Cyan")
pVanilla + pBayes + pBayes2 + pBayes3

pr <- prediction(dataTest$RESULTS, dataTest$CONTROL2)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, main="ROC Curve for Tuition vs. Control")

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
print(paste("AUC:",auc))

perf1 <- performance(pr, "prec", "rec")
plot(perf1, main= "Precision-Recall Curve for Tuition vs. Control")

model.results <- predict(model, newdata=dataValidate, type='response')
dataValidate$RESULTS <- model.results
print(paste("AIC:",calcAIC(dataValidate$RESULTS, dataValidate$CONTROL2, 3)))

########################################
# Explore other features
dataTrain <- read.csv("trainingData2012-13.csv")
dataValidate <- read.csv("validationData2012-13.csv")
dataTest <- read.csv("testingData2012-13.csv")

dataTrain$CONTROL2 <- ifelse(dataTrain$CONTROL==1,1,0)
dataTrain$CONTROL3 <- factor(dataTrain$CONTROL2, labels=c("Private","Public"))
dataValidate$CONTROL2 <- ifelse(dataValidate$CONTROL==1,1,0)
dataValidate$CONTROL3 <- factor(dataValidate$CONTROL2, labels=c("Private","Public"))
dataTest$CONTROL2 <- ifelse(dataTest$CONTROL==1,1,0)
dataTest$CONTROL3 <- factor(dataTest$CONTROL2, labels=c("Private","Public"))

features <- c("UGDS","AVGFACSAL","C150_4","COMPL_RPY_5YR_RT", "PAR_ED_PCT_1STGEN", "FAMINC", "DEBT_MDN", "GRAD_DEBT_MDN","WDRAW_DEBT_MDN","CDR2", "MN_EARN_WNE_P10")
 
N <- as.list(1:length(features))
COMB <- sapply(N, function(m) combn(x=features, m))
COMB2 <- list()
k=0
for(i in seq(COMB)){
  tmp <- COMB[[i]]
  for(j in seq(ncol(tmp))){
    k <- k + 1
    COMB2[[k]] <- formula(paste("CONTROL2", "~", paste(tmp[,j], collapse=" + ")))
  }
}

names <- list()
equation <- list()
aics <- list()
accs <- list()
fCount <- list()

for(i in seq(COMB2)){
  numFeatures <- length(attr(terms(COMB2[[i]]), "term.labels"))+1 #+1 for intercept
  fCount[[i]] <- numFeatures
  model <- glm(COMB2[[i]],family=binomial(link='logit'),data=dataTrain)
  # tmp <- summary(model)
  # print(tmp)
  
  # anova(model, test="Chisq")
  # pR2(model)
  model.results <- predict(model, newdata=dataValidate, type='response')
  dataValidate$RESULTS <- model.results
  model.results <- ifelse(model.results > .5,1,0)
  misClasificError <- mean(model.results != dataValidate$CONTROL2)
  # print(paste("Accuracy:", 1-misClasificError))
  # table(model.results, dataTest$CONTROL2, dnn=c("Predicted","Actual"))
  
  names[[i]] <- paste(attr(terms(COMB2[[i]]), "term.labels"),collapse=" ")
  equation <- append(equation, COMB2[[i]])
  accs[[i]] <- 1-misClasificError
  aic <- calcAIC(dataValidate$RESULTS, dataValidate$CONTROL2, numFeatures)
  aics[[i]] <- aic
  
  
  if((1-misClasificError) > .88) {
    print(COMB2[[i]])
    print(i)
    print(paste("Accuracy:", 1-misClasificError))
    print(table(model.results, dataValidate$CONTROL2, dnn=c("Predicted","Actual")))
    print(paste("AIC:",aic))
  }
  
  # print(i)
}

df1 <- data.frame(matrix(unlist(accs), nrow=length(accs), byrow=T),stringsAsFactors=FALSE)
df2 <- data.frame(matrix(unlist(aics), nrow=length(aics), byrow=T),stringsAsFactors=FALSE)
df3 <- data.frame(matrix(unlist(names), nrow=length(names), byrow=T),stringsAsFactors=FALSE)
df4 <- data.frame(matrix(unlist(fCount), nrow=length(fCount), byrow=T),stringsAsFactors=FALSE)
df5 <- data.frame(matrix(unlist(equation), nrow=length(equation), byrow=T),stringsAsFactors=FALSE)
# df <- merge.data.frame()
df <- cbind(df1,df2,df3, df4, df5)
names(df) <- c("ACCURACY", "AIC","NAMES", "NUMPARAMS", "EQUATION")
# Sort by ascending AIC
df <- df[order(df$AIC),]

# examine best 5 closer
print(df[1:5,"NAMES"])

# examine worst 5 closer
print(df[(nrow(df)-4):nrow(df),"NAMES"])

# Best 5
performAnalysis(df[1,"EQUATION"][[1]])
performAnalysis(df[2,"EQUATION"][[1]])
performAnalysis(df[3,"EQUATION"][[1]])
performAnalysis(df[4,"EQUATION"][[1]])
performAnalysis(df[5,"EQUATION"][[1]])

# Worst 5
performAnalysis(df[nrow(df),"EQUATION"][[1]])
performAnalysis(df[nrow(df)-1,"EQUATION"][[1]])
performAnalysis(df[nrow(df)-2,"EQUATION"][[1]])
performAnalysis(df[nrow(df)-3,"EQUATION"][[1]])
performAnalysis(df[nrow(df)-4,"EQUATION"][[1]])

# Best 5 Bayes Uniformative
performAnalysisBayes(df[1,"EQUATION"][[1]],0,0, strsplit(df$NAMES[1], split=" ")[[1]])
performAnalysisBayes(df[2,"EQUATION"][[1]],0,0, strsplit(df$NAMES[2], split=" ")[[1]])
performAnalysisBayes(df[3,"EQUATION"][[1]],0,0, strsplit(df$NAMES[3], split=" ")[[1]])
performAnalysisBayes(df[4,"EQUATION"][[1]],0,0, strsplit(df$NAMES[4], split=" ")[[1]])
performAnalysisBayes(df[5,"EQUATION"][[1]],0,0, strsplit(df$NAMES[5], split=" ")[[1]])


# Worst 5 Bayes Uniformative
performAnalysisBayes(df[nrow(df),"EQUATION"][[1]],0,0, strsplit(df$NAMES[nrow(df)], split=" ")[[1]])
performAnalysisBayes(df[nrow(df)-1,"EQUATION"][[1]],0,0, strsplit(df$NAMES[nrow(df)-1], split=" ")[[1]])
performAnalysisBayes(df[nrow(df)-2,"EQUATION"][[1]],0,0, strsplit(df$NAMES[nrow(df)-2], split=" ")[[1]])
performAnalysisBayes(df[nrow(df)-3,"EQUATION"][[1]],0,0, strsplit(df$NAMES[nrow(df)-3], split=" ")[[1]])
performAnalysisBayes(df[nrow(df)-4,"EQUATION"][[1]],0,0, strsplit(df$NAMES[nrow(df)-4], split=" ")[[1]])

# Best 5 Bayes Informative
performAnalysisBayes(df[1,"EQUATION"][[1]],c(0,1,-1, -1, 1, 1, -1, -1),diag(c(.001,1,1,1,1,1,1,1)), strsplit(df$NAMES[1], split=" ")[[1]])
performAnalysisBayes(df[1,"EQUATION"][[1]],c(0,1,-1, -1, 1, 1, -1, -1),diag(c(.001,10,10,10,10,10,10,10)), strsplit(df$NAMES[1], split=" ")[[1]])
performAnalysisBayes(df[1,"EQUATION"][[1]],c(0,1,-1, -1, 1, 1, -1, -1),diag(c(.001,.1,.1,.1,.1,.1,.1,.1)), strsplit(df$NAMES[1], split=" ")[[1]])

performAnalysisBayes(df[2,"EQUATION"][[1]],c(0,1,-1, -1, 1, -1, -1),diag(c(.001,1,1,1,1,1,1)), strsplit(df$NAMES[2], split=" ")[[1]])
performAnalysisBayes(df[2,"EQUATION"][[1]],c(0,1,-1, -1, 1, -1, -1),diag(c(.001,10,10,10,10,10,10)), strsplit(df$NAMES[2], split=" ")[[1]])
performAnalysisBayes(df[2,"EQUATION"][[1]],c(0,1,-1, -1, 1, -1, -1),diag(c(.001,.1,.1,.1,.1,.1,.1)), strsplit(df$NAMES[2], split=" ")[[1]])

# Worst 5 Bayes Informative
performAnalysisBayes(df[nrow(df),"EQUATION"][[1]],c(0, 1, -1, -1, -1),c(.001,1,1,1,1), strsplit(df$NAMES[nrow(df)], split=" ")[[1]])
performAnalysisBayes(df[nrow(df),"EQUATION"][[1]],c(0, 1, -1, -1, -1),c(.001,10,10,10,10), strsplit(df$NAMES[nrow(df)], split=" ")[[1]])
performAnalysisBayes(df[nrow(df),"EQUATION"][[1]],c(0, 1, -1, -1, -1),c(.001,.1,.1,.1,.1), strsplit(df$NAMES[nrow(df)], split=" ")[[1]])

performAnalysisBayes(df[nrow(df)-1,"EQUATION"][[1]],c(0,-1,-1,-1),c(.001,1,1,1), strsplit(df$NAMES[nrow(df)-1], split=" ")[[1]])
performAnalysisBayes(df[nrow(df)-1,"EQUATION"][[1]],c(0,-1,-1,-1),c(.001,10,10,10), strsplit(df$NAMES[nrow(df)-1], split=" ")[[1]])
performAnalysisBayes(df[nrow(df)-1,"EQUATION"][[1]],c(0,-1,-1,-1),c(.001,.1,.1,.1), strsplit(df$NAMES[nrow(df)-1], split=" ")[[1]])

###################
# Analysis on REGION
target <- "REGION"
target2 <- 1
labels <- c("Other", "New England")


dataTrain <- read.csv("trainingData2012-13.csv")
dataValidate <- read.csv("validationData2012-13.csv")
dataTest <- read.csv("testingData2012-13.csv")

if(target == "CONTROL") {
  
} else if(target =="REGION") {
  dataTrain <- read.csv("trainingData2012-13REGION.csv")
  dataValidate <- read.csv("validationData2012-13REGION.csv")
  dataTest <- read.csv("testingData2012-13REGION.csv")
  
  
  # tmpInit <- paste(dataScaled$INSTNM)
  # tmpTrain <- paste(dataTrain$INSTNM)
  # tmpValidate <- paste(dataValidate$INSTNM)
  # tmpTest <- paste(dataTest$INSTNM)
  # 
  # trainRows <- unlist(sapply(1:nrow(dataTrain), function(row) {
  #   which((tmpInit == tmpTrain[row]) & (abs(dataScaled$UGDS-dataTrain$UGDS[row]) < 10^-8))
  # }))
  # validateRows <- unlist(sapply(1:nrow(dataValidate), function(row) {
  #   which((tmpInit == tmpValidate[row]) & (abs(dataScaled$UGDS-dataValidate$UGDS[row]) < 10^-8))
  # }))
  # testRows <- unlist(sapply(1:nrow(dataTest), function(row) {
  #   which((tmpInit == tmpTest[row]) & (abs(dataScaled$UGDS-dataTest$UGDS[row]) < 10^-8))
  # }))
  
  # dataTrain$REGION <- dataScaled$REGION[trainRows]
  # dataValidate$REGION <- dataScaled$REGION[validateRows]
  # dataTest$REGION <- dataScaled$REGION[testRows]
  
  dataTrain$CONTROL = dataTrain$REGION
  dataValidate$CONTROL = dataValidate$REGION
  dataTest$CONTROL = dataTest$REGION
} 

dataTrain$CONTROL2 <- ifelse(dataTrain$CONTROL==target2,1,0)
dataTrain$CONTROL3 <- factor(dataTrain$CONTROL2, labels=labels)
dataValidate$CONTROL2 <- ifelse(dataValidate$CONTROL==target2,1,0)
dataValidate$CONTROL3 <- factor(dataValidate$CONTROL2, labels=labels)
dataTest$CONTROL2 <- ifelse(dataTest$CONTROL==target2,1,0)
dataTest$CONTROL3 <- factor(dataTest$CONTROL2, labels=labels)


features <- c("UGDS", "TUITIONFEE_IN", "TUITIONFEE_OUT","AVGFACSAL","C150_4","COMPL_RPY_5YR_RT" , "FAMINC", "GRAD_DEBT_MDN","WDRAW_DEBT_MDN","CDR2", "MN_EARN_WNE_P10")
N <- as.list(1:length(features))
COMB <- sapply(N, function(m) combn(x=features, m))
COMB2 <- list()
k=0
for(i in seq(COMB)){
  tmp <- COMB[[i]]
  for(j in seq(ncol(tmp))){
    k <- k + 1
    COMB2[[k]] <- formula(paste("CONTROL2", "~", paste(tmp[,j], collapse=" + ")))
  }
}

names <- list()
equation <- list()
aics <- list()
accs <- list()
fCount <- list()

for(i in seq(COMB2)){
  numFeatures <- length(attr(terms(COMB2[[i]]), "term.labels"))+1 #+1 for intercept
  fCount[[i]] <- numFeatures
  model <- glm(COMB2[[i]],family=binomial(link='logit'),data=dataTrain)
  # tmp <- summary(model)
  # print(tmp)
  
  # anova(model, test="Chisq")
  # pR2(model)
  model.results <- predict(model, newdata=dataValidate, type='response')
  dataValidate$RESULTS <- model.results
  model.results <- ifelse(model.results > .5,1,0)
  misClasificError <- mean(model.results != dataValidate$CONTROL2)
  # print(paste("Accuracy:", 1-misClasificError))
  # table(model.results, dataTest$CONTROL2, dnn=c("Predicted","Actual"))
  
  names[[i]] <- paste(attr(terms(COMB2[[i]]), "term.labels"),collapse=" ")
  equation <- append(equation, COMB2[[i]])
  accs[[i]] <- 1-misClasificError
  aic <- calcAIC(dataValidate$RESULTS, dataValidate$CONTROL2, numFeatures)
  aics[[i]] <- aic
  
  
  if((1-misClasificError) > .999) {
    print(COMB2[[i]])
    print(i)
    print(paste("Accuracy:", 1-misClasificError))
    print(table(model.results, dataValidate$CONTROL2, dnn=c("Predicted","Actual")))
    print(paste("AIC:",aic))
  }
  
  # print(i)
}

df1 <- data.frame(matrix(unlist(accs), nrow=length(accs), byrow=T),stringsAsFactors=FALSE)
df2 <- data.frame(matrix(unlist(aics), nrow=length(aics), byrow=T),stringsAsFactors=FALSE)
df3 <- data.frame(matrix(unlist(names), nrow=length(names), byrow=T),stringsAsFactors=FALSE)
df4 <- data.frame(matrix(unlist(fCount), nrow=length(fCount), byrow=T),stringsAsFactors=FALSE)
df5 <- data.frame(matrix(unlist(equation), nrow=length(equation), byrow=T),stringsAsFactors=FALSE)
# df <- merge.data.frame()
df <- cbind(df1,df2,df3, df4, df5)
names(df) <- c("ACCURACY", "AIC","NAMES", "NUMPARAMS", "EQUATION")
# Sort by ascending AIC
df <- df[order(df$AIC),]

# examine best 5 closer
print(df[1:5,"NAMES"])

# examine worst 5 closer
print(df[(nrow(df)-4):nrow(df),"NAMES"])


performAnalysis(df[1,"EQUATION"][[1]],target = "REGION", labels=labels, target2 = target2)
performAnalysis(df[2,"EQUATION"][[1]],target = "REGION", labels=labels, target2 = target2)
performAnalysis(df[3,"EQUATION"][[1]],target = "REGION", labels=labels, target2 = target2)
performAnalysis(df[4,"EQUATION"][[1]],target = "REGION", labels=labels, target2 = target2)
performAnalysis(df[5,"EQUATION"][[1]],target = "REGION", labels=labels, target2 = target2)

performAnalysis(df[nrow(df),"EQUATION"][[1]],target = "REGION", labels=labels, target2 = target2)
performAnalysis(df[nrow(df)-1,"EQUATION"][[1]],target = "REGION", labels=labels, target2 = target2)
performAnalysis(df[nrow(df)-2,"EQUATION"][[1]],target = "REGION", labels=labels, target2 = target2)
performAnalysis(df[nrow(df)-3,"EQUATION"][[1]],target = "REGION", labels=labels, target2 = target2)
performAnalysis(df[nrow(df)-4,"EQUATION"][[1]],target = "REGION", labels=labels, target2 = target2)




##
# Outlying plots

# 1d plots for each variable
p1 <- ggplot(data=dataTest) + geom_point(size=1, aes(x=TUITIONFEE_OUT, y=CONTROL2, col=CONTROL3)) + labs(x="Out-State Tuition", y="Region", color="Region")
p2 <- ggplot(data=dataTest) + geom_point(size=1, aes(x=TUITIONFEE_OUT, y=RESULTS, col=CONTROL3),shape=4) + geom_abline(slope=0,intercept=.5,linetype=2) + labs(x="Out-State Tuition", y="Region", color="Region")
grid.arrange(p1, p2)

p1 <- ggplot(data=dataTest) + geom_point(size=1, aes(x=AVGFACSAL, y=CONTROL2, col=CONTROL3)) + labs(x="Average Faculty Salary", y="Region", color="Region")
p2 <- ggplot(data=dataTest) + geom_point(size=1, aes(x=TUITIONFEE_OUT, y=RESULTS, col=CONTROL3),shape=4) + geom_abline(slope=0,intercept=.5,linetype=2) + labs(x="Average Faculty Salary", y="Region", color="Region")
grid.arrange(p1, p2)

# Generate a 2d plot 1/2 decision boundary
tuitIn <- seq(-.5,.7,.01)
tuitOut <- (-model$coefficients[1] - model$coefficients[2]*tuitIn)/model$coefficients[3]
tmpData <- data.frame(tuitIn = tuitIn, tuitOut = tuitOut)

# General 2d plot 
ggplot(data=dataTest) + geom_point(aes(x=TUITIONFEE_OUT, y=AVGFACSAL, col=CONTROL3)) + ggtitle("Outlying versus Other") + geom_line(data=tmpData,aes(x=tuitIn, y=tuitOut),linetype=3) + labs(x="Out-state tuition", y="Average Faculty Salary", color="Region")


############################################
# Multiclass logistic regression
# library(nnet)
# mod <- multinom(y ~ x1 + x2, df1)


#######################################################
# # Uniform Prior, bo is mean, BO is variance of gaussian priors
# model2 <- MCMClogit(CONTROL~TUITIONFEE_OUT, data=dataTrain, b0=0, B0=0.01,marginal.likelihood="Laplace")
# 
# # Uniform Prior, bo is mean, BO is variance of gaussian priors
# model3 <- MCMClogit(CONTROL~TUITIONFEE_IN + TUITIONFEE_OUT, data=dataTrain, b0=0, B0=0.01,marginal.likelihood="Laplace")
# 
# bf <- BayesFactor(model1,model2,model3)
# 
# bf.probs <- PostProbMod(bf)
# print(bf.probs)

## MCMCmnl multinomial logistic regression


#########################################
## Additional analysis on random things
# # Count number of columns non-na rows for each feature
# nonNaCounts <- sapply(3:(length(features)+2), function(col) {
#   sum(!is.na(dataTest[,col]))
# })
# 
# tmp <- data.frame(features,nonNaCounts)
# tmp

# # Count Class imbalance
# table(dataTrain$CONTROL2)
# table(dataTest$CONTROL2)

# partitionDataToFile(dataScaled)

# Undo feature scaling

# dataTrain <- read.csv("trainingData2012-13.csv")
# dataValidate <- read.csv("validationData2012-13.csv")
# dataTest <- read.csv("testingData2012-13.csv")
# 
# dataUndoTrain <- dataTrain
# dataUndoValidate <- dataValidate
# dataUndoTest <- dataTest
# 
# dataUndoTrain[,features] <- sapply(features, function(x) {
#   tmp <- (max(dataSubset[,x]) - min(dataSubset[,x])) * dataTrain[,x]
#   tmp + mean(dataSubset[,x])
# })
# 
# dataUndoValidate[,features] <- sapply(features, function(x) {
#   tmp <- (max(dataSubset[,x]) - min(dataSubset[,x])) * dataValidate[,x]
#   tmp + mean(dataSubset[,x])
# })
# 
# dataUndoTest[,features] <- sapply(features, function(x) {
#   tmp <- (max(dataSubset[,x]) - min(dataSubset[,x])) * dataTest[,x]
#   tmp + mean(dataSubset[,x])
# })
# 
# write.table(dataUndoTrain,file="trainingData2012-13Unscaled.csv", sep=",", row.names=F)
# write.table(dataUndoValidate,file="validationData2012-13Unscaled.csv", sep=",", row.names=F)
# write.table(dataUndoTest,file="testingData2012-13Unscaled.csv", sep=",", row.names=F)
