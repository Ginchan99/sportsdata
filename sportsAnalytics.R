#clear all environment variables
rm(list=ls())


#let us load the library files
#External function
printStats <- function (m, with.cx=TRUE) {
  if (class(m) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(m)$fstatistic
  p <- pf(f[1], f[2], f[3], lower.tail=FALSE)
  attributes(p) <- NULL
  
  fml <- as.character(formula(m))
  cx <- summary(m)$coeff
  stars <- rep(" ", nrow(cx))
  stars[cx[,4] <= 0.1] <- "."
  stars[cx[,4] <= 0.05] <- "*"
  stars[cx[,4] <= 0.01] <- "**"
  stars[cx[,4] <= 0.001] <- "***"
  cat("MODEL        : ", sprintf("%s", paste(fml[c(2,1,3)], sep=" ", collapse=" ")), "\n", sep="")
  cat("SUMMARY STATS: ")
  cat("R^2 = ",sprintf("%6.4f",summary(m)$r.squared), 
      "  (adj. = ", sprintf("%6.4f",summary(m)$adj.r.squared), ")", sep="")
  cat("\n")
  cat("               ")
  cat("F-stats: ", sprintf("%.3f",f[1]), " on ", f[2], " and ", f[3], " DF,  p-value: ", p, "\n", sep="")
  if( with.cx ) {
    cat("\n")
    print(cbind(format(cx[,c(1,2,4)], scientific=TRUE, justify="right", digits=5), Signif=stars), 
          quote=FALSE, print.gap=3)
  }
}

printStats.cpt <- function (m, with.cx=TRUE) {
  if (class(m) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(m)$fstatistic
  p <- pf(f[1], f[2], f[3], lower.tail=FALSE)
  attributes(p) <- NULL
  
  fml <- as.character(formula(m))
  cx <- summary(m)$coeff
  stars <- rep(" ", nrow(cx))
  stars[cx[,4] <= 0.1] <- "."
  stars[cx[,4] <= 0.05] <- "*"
  stars[cx[,4] <= 0.01] <- "**"
  stars[cx[,4] <= 0.001] <- "***"
  cat("MODEL : ", sprintf("%s", paste(fml[c(2,1,3)], sep=" ", collapse=" ")), "\n", sep="")
  cat("      : ")
  cat("adj. R^2 = ", sprintf("%6.4f",summary(m)$adj.r.squared), 
      " /  F-stats: ", sprintf("%.3f",f[1]), " on ", f[2],",", f[3], " Df,  p-value: ", p, "\n", sep="")
  if( with.cx ) {
    print(cbind(format(cx[,c(1,2,4)], scientific=TRUE, justify="right", digits=5), Signif=stars), 
          quote=FALSE, print.gap=3)
    cat("\n")
  }
}
library("dplyr")
library("magrittr")
library("ggplot2")

# install.packages("dplyr")
# install.packages("ggplot2")
#load the data
sportsData = read.csv("baseball.csv")
summary(sportsData)
trainsubset = subset(sportsData,Year<=2007)
testsubset = subset(sportsData,Year>2007)

# create a smaller subset with 2 constraints
sports96to2001 = subset(sportsData,Year>=1996 & Year<=2001)

# create good ggplots!
ggplot(data = sports96to2001, aes(x = W, y = Team)) + theme_bw() +
    scale_color_manual(values = c("grey", "red3")) + 
    geom_vline(xintercept = c(85.0, 95.0), col = "green2", linetype = "longdash") +
    geom_point(aes(color = factor(Playoffs)), pch = 16, size = 3.0) 

ggplot(data = sports96to2001, aes(x = RA, y = W)) + theme_bw() +  
scale_color_manual(values = c("grey", "red3")) + geom_hline(yintercept= c(85.0, 95.0), col = "green2", linetype = "longdash") +  
geom_point(aes(color = factor(Playoffs)), alpha = 0.5,pch = 16, size = 3.0) 
#data is the subset we want to look at
# aes is aesthetic that is defining x & y columns
# c() goes as a list, geom_vline is drawing a vertical line
# geom_points is points present on the graph
# pch is size that is either larger or smaller

#Create an LM = linear model
Wins_Regression <- lm(W ~ RA, data = trainsubset)
summary(Wins_Regression)

#create a model for wins
Wins_Regression <- lm(W ~ difference, data = trainsubset)
summary(Wins_Regression)

#create a model for runs record
RunScoredModel = lm(RS ~ OBP+SLG+BA, data = trainsubset)
summary(RunScoredModel)

 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -804.64      18.70 -43.026   <2e-16 ***
#   OBP          2894.10     103.47  27.969   <2e-16 ***
#   SLG          1553.57      41.13  37.770   <2e-16 ***
#   BA           -156.53     121.99  -1.283      0.2    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 25.14 on 1078 degrees of freedom
# Multiple R-squared:  0.928,	Adjusted R-squared:  0.9278 
# F-statistic:  4628 on 3 and 1078 DF,  p-value: < 2.2e-16

RunScoredModel2 = lm(RS ~ OBP+SLG, data = trainsubset)
summary(RunScoredModel2)
printStats(RunScoredModel2)
#runs scored = -811.70 + 2816.77 OBP + 1532.58*SLG
trainsubset$PredictionOfRunsScored = -811.70 + 2816.77 *trainsubset$OBP + 1532.58*trainsubset$SLG

#how much error in train set?
trainsubset$error = abs(trainsubset$PredictionOfRunsScored - trainsubset$RS)
View(trainsubset)

#how much error in test set
testsubset$PredictionOfRunsScored = -811.70 + 2816.77 *testsubset$OBP + 1532.58*testsubset$SLG
testsubset$error = abs(testsubset$PredictionOfRunsScored - testsubset$RS)

#create a model for wins
trainsubset$difference = trainsubset$RS - trainsubset$RA
Wins_Regression <- lm(W ~ difference, data = trainsubset)
summary(Wins_Regression)
trainsubset$WinsPrediction = 8.0894e+01 + 1.0471e-01* trainsubset$difference
trainsubset$Winserror = abs(trainsubset$W - trainsubset$WinsPrediction)

#create a model for wins
testsubset$difference = testsubset$RS - testsubset$RA
Wins_Regression <- lm(W ~ difference, data = testsubset)
summary(Wins_Regression)

testsubset$WinsPrediction = 8.0894e+01 + 1.0471e-01* testsubset$difference
testsubset$Winserror = abs(testsubset$W - testsubset$WinsPrediction)


ggplot(data = sports96to2001, aes(x = RS, y = W)) + theme_bw() +  
  scale_color_manual(values = c("grey", "red3")) + geom_hline(yintercept= c(85.0, 95.0), col = "green2", linetype = "longdash") +  
  geom_point(aes(color = factor(Playoffs)), alpha = 0.5,pch = 16, size = 3.0)

sports96to2001$difference = sports96to2001$RS - sports96to2001$RA
ggplot(data = sports96to2001, aes(x = difference, y = W)) + theme_bw() +  
  scale_color_manual(values = c("grey", "red3")) + geom_hline(yintercept= c(85.0, 95.0), col = "green2", linetype = "longdash") +  
  geom_point(aes(color = factor(Playoffs)), alpha = 0.5,pch = 16, size = 3.0) 

