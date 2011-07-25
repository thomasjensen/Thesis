setwd("/Users/thomasjensen/Documents/Thesis/VoteChoice/Data/")

data <- read.csv("councilVotes.csv")
parlgov <- read.csv("view_cabinet.csv")

##RECODE YES AND POSITIVE STATEMENT TO YES
data$value[data$value == 6] <- 0

##SUBSET THE DATA TO CONTROVERSIAL ISSUES
data <- data[data$bpoint >= 1 & data$DefinitiveAct == 1, ]

##ONLY CODE FOR PRIME MINISTER CHANGES FOR CABINETS

##GET DAYS IN GOVERNMENT
parlgov$start_date <- as.Date(parlgov$start_date, format = "%Y-%m-%d")
parlgov$election_date <- as.Date(parlgov$election_date, format = "%Y-%m-%d")

cabinets <- data.frame()
count <- 1

for (id in unique(data$cabinetid)){
  start <- parlgov[parlgov$cabinet_id == id,"start_date"]
  end <- parlgov[parlgov$previous_cabinet_id == id, "start_date"]
  end <- end[!is.na(end)][1]
  start <- start[!is.na(start)][1]
  if (is.na(end)){
    difference <- as.numeric(as.Date("2009-12-31", format = "%Y-%m-%d") - start)
  } else {
    difference <- as.numeric(end - start)
  }
  cabinets[count,"ID"] <- id
  cabinets[count,"TimeInGovernment"] <- difference
  count = count + 1
}

data <- merge(data, cabinets, by.x = "cabinetid", by.y = "ID")

##CHANGE LABELS FOR THE POLICYAREA VARIABLE
levels(data$policyarea) <- c("Agri/Pech","Comp.","Ecofin","Education/Culture","Employment/Consumer","Environment","Gen. Affairs","JHA","None","Trans./Energy")

##EXAMINE THE DATA AND CREATE PLOTS
library(plyr)
library(ggplot2)
library(lattice)
library(xtable)

##create summary of the dependent variable
library(tikzDevice)

depvar <- ddply(data, "policyarea", function(x) data.frame(Yes = sum(x$value == 0), No = sum(x$value == 1 | x$value == 4), Abs = sum(x$value == 2 | x$value == 5), yesNeg = sum(x$value == 5)))

depvar.m <- as.matrix(depvar[,3:5])
rownames(depvar.m) <- depvar$policyarea

tikz("depvar.tex", width = 7, height = 7)
dotplot(t(depvar.m), groups = FALSE, type = c("p","h"))
dev.off()

##create scatterplots for left-right deviance

cabinets <- data[, c("policyarea","newold","Year","value","cabms","TimeInGovernment","left_right","meanLeftRight")]

tableCab <- ddply(cabinets, c("cabms","newold"), function(x) data.frame(abs = sum(x$value == 2 | x$value == 4), no = sum(x$value == 1 | x$value == 3), yesNeg = sum(x$value == 5), rawDissent = sum(x$value != 0), time = unique(x$TimeInGovernment), LR = unique(x$left_right), meanlr = mean(x$meanLeftRight), deviance = abs(unique(x$left_right) - mean(x$meanLeftRight))))

library(lattice)

tikz("scatter1.tex", width = 5, height = 3.5)
xyplot(tableCab$abs ~ tableCab$deviance | factor(tableCab$newold), type = c("g", "p", "r"), xlab = "Deviance", ylab = "Abstentions")
dev.off()

tikz("scatter2.tex", width = 5, height = 3.5)
xyplot(tableCab$no ~ tableCab$deviance | factor(tableCab$newold), type = c("g","p","r"), xlab = "Deviance", ylab = "No Votes")
dev.off()

tikz("scatter3.tex", width = 5, height = 3.5)
xyplot(tableCab$yesNeg ~ tableCab$deviance | factor(tableCab$newold), type = c("g","p","r"),xlab = "Deviance", ylab = "Yes + Dissent")
dev.off()

polVote <- ddply(cabinets, c("cabms","policyarea","newold"), function(x) data.frame(abs = sum(x$value == 2 | x$value == 4), no = sum(x$value == 1 | x$value == 3), yesNeg = sum(x$value == 5), rawDissent = sum(x$value != 0), time = unique(x$TimeInGovernment), LR = unique(x$left_right), meanlr = mean(x$meanLeftRight), deviance = abs(unique(x$left_right) - mean(x$meanLeftRight))))

tikz("scattermatrix1.tex", width = 7, height = 7)
xyplot(polVote$abs ~ polVote$deviance | factor(polVote$policyarea), groups = polVote$newold, pch = 21, type = c("g", "p", "r"), col = c("red","blue"), fill = c("red","blue") , xlab = "Deviance", ylab = "Abstentions")
dev.off()

tikz("scattermatrix2.tex", width = 7, height = 7)
xyplot(polVote$no ~ polVote$deviance | factor(polVote$policyarea), groups = polVote$newold, pch = 21, type = c("g", "p", "r"), col = c("red","blue"), fill = c("red","blue") , xlab = "Deviance", ylab = "No Votes")
dev.off()

tikz("scattermatrix3.tex", width = 7, height = 7)
xyplot(polVote$yesNeg ~ polVote$deviance | factor(polVote$policyarea), groups = polVote$newold, pch = 21, type = c("g", "p", "r"), col = c("red","blue"), fill = c("red","blue") , xlab = "Deviance", ylab = "Yes + Dissent")
dev.off()

##create scatterplots for meeting frequency
meetFreq <- ddply(data, c("cabms","newold"), function(x) data.frame(abs = sum(x$value == 2 | x$value == 4), no = sum(x$value == 1 | x$value == 3), yesNeg = sum(x$value == 5), meet = sum(x$MeetingFreq), meetAbs = sum(x$value == 2 | x$value == 4)/sum(x$MeetingFreq), meetNo = sum(x$value == 1 | x$value == 3)/sum(x$MeetingFreq), meetYesNeg = sum(x$value == 5)/sum(x$MeetingFreq)))

catMeet <- vector()
for (i in 1:nrow(meetFreq)){
  if (meetFreq$meet[i] < 500){
    catMeet[i] <- 1
  }
  if (meetFreq$meet[i] >= 500 & meetFreq$meet[i] < 1000){
    catMeet[i] <- 2
  }
  if (meetFreq$meet[i] >= 1000 & meetFreq$meet[i] < 1500){
    catMeet[i] <- 3
  }
  if (meetFreq$meet[i] >= 1500 & meetFreq$meet[i] < 2000){
    catMeet[i] <- 4
  }
  if (meetFreq$meet[i] >= 2000){
    catMeet[i] <- 5
  }
}

meetFreq$catMeet <- catMeet
meetFreq <- na.omit(meetFreq)
xyplot(meetFreq$meetAbs ~ meetFreq$meet, type = "h")
xyplot(meetFreq$meetNo ~ meetFreq$meet, type = "h")
xyplot(meetFreq$meetYesNeg ~ meetFreq$meet, type = "h")

tikz("histogramYesNeg.tex", width = 5, height = 3.5)
histogram(~ meetFreq$meetYesNeg | factor(meetFreq$catMeet), xlab = "Relative Voting Frequency", col = "grey", type = "percent")
dev.off()

tikz("histogramNo.tex", width = 5, height = 3.5)
histogram(~ meetFreq$meetNo | factor(meetFreq$catMeet), xlab = "Relative Voting Frequency", col = "grey", type = "percent")
dev.off()

tikz("histogramAbs.tex", width = 5, height = 3.5)
histogram(~ meetFreq$meetAbs | factor(meetFreq$catMeet), xlab = "Relative Voting Frequency", col = "grey", type = "percent")
dev.off()

histogram(~ meetFreq$meetNo | meetFreq$catMeet)
histogram(~ meetFreq$meetAbs | meetFreq$catMeet)

densityplot(~ meetFreq$no | meetFreq$catMeet)



###the presidency
PresDummy <- vector()

for (i in 1:nrow(data)){
  if (as.character(data$variable[i]) == as.character(data$Presidency[i])){
    PresDummy[i] <- 1
  } else {
    PresDummy[i] <- 0
  }
}

data$PresDummy <- PresDummy

tablePres <- data[, c("PresDummy","value","cabms")]
tablePres <- ddply(tablePres,"PresDummy", function(x) data.frame(dissent = (sum(x$value == 1) + sum(x$value == 2) + sum(x$value == 3) + sum(x$value == 4) + sum(x$value == 5))/nrow(x)))

tablePresUK <- data[data$cabms == "UK_Blair_II", c("PresDummy","value")]
tablePresUK <- ddply(tablePresUK,c("PresDummy"), function(x) data.frame(dissent = sum(x$value == 5)/nrow(x)))

##
library(Zelig)
library(mlogit)
###create deviance variable
deviance <- abs(data$left_right - data$meanLeftRight)
data$Deviance <- deviance

###create agriculture/regional variable
agrireg <- vector()
for (i in 1:nrow(data)){
  if (grepl("Agri|agri|Region|region",as.character(data$fields_of_activity[i]))){
    agrireg[i] <- 1
  } else {
    agrireg[i] <- 0
  }
}

data$agrireg <- agrireg

##correct the levels in the voting rule variable
levels(data$rule_eurlexprelex)[1] <- c("Other")
levels(data$rule_eurlexprelex)[2] <- c("Other")
levels(data$rule_eurlexprelex)[3] <- c("Other")
levels(data$rule_eurlexprelex)[4] <- c("Other")


data <- na.omit(data)

model1 <- zelig(as.factor(value) ~ Deviance + Votes + Deviance*Votes + netBen + MeetingFreq + PresDummy + agrireg + policyarea, data = data, model = "mlogit")

model2 <- zelig(as.factor(value) ~ Deviance + Votes + Deviance*Votes + netBen + MeetingFreq + PresDummy + agrireg + type_of_file +policyarea, data = data, model = "mlogit")

model3 <- zelig(as.factor(value) ~ Deviance + Votes + Deviance*Votes + netBen + MeetingFreq + PresDummy + agrireg + rule_eurlexprelex + policyarea, data = data, model = "mlogit")

model4 <- zelig(as.factor(value) ~ Deviance + Votes + Deviance*Votes + netBen + MeetingFreq + PresDummy + agrireg + bpoint + policyarea, data = data, model = "mlogit")

model5 <- zelig(as.factor(value) ~ Deviance + Votes + Deviance*Votes + netBen + MeetingFreq + PresDummy + agrireg + type_of_file + rule_eurlexprelex + bpoint + policyarea, data = data, model = "mlogit")


coefs <- summary(model5)@coef3[c(1:60,101:105),1]
std.err <- summary(model1)@coef3[1:75,2]
tvalue <- summary(model1)@coef3[1:75,3]

#cbind(coefs, tvalue)

mat <- matrix(NA,nc = 13, nr = 5)
countC <- 1
for (i in 1:13){
  for (j in 1:5){
    mat[j,i] <- tvalue[countC]
    countC <- countC + 1
  }
}

colnames(mat) <- c("Constant","Outlier","Power","Net Beneficiary","Meeting Frequency","Presidency","Agriculture/Regional","Directive","Regulation","QMV","Unanimity","Salience","Outlier * Power")

rownames(mat) <- c("1|2","1|3","1|4","1|5","1|6")

##PREDICTED PROBABILITIES FROM MODEL 5
#outlier
X <- cbind(1,seq(0,2.931,0.01),10,-0.709, 7, 0, 0, 1,0,1,0,1,0,0,0,0,0,0,0,0)
logit0 <- rep(0,294)
logit1 <- as.matrix(X) %*% as.vector(mat[1,])
logit2 <- as.matrix(X) %*% as.vector(mat[2,])
logit3 <- as.matrix(X) %*% as.vector(mat[3,])
logit4 <- as.matrix(X) %*% as.vector(mat[4,])
logit5 <- as.matrix(X) %*% as.vector(mat[5,])

logits <- cbind(logit0,logit1,logit2,logit3,logit4,logit5)
p.unscaled <- exp(logits)
p <- p.unscaled/rowSums(p.unscaled)

plot.new()
plot.window(xlim = c(0,2.932), ylim = c(0,1), xlab = "Outlier", ylab = "Probability")
axis(1)
axis(2)
for (i in 2:6){
  lines(seq(0,2.931,0.01), p[,i], col = i)
}

###zelig control
x.low <- setx(model5, Deviance = 0, votes = 2, policyarea = "Competitiveness (internal market, industry, research and space)", rule_eurlexprelex = "QMV", type_of_file = "Regulation")
x.high <- setx(model5, Deviance = 2, votes = 29, policyarea = "Competitiveness (internal market, industry, research and space)", rule_eurlexprelex = "QMV", type_of_file = "Regulation")
s.out <- sim(model1, x = x.low, x1 = x.high)

x.low <- setx(model5, netBen = -400, policyarea = "Agriculture and Fisheries", rule_eurlexprelex = "QMV", type_of_file = "Regulation")
x.high <- setx(model5, netBen = 400, policyarea = "Agriculture and Fisheries", rule_eurlexprelex = "QMV", type_of_file = "Regulation")
s.out <- sim(model1, x = x.low, x1 = x.high)

x.low <- setx(model5, netBen = -400, policyarea = "Employment, Social Policy, Health and Consumer Affairs", rule_eurlexprelex = "QMV", type_of_file = "Regulation")
x.high <- setx(model5, netBen = 400, policyarea = "Employment, Social Policy, Health and Consumer Affairs", rule_eurlexprelex = "QMV", type_of_file = "Regulation")
s.out <- sim(model1, x = x.low, x1 = x.high)

x.low <- setx(model5, Deviance = 0, policyarea = "Environment", rule_eurlexprelex = "QMV", type_of_file = "Regulation")
x.high <- setx(model5, Deviance = 2, policyarea = "Environment", rule_eurlexprelex = "QMV", type_of_file = "Regulation")
s.out <- sim(model1, x = x.low, x1 = x.high)

#Power
X <- cbind(1,1.175,2:29,-0.7093, 7, 0, 0, 1,0,1,0,1,0,0,0,0,0,0,0,0)
logit0 <- rep(0,28)
logit1 <- as.matrix(X) %*% as.vector(mat[1,])
logit2 <- as.matrix(X) %*% as.vector(mat[2,])
logit3 <- as.matrix(X) %*% as.vector(mat[3,])
logit4 <- as.matrix(X) %*% as.vector(mat[4,])
logit5 <- as.matrix(X) %*% as.vector(mat[5,])

logits <- cbind(logit0,logit1,logit2,logit3,logit4,logit5)
p.unscaled <- exp(logits)
p <- p.unscaled/rowSums(p.unscaled)

plot.new()
plot.window(xlim = c(2,29), ylim = c(0,1), xlab = "Power", ylab = "Probability")
axis(1)
axis(2)
for (i in 2:6){
  lines(2:29, p[,i], col = i)
}

#Net Beneficiary
X <- cbind(1,1.175,10,seq(-437,531,1), 7, 0, 0, 1,0,1,0,1,0,0,0,0,0,0,0,0)
logit0 <- rep(0,969)
logit1 <- as.matrix(X) %*% as.vector(mat[1,])
logit2 <- as.matrix(X) %*% as.vector(mat[2,])
logit3 <- as.matrix(X) %*% as.vector(mat[3,])
logit4 <- as.matrix(X) %*% as.vector(mat[4,])
logit5 <- as.matrix(X) %*% as.vector(mat[5,])

logits <- cbind(logit0,logit1,logit2,logit3,logit4,logit5)
p.unscaled <- exp(logits)
p <- p.unscaled/rowSums(p.unscaled)

plot.new()
plot.window(xlim = c(-437,531), ylim = c(0,1), xlab = "Net Transfers", ylab = "Probability")
axis(1)
axis(2)
for (i in 2:6){
  lines(seq(-437,531,1), p[,i], col = i)
}

#agri/regional
X <- cbind(1,1.175,10, -0.7093, 7, 0, 0:1, 1,0,1,0,1,0,0,0,0,0,0,0,0)
logit0 <- rep(0,2)
logit1 <- as.matrix(X) %*% as.vector(mat[1,])
logit2 <- as.matrix(X) %*% as.vector(mat[2,])
logit3 <- as.matrix(X) %*% as.vector(mat[3,])
logit4 <- as.matrix(X) %*% as.vector(mat[4,])
logit5 <- as.matrix(X) %*% as.vector(mat[5,])

logits <- cbind(logit0,logit1,logit2,logit3,logit4,logit5)
p.unscaled <- exp(logits)
p <- p.unscaled/rowSums(p.unscaled)

plot.new()
plot.window(xlim = , ylim = c(0,1), xlab = "agri/regional", ylab = "Probability")
axis(1)
axis(2)
for (i in 2:6){
  lines(seq(-437,531,1), p[,i], col = i)
}


