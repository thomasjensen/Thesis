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

tableCab <- ddply(cabinets, c("cabms","newold"), function(x) data.frame(abs = sum(x$value == 2 | x$value == 4), no = sum(x$value == 1 | x$value == 3), yesNeg = sum(x$value == 5), rawDissent = sum(x$value != 0), time = unique(x$TimeInGovernment), LR = unique(x$left_right), meanlr = mean(x$meanLeftRight), deviance = abs(unique(x$left_right) - mean(x$meanLeftRight))^2))

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

##read libraries
library(Zelig)
library(mlogit)
###create deviance variable
deviance <- (data$left_right - data$meanLeftRight)^2
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

##recode the depedent variable to yes, no abstention yes + statement
data$value[data$value == 3] <- 1
data$value[data$value == 4] <- 2
data$value[data$value == 5] <- 3

##set no votes as the reference category
for (i in 1:length(data$value)){
  if (data$value[i] == 0){
    data$value[i] <- 1
  } else if (data$value[i] == 1){
    data$value[i] <- 0
  }
}

##new coding:
##0 = no
##1 = yes
##2 = abstention
##3 = yes + statement


#Analyis for employment and consumer affairs
data_sub <- subset(data, policyarea == "Employment/Consumer")
data_sub <- na.omit(data_sub)
data_sub$value

##Hausman test
mldata <- mlogit.data(data_sub,choice="value",shape="wide")
test <- mlogit(as.factor(value) ~ MeetingFreq, data =  data_sub, shape = "wide")

###with member state dummies
model1 <- zelig(as.factor(value) ~ Deviance + MeetingFreq + msabr, data = data_sub, model = "mlogit")

model2 <- zelig(as.factor(value) ~ Deviance + MeetingFreq + netBen + msabr, data = data, model = "mlogit")

model3 <- zelig(as.factor(value) ~ Deviance + MeetingFreq + netBen + PresDummy + msabr, data = data_sub, model = "mlogit")

model4 <- zelig(as.factor(value) ~ Deviance + MeetingFreq + netBen + PresDummy + msabr, data = data_sub, model = "mlogit")

model5 <- zelig(as.factor(value) ~ Deviance + MeetingFreq + PresDummy + bpoint + msabr, data = data_sub, model = "mlogit")

model6 <- zelig(as.factor(value) ~ Deviance + MeetingFreq + netBen + PresDummy + rule_eurlexprelex + type_of_file + msabr, data = data_sub, model = "mlogit")

out6 <- summary(model6)@coef3
out6.table <- matrix(nr = 70, nc = 3)
count1 <- 1
count2 <- 1
while (count2 != 70){
    out6.table[count2,1] <- out6[count1,1]
    out6.table[count2,2] <- out6[count1+1,1]
    out6.table[count2,3] <- out6[count1+2,1]
    out6.table[count2+1,1] <- out6[count1,2]
    out6.table[count2+1,2] <- out6[count1+1,2]
    out6.table[count2+1,3] <- out6[count1+2,2]
    count1 <- count1 + 3
    count2 <- count2 + 2
  }
names <- rownames(out6)
names <- gsub(":\\d{1}","",names, perl = TRUE)
names <- gsub("msabr","",names, perl = TRUE)
names <- gsub("rule_eurlexprelex","",names, perl = TRUE)
names <- gsub("type_of_file","",names, perl = TRUE)
names <- unique(names)

rownames(out6.table) <- c(rep("",70))
colnames(out6.table) <- c("Yes|No","Abstention|No","YesNeg|No")

count1 <- 1
count2 <- 1
while (count != 69){
  rownames(out6.table)[count1] <- names[count2]
  count1 <- count1 + 2
  count2 <- count2 + 1
}


###calculate marginal effects
###Deviance
### For different member states
beta <-cbind(matrix(coef(model6),nc = 3, byrow = TRUE),0)
memberstates <- list()
count <- 1
names <- c("AUT","BEL","BGR","CYP","CZE","DEU","DNK","ESP","EST","FIN","FRA","GBR","GRC","HUN","IRL","ITA","LTU","LUX","LVA","MLT","NLD","POL","PRT","ROU","SVK","SVN","SWE")

for (i in 9:35){
  if (i == 9){
    X <- cbind(1,seq(0,3,0.1),1,-.3,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  } else {
    X <- cbind(1,seq(0,3,0.1),1,-.3,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  X[,i] <- 1
  }
  P <- diag(as.vector(exp(X %*% beta) %*% as.matrix(rep(1,ncol(beta))))^-1) %*% exp(X %*% beta)
  memberstates[[names[count]]] <- P
  count <- count + 1
}

names <- c(rep("AUT",31),rep("BEL",31),rep("BGR",31),rep("CYP",31),rep("CZE",31),rep("DEU",31),rep("DNK",31),rep("ESP",31),rep("EST",31),rep("FIN",31),rep("FRA",31),rep("GBR",31),rep("GRC",31),rep("HUN",31),rep("IRL",31),rep("ITA",31),rep("LTU",31),rep("LUX",31),rep("LVA",31),rep("MLT",31),rep("NLD",31),rep("POL",31),rep("PRT",31),rep("ROU",31),rep("SVK",31),rep("SVN",31),rep("SWE",31))

x <- rep(seq(0,3,0.1),27)

memberstates <- as.data.frame(do.call(rbind,memberstates))
memberstates$ms <- names
memberstates$x <- x
colnames(memberstates) <- c("No", "Yes","Abstain","YesNeg","MS","X")

colors <-  palette(rainbow(27))
mykey <- list(space = "right",
              text = list(unique(names)),
              points = list(pch = 19, col = colors)
              )

tikz("effectDevEmplCons.tex", width = 7, height = 6)
xyplot(Yes + Abstain + YesNeg ~ X, groups = MS, data = memberstates, col = colors, type = "l", main = "", xlab = "Deviance", ylab = "Effect", key = mykey, layout = c(3,1))
dev.off()

##For new and old member states
beta <-cbind(matrix(coef(model7),nc = 3, byrow = TRUE),0)
newold <- list()
count <- 1
for (i in 1:2){
  if (i == 1){
    X <- cbind(1,seq(0,3,0.1),0,1,1,0,1,1,0)
  } else {
  X <- cbind(1,seq(0,3,0.1),0,1,1,0,1,1,1)
  }
  P <- diag(as.vector(exp(X %*% beta) %*% as.matrix(rep(1,ncol(beta))))^-1) %*% exp(X %*% beta)
  newold[[count]] <- P
  count <- count + 1
}

newold <- as.data.frame(do.call(rbind,newold))
newold$ms <- c(rep("old",31),rep("new",31))
x <- rep(seq(0,3,0.1),2)
newold$x <- x

colnames(newold) <- c("No", "Yes","Abstain","YesNeg","enlargement","X")

xyplot(Yes + Abstain + YesNeg ~ X, groups = enlargement, data = newold, col = colors, type = "l", main = "", xlab = "Deviance", ylab = "Effect", key = mykey, layout = c(3,1))


##Meeting Frequency
beta <-cbind(matrix(coef(model6),nc = 3, byrow = TRUE),0)
meetFreq <- list()
count <- 1
names <- c("AUT","BEL","BGR","CYP","CZE","DEU","DNK","ESP","EST","FIN","FRA","GBR","GRC","HUN","IRL","ITA","LTU","LUX","LVA","MLT","NLD","POL","PRT","ROU","SVK","SVN","SWE")

for (i in 9:35){
  if (i == 9){
    X <- cbind(1,1.9,1:10,-.3,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  } else {
    X <- cbind(1,1.9,1:10,-.3,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  X[,i] <- 1
  }
  P <- diag(as.vector(exp(X %*% beta) %*% as.matrix(rep(1,ncol(beta))))^-1) %*% exp(X %*% beta)
  meetFreq[[names[count]]] <- P
  count <- count + 1
}

x <- rep(seq(1:10),27)

meetFreq <- as.data.frame(do.call(rbind,meetFreq))
meetFreq$X <- x
names <- c(rep("AUT",10),rep("BEL",10),rep("BGR",10),rep("CYP",10),rep("CZE",10),rep("DEU",10),rep("DNK",10),rep("ESP",10),rep("EST",10),rep("FIN",10),rep("FRA",10),rep("GBR",10),rep("GRC",10),rep("HUN",10),rep("IRL",10),rep("ITA",10),rep("LTU",10),rep("LUX",10),rep("LVA",10),rep("MLT",10),rep("NLD",10),rep("POL",10),rep("PRT",10),rep("ROU",10),rep("SVK",10),rep("SVN",10),rep("SWE",10))
meetFreq$MS <- names
colnames(meetFreq) <- c("No", "Yes","Abstain","YesNeg","X","MS")

tikz("effectMeetEmplCons.tex", width = 7, height = 6)
xyplot(Yes + Abstain + YesNeg ~ X, groups = MS, data = meetFreq, col = colors, type = "l", main = "", xlab = "MeetingFrequency", ylab = "Effect", key = mykey, layout = c(3,1))
dev.off()


#Analyis for agriculture and fisheries
data_sub <- subset(data, policyarea == "Agri/Pech")
data_sub <- na.omit(data_sub)

###with member state dummies
model1 <- zelig(as.factor(value) ~ Deviance + MeetingFreq + msabr, data = data_sub, model = "mlogit")

model2 <- zelig(as.factor(value) ~ Deviance + MeetingFreq + netBen + msabr, data = data_sub, model = "mlogit")

model3 <- zelig(as.factor(value) ~ Deviance + MeetingFreq + netBen + PresDummy + msabr, data = data_sub, model = "mlogit")

model4 <- zelig(as.factor(value) ~ Deviance + MeetingFreq + netBen + PresDummy + msabr, data = data_sub, model = "mlogit")

model5 <- zelig(as.factor(value) ~ Deviance + MeetingFreq + PresDummy + bpoint + msabr, data = data_sub, model = "mlogit")

model6 <- zelig(as.factor(value) ~ Deviance + MeetingFreq + netBen + PresDummy + rule_eurlexprelex + type_of_file + msabr, data = data_sub, model = "mlogit")


###calculate marginal effects
###Deviance
### For different member states
beta <-cbind(matrix(coef(model6),nc = 3, byrow = TRUE),0)
memberstates <- list()
count <- 1
names <- c("AUT","BEL","BGR","CYP","CZE","DEU","DNK","ESP","EST","FIN","FRA","GBR","GRC","HUN","IRL","ITA","LTU","LUX","LVA","MLT","NLD","POL","PRT","ROU","SVK","SVN","SWE")

for (i in 9:35){
  if (i == 9){
    X <- cbind(1,seq(0,3,0.1),1,-.3,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  } else {
    X <- cbind(1,seq(0,3,0.1),1,-.3,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  X[,i] <- 1
  }
  P <- diag(as.vector(exp(X %*% beta) %*% as.matrix(rep(1,ncol(beta))))^-1) %*% exp(X %*% beta)
  memberstates[[names[count]]] <- P
  count <- count + 1
}

names <- c(rep("AUT",31),rep("BEL",31),rep("BGR",31),rep("CYP",31),rep("CZE",31),rep("DEU",31),rep("DNK",31),rep("ESP",31),rep("EST",31),rep("FIN",31),rep("FRA",31),rep("GBR",31),rep("GRC",31),rep("HUN",31),rep("IRL",31),rep("ITA",31),rep("LTU",31),rep("LUX",31),rep("LVA",31),rep("MLT",31),rep("NLD",31),rep("POL",31),rep("PRT",31),rep("ROU",31),rep("SVK",31),rep("SVN",31),rep("SWE",31))

x <- rep(seq(0,3,0.1),27)

memberstates <- as.data.frame(do.call(rbind,memberstates))
memberstates$ms <- names
memberstates$x <- x
colnames(memberstates) <- c("No", "Yes","Abstain","YesNeg","MS","X")

colors <-  palette(rainbow(27))
mykey <- list(space = "right",
              text = list(unique(names)),
              points = list(pch = 19, col = colors)
              )

tikz("effectDevAgriPech.tex", width = 7, height = 6)
xyplot(Yes + Abstain + YesNeg ~ X, groups = MS, data = memberstates, col = colors, type = "l", main = "", xlab = "Deviance", ylab = "Effect", key = mykey, layout = c(3,1))
dev.off()


##Meeting Frequency
beta <-cbind(matrix(coef(model6),nc = 3, byrow = TRUE),0)
meetFreq <- list()
count <- 1
names <- c("AUT","BEL","BGR","CYP","CZE","DEU","DNK","ESP","EST","FIN","FRA","GBR","GRC","HUN","IRL","ITA","LTU","LUX","LVA","MLT","NLD","POL","PRT","ROU","SVK","SVN","SWE")

for (i in 9:35){
  if (i == 9){
    X <- cbind(1,1,1:10,-.3,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  } else {
    X <- cbind(1,1,1:10,-.3,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  X[,i] <- 1
  }
  P <- diag(as.vector(exp(X %*% beta) %*% as.matrix(rep(1,ncol(beta))))^-1) %*% exp(X %*% beta)
  meetFreq[[names[count]]] <- P
  count <- count + 1
}

x <- rep(seq(1:10),27)

meetFreq <- as.data.frame(do.call(rbind,meetFreq))
meetFreq$X <- x
names <- c(rep("AUT",10),rep("BEL",10),rep("BGR",10),rep("CYP",10),rep("CZE",10),rep("DEU",10),rep("DNK",10),rep("ESP",10),rep("EST",10),rep("FIN",10),rep("FRA",10),rep("GBR",10),rep("GRC",10),rep("HUN",10),rep("IRL",10),rep("ITA",10),rep("LTU",10),rep("LUX",10),rep("LVA",10),rep("MLT",10),rep("NLD",10),rep("POL",10),rep("PRT",10),rep("ROU",10),rep("SVK",10),rep("SVN",10),rep("SWE",10))
meetFreq$MS <- names
colnames(meetFreq) <- c("No", "Yes","Abstain","YesNeg","X","MS")

tikz("effectMeetAgriPech.tex", width = 7, height = 6)
xyplot(Yes + Abstain + YesNeg ~ X, groups = MS, data = meetFreq, col = colors, type = "l", main = "", xlab = "MeetingFrequency", ylab = "Effect", key = mykey, layout = c(3,1))
dev.off()
 
#Analyis for competition 
data_sub <- subset(data, policyarea == "Comp.")
data_sub <- na.omit(data_sub)

###with member state dummies
model1 <- zelig(as.factor(value) ~ Deviance + MeetingFreq + msabr, data = data_sub, model = "mlogit")

model2 <- zelig(as.factor(value) ~ Deviance + MeetingFreq + netBen + msabr, data = data_sub, model = "mlogit")

model3 <- zelig(as.factor(value) ~ Deviance + MeetingFreq + netBen + PresDummy + msabr, data = data_sub, model = "mlogit")

model4 <- zelig(as.factor(value) ~ Deviance + MeetingFreq + netBen + PresDummy + msabr, data = data_sub, model = "mlogit")

model5 <- zelig(as.factor(value) ~ Deviance + MeetingFreq + PresDummy + bpoint + msabr, data = data_sub, model = "mlogit")

model6 <- zelig(as.factor(value) ~ Deviance + MeetingFreq + netBen + PresDummy + rule_eurlexprelex + type_of_file + msabr, data = data_sub, model = "mlogit")



###calculate marginal effects
###Deviance
### For different member states
beta <-cbind(matrix(coef(model6),nc = 3, byrow = TRUE),0)
memberstates <- list()
count <- 1
names <- c("AUT","BEL","BGR","CYP","CZE","DEU","DNK","ESP","EST","FIN","FRA","GBR","GRC","HUN","IRL","ITA","LTU","LUX","LVA","MLT","NLD","POL","PRT","ROU","SVK","SVN","SWE")

for (i in 9:35){
  if (i == 9){
    X <- cbind(1,seq(0,3,0.1),1,-.3,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  } else {
  X <- cbind(1,seq(0,3,0.1),1,-.3,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  X[,i] <- 1
  }
  P <- diag(as.vector(exp(X %*% beta) %*% as.matrix(rep(1,ncol(beta))))^-1) %*% exp(X %*% beta)
  memberstates[[names[count]]] <- P
  count <- count + 1
}

names <- c(rep("AUT",31),rep("BEL",31),rep("BGR",31),rep("CYP",31),rep("CZE",31),rep("DEU",31),rep("DNK",31),rep("ESP",31),rep("EST",31),rep("FIN",31),rep("FRA",31),rep("GBR",31),rep("GRC",31),rep("HUN",31),rep("IRL",31),rep("ITA",31),rep("LTU",31),rep("LUX",31),rep("LVA",31),rep("MLT",31),rep("NLD",31),rep("POL",31),rep("PRT",31),rep("ROU",31),rep("SVK",31),rep("SVN",31),rep("SWE",31))

x <- rep(seq(0,3,0.1),27)

memberstates <- as.data.frame(do.call(rbind,memberstates))
memberstates$ms <- names
memberstates$x <- x
colnames(memberstates) <- c("No", "Yes","Abstain","YesNeg","MS","X")

colors <-  palette(rainbow(27))
mykey <- list(space = "right",
              text = list(unique(names)),
              points = list(pch = 19, col = colors)
              )

tikz("effectDevComp.tex", width = 7, height = 6)
xyplot(Yes + Abstain + YesNeg ~ X, groups = MS, data = memberstates, col = colors, type = "l", main = "", xlab = "Deviance", ylab = "Effect", key = mykey, layout = c(3,1))
dev.off()


##Meeting Frequency
beta <-cbind(matrix(coef(model6),nc = 3, byrow = TRUE),0)
meetFreq <- list()
count <- 1
names <- c("AUT","BEL","BGR","CYP","CZE","DEU","DNK","ESP","EST","FIN","FRA","GBR","GRC","HUN","IRL","ITA","LTU","LUX","LVA","MLT","NLD","POL","PRT","ROU","SVK","SVN","SWE")

for (i in 9:35){
  if (i == 9){
    X <- cbind(1,1.9,1:10,-.3,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  } else {
    X <- cbind(1,1.9,1:10,-.3,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  X[,i] <- 1
  }
  P <- diag(as.vector(exp(X %*% beta) %*% as.matrix(rep(1,ncol(beta))))^-1) %*% exp(X %*% beta)
  meetFreq[[names[count]]] <- P
  count <- count + 1
}

x <- rep(seq(1:10),27)

meetFreq <- as.data.frame(do.call(rbind,meetFreq))
meetFreq$X <- x
names <- c(rep("AUT",10),rep("BEL",10),rep("BGR",10),rep("CYP",10),rep("CZE",10),rep("DEU",10),rep("DNK",10),rep("ESP",10),rep("EST",10),rep("FIN",10),rep("FRA",10),rep("GBR",10),rep("GRC",10),rep("HUN",10),rep("IRL",10),rep("ITA",10),rep("LTU",10),rep("LUX",10),rep("LVA",10),rep("MLT",10),rep("NLD",10),rep("POL",10),rep("PRT",10),rep("ROU",10),rep("SVK",10),rep("SVN",10),rep("SWE",10))
meetFreq$MS <- names
colnames(meetFreq) <- c("No", "Yes","Abstain","YesNeg","X","MS")

tikz("effectMeetComp.tex", width = 7, height = 6)
xyplot(Yes + Abstain + YesNeg ~ X, groups = MS, data = meetFreq, col = colors, type = "l", main = "", xlab = "MeetingFrequency", ylab = "Effect", key = mykey, layout = c(3,1))
dev.off()
 






#cbind(coefs, tvalue)
