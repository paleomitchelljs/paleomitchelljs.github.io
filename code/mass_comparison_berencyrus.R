
# 
setwd("C:\\Users\\jonsm\\Dropbox\\Professional\\paleomitchelljs.github.io\\data\\")
#setwd("~/Dropbox/Professional/paleomitchelljs.github.io/data")
Beren <- read.csv("beren.csv")
dayID <- apply(Beren, 1, function(x) paste(x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format="%Y-%m-%d", origin = "2019-04-18")
Beren$age <- dateID - dateID[which(Beren$event == "birth")]

Cyrus <- read.csv("cyrus.csv")
dayID <- apply(Cyrus, 1, function(x) paste(x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format="%Y-%m-%d", origin = "2022-04-12")
Cyrus$age <- dateID - dateID[which(Cyrus$event == "birth")]



bMass <- Beren[grep("mass", Beren$event),c("value","age")]
bMass <- bMass[order(bMass$age),]
cMass <- Cyrus[grep("mass", Cyrus$event),c("value","age")]
cMass <- cMass[order(cMass$age),]

who <- read.csv("who_median.csv", header=F)

pdf("mass_growth.pdf", height=5, width=5)
par(las=1, mar=c(4,4,1,1), mgp=c(2.5, 0.5, 0), tck=-0.01)
plot(bMass$age, bMass$value, type="b", pch=16, col="#f1a340", xlab="age (in days)", ylab="mass (in kg)")
lines(cMass$age, cMass$value/1000, type="b", pch=15, col="#998ec3")
lines(who[,1], who[,2], type="l", col='black', lty=3, lwd=2)
dev.off()


pdf("mass_growth_log.pdf", height=5, width=5)
par(las=1, mar=c(4,4,1,1), mgp=c(2.5, 0.5, 0), tck=-0.01)
plot(bMass$age, bMass$value, type="b", pch=16, col="#f1a340", xlab="age (in days)", ylab="mass (in kg)", log="y")
lines(cMass$age, cMass$value/1000, type="b", pch=15, col="#998ec3")
dev.off()


pdf("mass_growth_log2.pdf", height=5, width=5)
par(las=1, mar=c(4,4,1,1), mgp=c(2.5, 0.5, 0), tck=-0.01)
plot(bMass$age, log(bMass$value), type="b", pch=16, col="#f1a340", xlab="age (in days)", ylab="log mass (in kg)", log="y")
lines(cMass$age, log(cMass$value/1000), type="b", pch=15, col="#998ec3")
dev.off()

lbAge <- bMass$age
lbAge[which(lbAge == 0)] <- lbAge[which(lbAge == 0)] + 1
lbAge <- log(lbAge)

lcAge <- cMass$age
lcAge[which(lcAge == 0)] <- lcAge[which(lcAge == 0)] + 1
lcAge <- log(lcAge)

pdf("mass_growth_log3.pdf", height=5, width=5)
par(las=1, mar=c(4,4,1,1), mgp=c(2.5, 0.5, 0), tck=-0.01)
plot(lbAge, log(bMass$value), type="b", pch=16, col="#f1a340", xlab="age (in log days)", ylab="mass (in log kg)", log="y")
lines(lcAge, log(cMass$value/1000), type="b", pch=15, col="#998ec3")
dev.off()

hist(x, col='black', border='white', xlab='offspring masses', main='')
