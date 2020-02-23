# have this all be on the website & loaded
sampleFreq <- read.csv("http://jonsmitchell.com/data/scrubjayFreq.csv", stringsAsFactors = F)
overallFreq <- sampleFreq[which(sampleFreq$Category == "xt"),]
overallN <- sampleFreq[which(sampleFreq$Category == "nt"),]
nestlingsFreq <- sampleFreq[which(sampleFreq$Category == "xb"),]
nestlingsN <- sampleFreq[which(sampleFreq$Category == "nb"),]
survivorFreq <- sampleFreq[which(sampleFreq$Category == "xs"),]
survivorN <- sampleFreq[which(sampleFreq$Category == "ns"),]
immFreq <- sampleFreq[which(sampleFreq$Category == "xi"),]
immN <- sampleFreq[which(sampleFreq$Category == "ni"),]
dSurvive <- sampleFreq[which(sampleFreq$Category == "xs-xt"),]
dBirths <- sampleFreq[which(sampleFreq$Category == "xb-xt"),]
dImm <- sampleFreq[which(sampleFreq$Category == "xi-xt"),]
dOver <- sampleFreq[which(sampleFreq$Category == "xt1-xt"),]

Ne_total <- 1/mean(1/overallN[,3])

alleleVec <- c()
scale_alleleVec <- c()
dImmVec <- rep(0, ncol(overallFreq)-2)
dBirthVec <- rep(0, ncol(overallFreq)-2)
dSurvVec <- rep(0, ncol(overallFreq)-2)
dOverVec <- rep(0, ncol(overallFreq)-2)

nImmVec <- rep(0, ncol(overallFreq)-2)
nBirthVec <- rep(0, ncol(overallFreq)-2)
nSurvVec <- rep(0, ncol(overallFreq)-2)
nOverVec <- c()

yearVec <- c()
locus <- c()
for (i in 3:ncol(overallFreq))	{
	alleleVec <- c(alleleVec, overallFreq[,i])
	scale_alleleVec <- c(scale_alleleVec, overallFreq[,i]-overallFreq[1,i])

	dOverVec <- c(dOverVec, dOver[,i])
	dSurvVec <- c(dSurvVec, dSurvive[,i])
	dBirthVec <- c(dBirthVec, dBirths[,i])
	dImmVec <- c(dImmVec, dImm[,i])

	nOverVec <- c(nOverVec, overallN[,i])
	nSurvVec <- c(nSurvVec, survivorN[,i])
	nBirthVec <- c(nBirthVec, nestlingsN[,i])
	nImmVec <- c(nImmVec, immN[,i])

	yearVec <- c(yearVec, overallFreq[,1])
	locus <- c(locus, rep(i-2, nrow(overallFreq)))
}

alleleFreqs <- data.frame(year=yearVec, locus=locus, freq=alleleVec, rfreq=scale_alleleVec, d_freq = dOverVec, d_imm = dImmVec, d_surv = dSurvVec, d_birth = dBirthVec, n_freq = nOverVec, n_imm = nImmVec, n_surv = nSurvVec, n_birth = nBirthVec)
sizeChanges <- data.frame(year = overallN[,1], n_total = overallN[,3], n_births = c(NA,nestlingsN[,3]), n_survive = c(NA,survivorN[,3]), n_immigrants = c(NA,immN[,3]))

