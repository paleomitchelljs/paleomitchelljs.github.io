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

# Set up plots
Alpha <- 0.75
divCols <- rev(c(rgb(158/255,1/255,66/255, Alpha),rgb(213/255,62/255,79/255, Alpha),rgb(244/255,109/255,67/255, Alpha),rgb(253/255,174/255,97/255, Alpha),rgb(254/255,224/255,139/255, Alpha),rgb(255/255,255/255,191/255, Alpha),rgb(230/255,245/255,152/255, Alpha),rgb(171/255,221/255,164/255, Alpha),rgb(102/255,194/255,165/255, Alpha),rgb(50/255,136/255,189/255, Alpha),rgb(94/255,79/255,162/255, Alpha)))
qualCols <- c(rgb(166/255,206/255,227/255,Alpha),rgb(31/255,120/255,180/255,Alpha),rgb(178/255,223/255,138/255,Alpha),rgb(51/255,160/255,44/255,Alpha),rgb(251/255,154/255,153/255,Alpha),rgb(227/255,26/255,28/255,Alpha),rgb(253/255,191/255,111/255,Alpha),rgb(255/255,127/255,0,Alpha),rgb(202/255,178/255,214/255,Alpha))
Pal <- colorRampPalette(divCols, interpolate = "spline", alpha = T)