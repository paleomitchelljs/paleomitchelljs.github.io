genInd <- function(nLoci, Mean=0, Var=2)	{
	Ind <- rbind(rnorm(nLoci, Mean, Var), rnorm(nLoci, Mean, Var))
	return(Ind)
}
genKid <- function(x1, x2)	{
	Choose1 <- sample(c(1,2), ncol(x1), replace=T)
	Gam1 <- sapply(1:ncol(x1), function(x) x1[Choose1[x],x])

	Choose2 <- sample(c(1,2), ncol(x2), replace=T)
	Gam2 <- sapply(1:ncol(x2), function(x) x2[Choose2[x],x])

	x3 <- rbind(Gam1, Gam2)
	return(x3)
}

# a 1/length(x) chance of mating
# sd of mating = relative fitness. Higher sd = higher variance = stronger selection
getProb <- function(x, wSD=0.05)	{
	if (sd(x) > 0)	{
		Zscores <- ( x - mean(x) ) / sd(x)
		Probs <- (1 / length(x)) + (Zscores * wSD)
		Probs[which(Probs < 0)] <- 0
	}
	else{
		Probs <- rep(1/length(x), length(x))
	}
	return(Probs)
}
# This will run a model that assumes a hermaphroditic population (think plants) that cannot self-fertilize. 
# The population is of fixed size with non-overlapping generations. Eg, it always has exactly PopSize number of individuals.
# Each individual is chosen at random to mate. The probably an individual is chosen is a function of it's size relative to the other members of the population. 
# Beta = 0 (no selection) means that each individual has a 1/PopSize chance to breed
# Beta = 1 means that 
runAddSim <- function(PopSize, Va=5, nLoci=5, nSims=5, nGens=50, Err=0.001, S=0.005, Cols=c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00"), Cexs=c(1.5, 1.4, 1.3, 1.2, 1.1), showMax=F, TopLim=125, Bottom=-10, output=F, setPar=T, plotAlleles=F, plotGen=T)	{
	# store it all 
	Z <- list()
	nAlleles <- matrix(NA, ncol=nGens, nrow=nSims)

	# start the population
	Pop_init <- replicate(PopSize, genInd(nLoci, Var=Va), simplify=FALSE)
	nKids <- PopSize
	
	# set up a plot	
	x <- unlist(Pop_init)
	if (showMax)	{
		TopLim <- sum(x[which(x>0)])
		Bottom <- sum(x[which(x<0)])
	}

	if (setPar)	{
		par(las=1, mgp=c(2, 0.5, 0), tck=-0.01)
	}
	if (plotGen)	{
		plot(1,1, xlim=c(0, nGens), ylim=c(Bottom, TopLim), type="n", xlab="generation", ylab="avg value")
	}
	if (showMax)	{
		abline(h=TopLim, lty=3)
	}
	# for the k'th simulation... 
	for (k in 1:nSims){
		# set up a storage matrix...
		X <- matrix(nrow=nGens, ncol=nKids)
		nA <- c()
		# start with the same individuals...
		Pop <- Pop_init
		X[1,] <- sapply(Pop, sum)
		nA[1] <- length(unique(unlist(Pop)))
		# then for each generation...
		for (i in 2:nGens)	{
			# find each individual's size
			Sizes <- sapply(Pop, sum)
			# expected number of kids is 2 modified by Beta * Z-score of size
			Prob <- getProb(Sizes, wSD=S) + Err
			# make a storage list
			newPop <- list()
			# then for each kid...
			for (j in 1:nKids)	{
				# determine the parents..
				Parents <- sample(1:length(Pop), 2, prob=Prob, replace=F)
				# pass on alleles...
				newPop[[j]] <- genKid(Pop[[Parents[1]]], Pop[[Parents[2]]])	
			}
			# replace the old generation with the new one
			Pop <- newPop
			# store the new population's sizes
			X[i,] <- sapply(Pop, sum)
			nA[i] <- length(unique(unlist(Pop)))
		}
		# plot the change in mean size over all th generations
		if (plotGen)	{
			points(1:nGens, apply(X,1,mean), type="b", col=Cols[k], pch=16, cex=Cexs[k], lwd=Cexs[k])
		}
		Z[[k]] <- X
		nAlleles[k,] <- nA
	}
	if (plotAlleles)	{
		plot(1,1, xlim=c(0, nGens), ylim=c(0, log(2*nLoci*PopSize)), type="n", xlab="generation", ylab="log number of unique alleles")
		for (sim in 1:nSims)	{
			points(1:nGens, log(nAlleles[sim,]), col=Cols[sim], lwd=Cexs[sim], cex=Cexs[sim], pch=16)
		}
	}
	if (output)	{
		out <- list(Z, nAlleles)
		return(out)
	}	
}

calcDelta <- function(x)	{
	Means <- apply(x,1,mean)
	SDs <- apply(x, 1, sd)
	Deltas <- c(diff(Means),0)
	return(cbind(SDs,Deltas))
}
plotDelta <- function(x)	{
	Out <- x[[1]]
	Ds <- lapply(Out, calcDelta)
	Range <- range(unlist(as.vector(Ds)))
	par(las=1, mgp=c(2, 0.5, 0), tck=-0.01)
	plot(1, 1, type="n", ylim=Range, xlim=Range, xlab="mean difference between parents and offspring", ylab="variance of parents")
	silent <- sapply(Ds, function(x) lines(x[,1], x[,2], lwd=1.2, col=rgb(0,0,0,1/length(Ds))))
	abline(0,1,lty=3, col='red')
#	legend("bottomright", legend="line of unity", lty=3, col='red', bty="n")
}

