#######################################
####  BIOL 112 STICK ANALYSIS FXN  ####
#######################################
calcChi <- function(x, expect=NULL)	{
	if (is.null(expect))	{
		expect <- rep(10, length(x))
	}
	sqErr <- (x - expect)^2
	sqErr <- sqErr / expect
	Chisq <- sum(sqErr)
	return(Chisq)
}
simDraws <- function(nruns, ncols=6, nstart=10, nrounds=3, w=NULL)	{
	Chiout <- c()
	for (j in 1:nruns)	{
		Start <- rep(1:ncols, nstart)
		Pop <- Start
		for (i in 1:nrounds)	{
			if (is.null(w))	{
				Draws <- sample(Pop, 20, replace = F)
			}
			else if (!is.null(w))	{
				if (length(setdiff(unique(Pop), names(w))) == 0)	{
					Draws <- sample(Pop, 20, replace=F, prob=w[Pop])
				}
				else if (length(setdiff(unique(Pop), names(w))) != 0)	{
					cat("Not enough fitness values! ", setdiff(unique(Pop), names(w)))
				}
			}
			Pop <- sort(c(Draws,Draws,Draws))
		}
		Summary <- c()
		for (k in 1:ncols)	{
			Summary[k] <- length(which(Pop == k))
		}

		Chiout[j] <- sum(((Summary - nstart)^2) / nstart)
	}
	return(Chiout)
}
addHist <- function(Y, Dat, Color, Conf = 0.5, LWD=2, jiggle = 0.1, Yexp = 0.75, addPts = FALSE)	{
	Den <- density(Dat)
	Ydelt <- Yexp / max(Den$y) 
	polygon(Den$x, (Den$y * Ydelt) + Y, col = Color, border = 'black')

	if (addPts)	{
		Quant <- (quantile(Dat, prob = c(0.5 - (Conf / 2), 0.5 + (Conf / 2)))) / 3
		segments(Quant[1], Y, Quant[2], Y, lwd=LWD)
		segments(Quant[1], Y-jiggle, Quant[1], Y+jiggle, lwd=LWD)
		segments(Quant[2], Y-jiggle, Quant[2], Y+jiggle, lwd=LWD)
		points(mean(Dat), Y, pch=21, bg=Color)
	}	
}
plotChis <- function( counts , Ylims = c(0, 60) )	{
	par(mfrow=c(4,1))
	barplot(as.numeric(counts[sample(which(Chisqs == max(Chisqs)))[1],]), names.arg=colnames(counts), col=colnames(counts), main=bquote(chi^2==.(max(Chisqs))), ylim=Ylims)
	abline(h=10, lty=2, col='gray70')
	barplot(as.numeric(counts[sample(which(Chisqs == median(Chisqs)))[1],]), names.arg=colnames(counts), col=colnames(counts), main=bquote(chi^2==.(median(Chisqs))), ylim=Ylims)
	abline(h=10, lty=2, col='gray70')
	barplot(as.numeric(counts[sample(which(Chisqs == min(Chisqs)))[1],]), names.arg=colnames(counts), col=colnames(counts), main=bquote(chi^2==.(min(Chisqs))), ylim=Ylims)
	abline(h=10, lty=2, col='gray70')
	barplot(rep(10, ncol(counts)), names.arg=colnames(counts), col=colnames(counts), main=bquote(chi^2==0), ylim=Ylims)
	abline(h=10, lty=2, col='gray70')
}

######
#######################################
####  POPULATION SIM ANALYSIS FXN  ####
#######################################

#### Fxn for multi-locus IBD tracker
makeFounder <- function(Name, len = 1e4, uni = T)	{
	z <- sapply(1:len, function(x) paste(Name, x, sep="-"))
	return(cbind(z,z))
}
makeGamete <- function( individual, rate = 1.6 )	{
	Ncuts <- rgeom(1, prob = 1 / rate) + 1
	Stop <- nrow(individual) - 1
	Spots <- sort(sample(1:Stop, Ncuts))

	for ( i in Spots )	{
		individual <- rbind(individual[1 : i, c(1,2)], individual[(i + 1) : nrow(individual), c(2,1)])
	}

	Gam <- individual[,sample(c(1,2),1)]
	return(Gam)
}
makeBaby <- function(mother, father)	{
	egg <- makeGamete( mother )
	sperm <- makeGamete( father )
	zygote <- cbind(egg, sperm)
	return(zygote)
}


#### Fxn for 2-allele pop sim
# pass Nalleles and mu to mating() to drive new alleles. Have it return h & s if new mutant
mating <- function(Mother, Father, mu = 0, twoway = TRUE)	{
	# produce a maternal gamete
	Gam1 <- sample(Mother, size = 1)
	Gam2 <- sample(Father, size = 1)

	if (mu > 0)	{
	# check to see if it's going to mutate. 
		if ( runif(1) < mu )	{
			if (Gam1 == "a")	{
				Gam1 <- "b"
			}
			if (Gam1 == "b" && twoway == T)	{
				Gam1 <- "a"
			}
		}
		if ( runif(1) < mu )	{
			if (Gam2 == "a")	{
				Gam2 <- "b"
			}
			if (Gam2 == "b" && twoway == T)	{
				Gam2 <- "a"
			}

		}
	}
	Offspring <- sort(c(Gam1, Gam2))
	return(Offspring)
}

# intP needs to be a vector of proportions
makePop <- function(Popsize, intP = 0.5)	{
	Individuals <- t(replicate(Popsize, sort(sample(c("a","b"), size = 2, replace = T, prob = c( intP, 1 - intP )))))
	return(Individuals)
}

checkFitness <- function(Geno, w)	{
	Genot <- paste(Geno[1], Geno[2], sep="")
	Fit <- w[Genot]
	if (min(Fit) < 0)	{
		Fit[which(Fit < 0)] <- 0
	}
	return(Fit)
}

# make infinite alleles model?
# have output be a list with:
#	[[1]] a dataframe with nallele columns and ngen rows
#	[[2]] a matrix of h & s values for alleles
simPop <- function(Popsize=100, nGenerations=100, h=1, s=0, initial_p=0.5, mu = 0, twoway = TRUE, w = NULL)	{
	if (is.null(w))	{
		# make fitness vector using h & s values
		w <- c(aa = 1, ab = 1 - h * s, bb = 1 - s)
	}
	else if (!is.null(w))	{
		if (length(w) != 3)	{
			stop(cat("Not enough fitness values supplied. Set w to NULL or provide 3 fitnesses"))
		}
		if (is.null(names(w)))	{
			names(w) <- c("aa", "ab", "bb")
			cat("Setting names of w to aa, ab, and bb")
		}
	}
	# initialize the simulation with a starting population
	All_Individuals <- makePop(Popsize, intP = initial_p)

	# set up storage objects for the simulation output
	freqa <- c()
	freqa[1] <- sum(All_Individuals == "a") / length(All_Individuals)

	freqb <- c()
	freqb[1] <- sum(All_Individuals == "b") / length(All_Individuals)
	
	Gens <- apply(All_Individuals, 1, function(x) paste(sort(x), collapse=""))
	Genotypes <- matrix(0, nrow=nGenerations, ncol=3)
	colnames(Genotypes) <- names(w)
	GenCounts <- tapply(Gens, Gens, length)
	Genotypes[1,names(GenCounts)] <- GenCounts
	
	for (generation in 2:nGenerations)	{
		# Last generation's kids are this generation's parents!
		# What are their fitnesses?
		Fitnesses <- apply(All_Individuals, 1, checkFitness, w=w)

		# Half of the kids are female...
		Women <- sample(1:Popsize, size=(Popsize / 2))
		Mothers <- All_Individuals[Women,]

		# ...the other half are male
		Men <- setdiff(1:Popsize, Women)
		Fathers <- All_Individuals[Men,]

		# Now we do what we did before!
		nIndividuals <- Popsize
		for (child in 1:nIndividuals)	{
			# When using sample(), you can actually SET the probability that
			#	a given individual is sampled. 
			father <- sample(1:nrow(Fathers), size=1, prob=Fitnesses[Men])
			mother <- sample(1:nrow(Mothers), size=1, prob=Fitnesses[Women])

			Child <- mating(Mothers[mother,], Fathers[father,], mu = mu, twoway = twoway)

			cGenotype <- paste(Child[1], Child[2], sep="")
			Genotypes[generation, cGenotype] <- Genotypes[generation, cGenotype] + 1
			All_Individuals[child,] <- Child
		}

		nA_Individuals <- length(which(All_Individuals == "a"))
		freqa[generation] <- nA_Individuals / length(All_Individuals)

		na_Individuals <- length(which(All_Individuals == "b"))
		freqb[generation] <- na_Individuals / length(All_Individuals)

		if (max(c(freqa[generation],freqb[generation])) == 1 && mu == 0)	{
			Diff <- nGenerations - generation
			freqa <- c(freqa, rep(freqa[generation], Diff))
			freqb <- c(freqb, rep(freqb[generation], Diff))
			Genotypes <- rbind(Genotypes, matrix(Genotypes[generation,], ncol=ncol(Genotypes), nrow=Diff, byrow= TRUE))
			break;
		}
	}
	output <- data.frame(freqa=freqa, freqb=freqb, Genotypes=Genotypes[1:length(freqa),])
	return(output)
}

# palette
#divCols <- c(rgb(158/255,1/255,66/255, Alpha),rgb(213/255,62/255,79/255, Alpha),rgb(244/255,109/255,67/255, Alpha),rgb(253/255,174/255,97/255, Alpha),rgb(254/255,224/255,139/255, Alpha),rgb(255/255,255/255,191/255, Alpha),rgb(230/255,245/255,152/255, Alpha),rgb(171/255,221/255,164/255, Alpha),rgb(102/255,194/255,165/255, Alpha),rgb(50/255,136/255,189/255, Alpha),rgb(94/255,79/255,162/255, Alpha))
#qualCols <- c(rgb(166/255,206/255,227/255,Alpha),rgb(31/255,120/255,180/255,Alpha),rgb(178/255,223/255,138/255,Alpha),rgb(51/255,160/255,44/255,Alpha),rgb(251/255,154/255,153/255,Alpha),rgb(227/255,26/255,28/255,Alpha),rgb(253/255,191/255,111/255,Alpha),rgb(255/255,127/255,0,Alpha),rgb(202/255,178/255,214/255,Alpha))

plotFit <- function(nruns, n = 100, ngens = 100, init_p = 0.5, h = 1, s = 0, mu = 1e-6, cpt=1.2, cLwd=2, Alpha = 1)	{
	Cols <- c(rgb(166/255,206/255,227/255,Alpha),rgb(31/255,120/255,180/255,Alpha),rgb(178/255,223/255,138/255,Alpha),rgb(51/255,160/255,44/255,Alpha),rgb(251/255,154/255,153/255,Alpha),rgb(227/255,26/255,28/255,Alpha),rgb(253/255,191/255,111/255,Alpha),rgb(255/255,127/255,0,Alpha),rgb(202/255,178/255,214/255,Alpha))
	Pal <- colorRampPalette(Cols, interpolate = "spline", alpha = T)
	simCol <- Pal(nruns)

# fitness surface
	Fits <- c(1, 1 - h * s, 1 - s)
	par(mfrow=c(1, 2), mar=c(4,5,1,1), las = 1, mgp=c(2, 0.25, 0), tck = -0.01, cex.axis = 0.95, cex.lab = 1.1 )
	plot(1, 1, type="n", xlab="genotype", xaxt="n", ylab="fitness", ylim=c(min(Fits), max(Fits)), xlim=c(0,4))
	axis(1, at=c(1, 2, 3), labels=c("aa", "ab", "bb"))
	points(c(1, 2, 3), Fits, type="b", pch=16, cex=cpt, lwd=cLwd)

# simulation runs
	plot(1, 1, type = "n", xlab = "time (generation)", ylab= "freq. of a", xlim=c(0, ngens), ylim=c(0, 1))
	for (i in 1:nruns)	{
		Pops <- simPop( n, ngens, h = h, s = s, initial_p = init_p, mu = mu)
		lines(1:nrow(Pops), Pops[,1], lwd=cLwd, col=simCol[i])
	}	

}

addFit <- function(nruns, n = 100, ngens = 100, init_p = 0.5, h = 1, s = 0, mu = 1e-6, cpt=1.2, cLwd=2, Alpha = 1)	{
	Cols <- c(rgb(166/255,206/255,227/255,Alpha),rgb(31/255,120/255,180/255,Alpha),rgb(178/255,223/255,138/255,Alpha),rgb(51/255,160/255,44/255,Alpha),rgb(251/255,154/255,153/255,Alpha),rgb(227/255,26/255,28/255,Alpha),rgb(253/255,191/255,111/255,Alpha),rgb(255/255,127/255,0,Alpha),rgb(202/255,178/255,214/255,Alpha))
	Pal <- colorRampPalette(Cols, interpolate = "spline", alpha = T)
	simCol <- Pal(nruns)

# simulation runs
	for (i in 1:nruns)	{
		Pops <- simPop( n, ngens, h = h, s = s, initial_p = init_p, mu = mu)
		lines(1:nrow(Pops), Pops[,1], lwd=cLwd, col=simCol[i])
	}	

}
