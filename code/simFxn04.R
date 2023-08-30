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
			break;
		}
	}
	output <- data.frame(freqa=freqa, freqb=freqb, Genotypes=Genotypes[1:length(freqa),])
	return(output)
}