######## generate individual
gen_ind <- function(alleles, freq = NULL, ploidy = 2)	{
	if (is.null(freq))	{
		freq <- rep(1/length(alleles), length(alleles))
	}
	if (length(freq) == 1 && length(alleles) == 2)	{
		freq <- c(freq, 1-freq)
	}
	if (length(alleles) != length(freq))	{
		error("need a frequency for each allele!")
	}
	genotype <- sort(sample(alleles, size = ploidy, prob = freq, replace = TRUE))
	return(genotype)
}

######## generate a population of individuals
gen_pop <- function(N, alleles = c("a", "b"), freqs = 0.7)	{
	Pop <- replicate(N, gen_ind(alleles, freqs))
	return(Pop)
}

######## mate two individuals
mating <- function(ind01, ind02)	{
	if (length(ind01) != length(ind02))	{
		error("different numbers of loci between individuals!")
	}
	kid <- sort(c(sample(ind01, 1), sample(ind02, 1)))
	return(kid)
}

######## advance a generation
adv_gen <- function(pop)	{
	old_gen <- pop
	new_gen <- matrix(NA, nrow = nrow(old_gen), ncol = ncol(old_gen))
	counter <- 1
	Stop <- FALSE
	while(Stop == FALSE)	{
		grab <- sample(1:ncol(old_gen), 2, replace = FALSE)
		new_gen[,counter] <- mating(old_gen[,grab[1]], old_gen[,grab[2]])
		counter <- counter + 1
		if (counter > ncol(new_gen))	{
			Stop <- TRUE
			break;
		}				
	}
	return(new_gen)
}

######## find end
find_end <- function(x)	{
	Fs <- x[,3:5]
	Maxs <- apply(Fs,1,max, na.rm = T)
	if (max(Maxs) > 0.9999)	{
		out <- which(Maxs > 0.9999)[1]
	}
	else if (max(Maxs) < 0.99999)	{
		out <- NA
	}
	return(out)
}
######## run two-allele pop through time
sim_pop <- function(time = 10, N = 36)	{
	output <- matrix(0, nrow = time, ncol = 5)
	colnames(output) <- c("a", "b", "aa", "ab", "bb")
	parents <- gen_pop(N)
	genos <- table(apply(parents, 2, paste, collapse = "")) / N
	p <- length(which(parents == "a"))
	q <- length(which(parents == "b"))
	output[1,c("a", "b", names(genos))] <- c(round(p/length(parents), digits = 2), round(q/length(parents), digits = 2), round(genos, 2))
	for (i in 2:time)	{
		kids <- adv_gen(parents)
		parents <- kids
		genos <- round(table(apply(kids, 2, paste, collapse = "")) / N, 2)
		output[i,c("a", "b", names(genos))] <- c(round(length(which(kids == "a"))/length(kids), digits = 2), round(length(which(kids == "b"))/length(kids), digits = 2), round(genos, 2))
	}
	return(output)
}

######## plot pop
plot_pop <- function(Pop, Col = "red", Col2 = "blue", Lwd = 2)	{
	par(mfrow=c(1, 2), las = 1, mar = c(4,4,1,1), tck = -0.01, mgp = c(2, 0.25, 0))
	plot(1:nrow(Pop), Pop[,"a"], lwd = Lwd, col = Col, type = "l", xlab = "time", ylab = "frequency of allele a")
	T_fix <- find_end(Pop)
	legend("right", legend = paste("gen.", T_fix), bty = "n")
	plot(1:nrow(Pop), abs(Pop[,"aa"] - Pop[,"a"]^2), xlab = "time", ylab = "|observed - HWE| aa", lwd = Lwd, col = Col2, type = "l")
}


#test <- sim_pop(time = 200, N = 40)
#plot_pop(test)
#z <-  c(68, 39, 59, 86, 124, 72, 44, 65, 46, 43, 54, 82, 95, 112, 40, 87, 78, 64, 95, 133, 68, 104, 146)
