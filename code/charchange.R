### currently cannot handle ambiguity

makeCharMat <- function(tree, traitMatrix, ambig="?") {
	require(phangorn)
	require(paleotree)

	# traitMatrix needs to have rownames == tiplabels of tree
	traitMatrix <- traitMatrix[tree$tip.label,]

	# make a storage matrix
	charMat <- matrix(0, nrow=Ntip(tree)*2 - 1, ncol=ncol(traitMatrix))
	for (i in 1:ncol(traitMatrix))	{
		# make a trait vector
		Trait <- traitMatrix[,i]
		names(Trait) <- rownames(traitMatrix)

		# use paleotree to find states
		aTrait <- ancPropStateMat(Trait, tree, type="ACCTRAN")

		# ambiguously reconstructed traits are currently randomly sampled.
		charMat[,i] <-  apply(aTrait, 1, function(x) ifelse(min(x)==0, which(x == 0), sample(x,1,prob=x)))
	}
	rownames(charMat) <- rownames(aTrait)
	return(charMat)
}
#
plotCharChange <- function(tree, charMat, plottree=TRUE, Pch=c(24, 25), Bg=c("blue", "red"), Rescale=FALSE, Rmin = 0.5, Mar=c(4,4,4,4), Threshold = T)	{
	# tree is a phyogenetic tree w/ branch lengths
	# charMat is a matrix with character states for each tip AND each node. Must have rownames equalling the edge number for the phylo object of ape
	# plottree is a logical, set to TRUE to plot the tree
	# Pch and Bg both control the points that plot alongside the char change notations
	#	Pch and Bg can be set to the same value (eg., Pch=c(25,25), Bg=c("lightblue","lightblue")) to map all transitions identically. Currently the only "custom" points this allows are "up" and "down" transitions, although it's R. Anything is possible!
	# Mar are the (bottom, left, top, right) margin values for R.
	# Rescale is a logical, asking if you want to make a tree with branches rescaled to the observed change numbers. Rmin is the min branch length for a rescaled tree
	#		for a lot of characters, you probably want to do x <- plotCharChange(tree,Data,Rescale=T) followed by plotCharChange(x, Data)
	# Threshold is a limit to prevent the reconstruction of ambiguous ransitions. If set to false, transitions from "0.5 -> X" will be mapped. If set T, only confidently reconstructed transitions will be mapped. Neither is optimal, and many better solutions are obvious, but this is what I've done for now.
	require(ape)
	# extract edge matarix
	eMat <- tree$edge

	# get characters of ancestor and descendent of each edge
	Ancest <- charMat[as.character(eMat[,1]),]
	Descend <- charMat[as.character(eMat[,2]),]

	# if you want to scale edges by character change, set Rescale is true and this function will save a new tree rescaled that you can then rerun the function on to plot
	if (Rescale)	{
		reTreeE <- rep(1, length(tree$edge.length))
	}

	# make the phylo
	if (plottree)	{
		par(mar=Mar)
		plot(tree, direction="up")
	}

	# extract some plotting parameters
	lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)

	# find which characters change on each edge
	for (i in 1:nrow(eMat))	{
		# find which characters changed (which ancestor states != descendent states for each edge)
		changeVec <- Ancest[i,] - Descend[i,]
		if (Threshold == TRUE)	{
			changeVec <- sapply(changeVec, floor)
		}
		Chars <- which(abs(changeVec) > 0)
		CharVals <- as.numeric(changeVec[Chars] > 0)+1
		# if any change happened..
		if (length(Chars) > 0)	{
			Reformat <- sapply(Chars, function(x) paste(x, ": ", Ancest[i,x], "->", Descend[i,x], sep=""))
			if (Rescale)	{
				reTreeE[i] <- length(Chars)
			}
			if (lastPP$direction %in% c("rightwards", "leftwards"))	{
				xloc_b <- lastPP$xx[eMat[i,1]]
				xloc_e <- lastPP$xx[eMat[i,2]]
				xRange <- ( xloc_e - xloc_b ) / (length(Chars) + 2)
				yloc <- lastPP$yy[eMat[i,2]]
			}
			else	{
				yloc_b <- lastPP$yy[eMat[i,1]]
				yloc_e <- lastPP$yy[eMat[i,2]]
				yRange <- ( yloc_e - yloc_b ) / (length(Chars) + 2)
				xloc <- lastPP$xx[eMat[i,2]]
			}
			# add the change on the tree on the right branch
			for (j in 1:length(Chars))	{
				if (lastPP$direction %in% c("rightwards", "leftwards"))	{
					points(xloc_b+(j*xRange), yloc, pch=Pch[CharVals[j]], bg=Bg[CharVals[j]])
					text(xloc_b+(j*xRange), yloc, Reformat[j], pos=4, srt=270, offset=1, xpd=NA)
				}
				else	{
					points(xloc, yloc_b+(j*yRange), pch=Pch[CharVals[j]], bg=Bg[CharVals[j]])
					text(xloc, yloc_b+(j*yRange), Reformat[j], pos=4, xpd=NA)				
				}		
			}
		}
	}
	if (Rescale)	{
		reTree <- tree
		reTree$edge.length <- reTreeE
		return(reTree)
	}
}


##############################
#      Make the plot!        #
##############################
# make fake data
library(phytools)
library(paleotree)

# make a fake tree
simphy <- pbtree(n=10)

# make some fake traits
fakeTraits <- matrix(0, nrow=Ntip(simphy), ncol=10)
for (i in 1:ncol(fakeTraits))	{
	traits <- sim.history(simphy, Q=matrix(rep(1/5,25), ncol=5, nrow=5))
	exTraits <- as.numeric(sapply(traits$maps, function(x) names(x)[which(x == max(x))])[1:Ntip(simphy)])
	renum <- seq(0, length(unique(exTraits)), 1)
	names(renum) <- unique(exTraits)
	fakeTraits[,i] <- renum[as.character(exTraits)]
}
rownames(fakeTraits) <- simphy$tip.label

# Any data you can arrange into a matrix like charMat *will work* for plotCharChange.
# charMat is a matrix where each row is an edge number, each column is a trait, and each cell is a reconstruction
# can be from acctran, or simmap, or ace, or whatever
# It (currently) has to be in standard R phylo object edge ordering. That is, the first 1 to Ntip taxa are tips, then the nodes follow after.
Mat <- makeCharMat(simphy, fakeTraits)

# make the plot
x <- plotCharChange(simphy, Mat, Rescale=T)

# make a plot w/ branches scaled to num char changes
plotCharChange(x, Mat)


