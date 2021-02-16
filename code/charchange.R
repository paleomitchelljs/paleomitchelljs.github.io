makeCharMat <- function(tree, phyDat) {
	require(phangorn)
	aceTree <- pace(tree, phyDat, "ACCTRAN", return="prob")

	# if phangorn does something weird, this will throw an error.
	charMat <- matrix(nrow=length(aceTree), ncol=ncol(testChar))
	for (i in 1:length(aceTree))	{
		# note that, as written, this only works on binary (0/1) characters. Modifying it isn't too hard, but would involve something other than just taking the 1 column from ace
		charMat[i,] <-  aceTree[[i]][,2]
	}
	rownames(charMat) <- 1:nrow(charMat)
	return(charMat)
}
#
plotCharChange <- function(tree, charMat, plottree=TRUE, Pch=25, Bg="lightblue", Rescale=FALSE, Rmin = 0.5, Mar=c(4,4,4,4))	{
	# tree is a phyogenetic tree w/ branch lengths
	# charMat is a matrix with character states for each tip AND each node. Must have rownames equalling the edge number for the phylo object of ape
	# plottree is a logical, set to TRUE to plot the tree
	# Pch and Bg both control the points that plot alongside the char change notations
	# Mar are the (bottom, left, top, right) margin values for R.
	# Rescale is a logical, asking if you want to make a tree with branches rescaled to the observed change numbers. Rmin is the min branch length for a rescaled tree
	#		for a lot of characters, you probably want to do x <- plotCharChange(tree,Data,Rescale=T) followed by plotCharChange(x, Data)

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
		Chars <- which(abs(changeVec) > 0)
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
					points(xloc_b+(j*xRange), yloc, pch=Pch, bg=Bg)
					text(xloc_b+(j*xRange), yloc, Reformat[j], pos=4, srt=270, offset=1, xpd=NA)
				}
				else	{
					points(xloc, yloc_b+(j*yRange), pch=Pch, bg=Bg)
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
library(phangorn)
library(phytools)
s <- "owls(((Strix_aluco:4.2,Asio_otus:4.2):3.1,Athene_noctua:7.3):6.3,Tyto_alba:13.5);"
testT <- read.tree(text=s)
testChar <- rbind(
			c(1,0,1,1,0), 
			c(1,1,0,1,0), 
			c(0,0,1,1,1), 
			c(1,1,0,1,0)
			)
rownames(testChar) <- testT$tip.label

# convert fake data to phyDat format for phangorn to use acctran
testPhy <- as.phyDat(testChar, type="USER", levels=c(0,1))

# phangorn is odd, so makeCharMat() will likely need heavy editting for different datasets
# but you do not need to use phangorn.
# Any data you can arrange into a matrix like charMat *will work* for plotCharChange. 
# charMat is a matrix where each row is an edge number, each column is a trait, and each cell is a reconstruction
# can be from acctran, or simmap, or ace, or whatever
# It (currently) has to be in standard R phylo object edge ordering. That is, the first 1 to Ntip taxa are tips, then the nodes follow after
Mat <- makeCharMat(testT, testPhy)


# make the plot
x <- plotCharChange(testT, Mat, Rescale=T)

# make a plot w/ branches scaled to num char changes
plotCharChange(x, Mat)


