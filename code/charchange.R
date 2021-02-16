library(phytools)
library(phangorn)


# make fake data
s <- "owls(((Strix_aluco:4.2,Asio_otus:4.2):3.1,Athene_noctua:7.3):6.3,Tyto_alba:13.5);"
testT <- read.tree(text=s)
testChar <- rbind(
			c(1,0,1,1,0), 
			c(1,1,0,1,0), 
			c(0,0,1,1,1), 
			c(1,1,0,1,0)
			)
rownames(testChar) <- testT$tip.label

# convert fake data to phyDat format for phangorn
testPhy <- as.phyDat(testChar, type="USER", levels=c(0,1))

# scale tree by characer change number. We'll use this later for plotting
rescaled <- acctran(testT, testPhy)

# use acctran to get ancestral states
aceTree <- pace(testT, testPhy, "ACCTRAN", return="prob")

##############################
#      Make the plot!        #
##############################
# Any data you can arrange into this type of object will plot. 
# changeMat is a matrix where each row is an edge number, each column is a trait, and each cell is a reconstruction
# can be from acctran, or simmap, or ace, or whatever
changeMat <- matrix(nrow=length(aceTree), ncol=ncol(testChar))
for (i in 1:length(aceTree))	{
	changeMat[i,] <-  aceTree[[i]][,2]
}
rownames(changeMat) <- 1:nrow(changeMat)

# extract edge matarix
eMat <- testT$edge

# get characters of ancestor and descendent of each edge
Ancest <- changeMat[as.character(eMat[,1]),]
Descend <- changeMat[as.character(eMat[,2]),]

# make the phylo
par(mar=c(4,4,4,4))
plot(testT, direction="up")

# extract some plotting parameters
#plot(rescaled)
lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)

# find which characters change on each edge
for (i in 1:nrow(eMat))	{
	# find which characters changed (which ancestor states != descendent states for each edge)
	changeVec <- Ancest[i,] - Descend[i,]
	Chars <- which(abs(changeVec) > 0)
	# if any change happened..
	if (length(Chars) > 0)	{
		Reformat <- sapply(Chars, function(x) paste(x, ": ", Ancest[i,x], "->", Descend[i,x], sep=""))
#		edgelabels(i, bg=NULL, frame="none", text=paste(Reformat, collapse="\n"), pos=1)
#		edgelabels(edge=i, bg=NULL, frame="none", pch=16)

		if (lastPP$direction %in% c("rightwards", "leftwards"))	{
			xloc_b <- lastPP$xx[rescaled$edge[i,1]]
			xloc_e <- lastPP$xx[i]
			xRange <- ( xloc_e - xloc_b ) / (length(Chars) + 2)
			yloc <- lastPP$yy[i]
		}
		else	{
			yloc_b <- lastPP$yy[rescaled$edge[i,1]]
			yloc_e <- lastPP$yy[i]
			yRange <- ( yloc_e - yloc_b ) / (length(Chars) + 2)
			xloc <- lastPP$xx[i]

		}
		# add the change on the tree on the right branch
		for (j in 1:length(Chars))	{
			if (lastPP$direction %in% c("rightwards", "leftwards"))	{
				points(xloc_b+(j*xRange), yloc, pch=25, bg="lightblue")
				text(xloc_b+(j*xRange), yloc, Reformat[j], pos=4, srt=270, offset=1, xpd=NA)
			}
			else	{
				points(xloc, yloc_b+(j*yRange), pch=25, bg="lightblue")
				text(xloc, yloc_b+(j*yRange), Reformat[j], pos=4, xpd=NA)				
			}		
		}
	}

}

