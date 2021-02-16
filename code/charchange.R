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

# convert to a change matrix
changeMat <- matrix(nrow=length(aceTree), ncol=ncol(testChar))
for (i in 1:length(aceTree))	{
	changeMat[i,] <-  aceTree[[i]][,2]
}
rownames(changeMat) <- 1:nrow(changeMat)

# extract edge matarix
eMat <- rescaled$edge

# get characters of ancestor and descendent of each edge
Ancest <- changeMat[as.character(eMat[,1]),]
Descend <- changeMat[as.character(eMat[,2]),]

# make the phylo
plot(testT)

# extract some plotting parameters
#plot(rescaled)
#obj <- get("last_plot.phylo", envir = .PlotPhyloEnv)
#Heights <- nodeHeights(rescaled)

plot(testT)
edgelabels(bg=NULL, frame="none", pch=16)
# find which characters change on each edge
for (i in 1:nrow(eMat))	{
	# find which characters changed (which ancestor states != descendent states for each edge)
	changeVec <- Ancest[i,] - Descend[i,]
	Chars <- which(abs(changeVec) > 0)
	# if any change happened..
	if (length(Chars) > 0)	{
		Reformat <- sapply(1:length(Chars), function(x) paste(Chars[x], ": ", Ancest[i,x], "->", Descend[i,x], sep=""))
		edgelabels(i, bg=NULL, frame="none", text=paste(Reformat, collapse="\n"), pos=1)

		# couldn't get the below to work, but it's the general idea. Can look up old code I wrote to stick labels on edges in specific places later if you can't get a satisfactory look
#		XX <- (obj$xx[rescaled$edge[i,2]] - obj$xx[rescaled$edge[i,1]]) / length(Chars)
		# add the change on the tree on the right branch
#		for (j in 1:length(Chars))	{
#			cat(obj$xx[rescaled$edge[i,1]] - Heights[i,1])
			# need to add something here for branches with multiple changes, not sure what
#			points(obj$xx[rescaled$edge[i,1]], obj$yy[rescaled$edge[i,2]], pch=16)
#			text(obj$xx[rescaled$edge[i,1]], obj$yy[rescaled$edge[i,2]], pos = 3,
#				paste(Chars[j],": ", Ancest[i,Chars[j]], "->", Descend[i,Chars[j]], sep="")
#				)
#		}
	}

}

