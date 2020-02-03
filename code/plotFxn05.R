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
plotChis <- function( counts )	{
	par(mfrow=c(4,1))
	barplot(as.numeric(counts[sample(which(Chisqs == max(Chisqs)))[1],]), names.arg=colnames(counts), col=colnames(counts), main=bquote(chi^2==.(max(Chisqs))))
	abline(h=10, lty=2, col='gray70')
	barplot(as.numeric(counts[sample(which(Chisqs == median(Chisqs)))[1],]), names.arg=colnames(counts), col=colnames(counts), main=bquote(chi^2==.(median(Chisqs))))
	abline(h=10, lty=2, col='gray70')
	barplot(as.numeric(counts[sample(which(Chisqs == min(Chisqs)))[1],]), names.arg=colnames(counts), col=colnames(counts), main=bquote(chi^2==.(min(Chisqs))))
	abline(h=10, lty=2, col='gray70')
	barplot(rep(10, ncol(counts)), names.arg=colnames(counts), col=colnames(counts), main=bquote(chi^2==0))
	abline(h=10, lty=2, col='gray70')
}