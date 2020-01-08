calcChi <- function(x, expect=NULL)	{
	if (is.null(expect))	{
		expect <- rep(10, length(x))
	}
	sqErr <- (x - expect)^2
	sqErr <- sqErr / expect
	Chisq <- sum(sqErr)
	return(Chisq)
}
simDraws <- function(nruns, ncols=6, nstart=10, nrounds=3)	{
	Chiout <- c()
	for (j in 1:nruns)	{
		Start <- rep(1:ncols, nstart)
		Pop <- Start
		for (i in 1:nrounds)	{
			Draws <- sample(Pop, 20, replace = F)
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
