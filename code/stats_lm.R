plotCurves <- function(x, y, Scale=0.03, exFac=0.1)	{
	require(vioplot)
	FullRange <- range(c(x,y))
	Expand <- diff(FullRange) * exFac
	plot(x, y, xlim=c(FullRange[1]-Expand, FullRange[2]+Expand), ylim=c(FullRange[1]-Expand, FullRange[2]+Expand), las=1, mgp=c(2, 0.5, 0), pch=16, cex=1.1)
	Model <- lm(y~x)
	Pred <- Model$coef[1] + Model$coef[2] * x
	SD <- sd(Model$resid)
	
	for (i in 1:length(Pred))	{
		vioplot(x=rnorm(1e3, mean=Pred[i], sd=SD), at=x[i], col=rgb(0,0,0,0.1), add=T, drawRect=F, plotCentre=F, wex=Scale)
	}
	abline(Model, col='red',lty=3)
	points(x, Pred, pch=1, cex=0.6)
}