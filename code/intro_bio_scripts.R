####################################
### LAB 01 MEASUREMENT AND ERROR ###
####################################

### Provide students the basic summary statistics from a simple linear regression
fit_regression <- function(predictor, response, digits=3)	{
	Model <- lm(response ~ predictor)
	sumMod <- summary(Model)
	Pars <- sumMod$coef[,1:2]
	Rsq <- sumMod$r.squared
	Pval <- round(sumMod$coef[2,4], digits = 3)
	if (Pval < 0.001)	{
		Pval <- "<0.001"
	}
	out <- data.frame(
		Intercept = round(Pars[1,1], digits=3), 
		Intercept_StdError = round(Pars[1,2], digits=3), 
		Slope = round(Pars[2,1], digits=3), 
		Slope_StdError = round(Pars[2,2], digits=3), 
		P_value = Pval,
		R_squared = round(Rsq, digits=5)*100
	)
	return(out)
}

# Make a simple regression plot
plot_regression <- function(predictor, response, predictor_label="", response_label="", col1="blue", col2="lightblue")	{
	par(las=1, mar=c(4,4,1,1), mgp=c(2.5,0.5,0), tck=-0.01)
	plot(x = predictor, y = response, pch=21, bg='gray70', xlab=predictor_label, ylab=response_label)
	Reg <- lm(response ~ predictor)
	CI <- confint(Reg)
	abline(Reg, col=col1, lty=1)
	abline(CI[,1], col=col2, lty=2)
	abline(CI[,2], col=col2, lty=2)

}

# Add additional functions for each lab. This should, ideally, be a one-stop-shop for O&E and CMB lab convenience functions

simulated_regression <- function(N = 100, slope = 1, intercept = 0, error = 0.1, col1="blue", col2="lightblue")	{
	x <- runif(N, min=0, max=5)
	y <- slope * x + intercept + rnorm(N, mean=0, sd=error)
	Model <- lm(y ~ x)
	plot(x, y, pch=16, col='lightgray')
	CI <- confint(Model)
	abline(Model, col=col1, lty=1)
	abline(CI[,1], col=col2, lty=2)
	abline(CI[,2], col=col2, lty=2)

	sumMod <- summary(Model)
	Pars <- sumMod$coef[,1:2]
	Rsq <- sumMod$r.squared
	Pval <- round(sumMod$coef[2,4], digits = 3)
	if (Pval < 0.001)	{
		Pval <- "<0.001"
	}
	out <- data.frame(
		Intercept = round(Pars[1,1], digits=3), 
		Intercept_StdError = round(Pars[1,2], digits=3), 
		Slope = round(Pars[2,1], digits=3), 
		Slope_StdError = round(Pars[2,2], digits=3), 
		P_value = Pval,
		R_squared = round(Rsq, digits=5)*100
	)

	return(out)
}

####################################
##### LAB 03 GIVING UP DENSITY #####
####################################
# Add bell curve
addCurve <- function(Data)	{
	x2 <- seq(from=min(Data), to=max(Data), length.out=length(Data))
	fun <- dnorm(x2, mean(Data), sd=sd(Data))
	z <- hist(Data, plot=F)
	
	scale <- max(z$counts)/max(fun)
	lines(x2, fun*scale, col='red', lwd=2)
}
# normality QQ plot
qqPlot <- function(Data)	{
	qqnorm(Data, pch = 1)
	qqline(Data, col='steelblue', lwd=2)
}

# Test for normality
test_normal <- function(Data)	{
	Test <- shapiro.test(Data)
	Stat <- round(Test$statistic, digits=3)
	Pval <- round(Test$p.value, digits = 3)
	if (Pval < 0.001)	{
		Pval <- 0.001
	}
	return(cat("W =", Stat, " & p-value =", Pval))
}

# Boxplots
make_boxplot <- function(Data, variable="Year", addPts=T)	{
	par(mfrow=c(1,1), mar=c(4,3,1,1), mgp=c(2, 0.5, 0), tck=-0.01, las=1, cex.axis=0.9, cex.lab=1.2)
	z <- boxplot(Data[,"GUD"]~Data[,variable], boxwex=0.15, col='white', xlab=variable, ylab="giving up density")
	if (addPts)	{
		Choices <- setNames(seq(from=0, to=(length(z$names)-1), by=1)+0.85, z$names)
		points(Choices[as.character(Data[,variable])], Data[,"GUD"], pch=16, cex=0.75, col=rgb(0,0,0,0.3))
	}
}

# Statistcal test
kruskal_wallis <- function(Data, variable="Year")	{
	Test <- kruskal.test(Data$GUD ~ Data[,variable], data=Data)
	
}