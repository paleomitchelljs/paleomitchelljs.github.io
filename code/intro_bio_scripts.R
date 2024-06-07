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
plot_regression <- function(predictor, response, predictor_label="", response_label="")	{
	par(las=1, mar=c(4,4,1,1), mgp=c(2.5,0.5,0), tck=-0.01)
	plot(x = predictor, y = response, pch=21, bg='gray70', xlab=predictor_label, ylab=response_label)
	Reg <- lm(response ~ predictor)
	CI <- confint(Reg)
	abline(Reg, col='blue', lty=1)
	abline(CI[,1], col='lightblue', lty=2)
	abline(CI[,2], col='lightblue', lty=2)

}

# Add additional functions for each lab. This should, ideally, be a one-stop-shop for O&E and CMB lab convenience functions

simulated_regression <- function(N = 100, slope = 1, intercept = 0, error = 0.1)	{
	x <- runif(N, min=0, max=5)
	y <- slope * x + intercept + rnorm(N, mean=0, sd=error)
	Model <- lm(y ~ x)
	plot(x, y, pch=16, col='lightgray')
	CI <- confint(Model)
	abline(Model, col='blue', lty=1)
	abline(CI[,1], col='lightblue', lty=2)
	abline(CI[,2], col='lightblue', lty=2)

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