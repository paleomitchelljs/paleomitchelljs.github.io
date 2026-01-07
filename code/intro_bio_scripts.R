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
	return(cat("Test statistic =", Stat, " & p-value =", Pval))
}

# Boxplots
make_boxplot <- function(Data, variable="Day", addPts=T)	{
	par(mfrow=c(1,1), mar=c(4,3,1,1), mgp=c(2, 0.5, 0), tck=-0.01, las=1, cex.axis=0.9, cex.lab=1.2)
	z <- boxplot(Data[,"GUD"]~Data[,variable], boxwex=0.15, col='white', xlab=variable, ylab="giving up density")
	if (addPts)	{
		Choices <- setNames(seq(from=0, to=(length(z$names)-1), by=1)+0.85, z$names)
		points(Choices[as.character(Data[,variable])], Data[,"GUD"], pch=16, cex=0.75, col=rgb(0,0,0,0.3))
	}
}

# Statistcal test
test_difference <- function(Data, variable="Day")	{
	Test <- kruskal.test(Data$GUD ~ Data[,variable], data=Data)
	Stat <- round(Test$statistic, digits=3)
	Pval <- round(Test$p.value, digits = 3)
	if (Pval < 0.001)	{
		Pval <- 0.001
	}
	return(cat("Test statistic =", Stat, " & p-value =", Pval))
	
}

####################################
###### t test ######
####################################

make_boxplot <- function(x, variable, ylabel="", xlabel="")	{
	par(mar=c(3,4,1,1), las=1, mgp=c(2.5, 0.25, 0), tck=-0.01)
	boxplot(x ~ variable, boxwex=0.15, ylab=ylabel, xlab=xlabel, col='white')
}

run_tTest <- function(x, variable)	{
	z <- t.test(x ~ variable)
	if (z$p.value < 0.001)	{
		cat("t-Test results", "\n", names(z$estimate)[1], z$estimate[1], "\n", names(z$estimate)[2], z$estimate[2], "\n", "degrees of freedom =", z$parameter, "\n", "scaled difference between group means (t) =", z$statistic, "\n", "p-value < 0.001", "\n")		
	}
	else		{
		cat("t-Test results", "\n", names(z$estimate)[1], z$estimate[1], "\n", names(z$estimate)[2], z$estimate[2], "\n", "degrees of freedom =", z$parameter, "\n", "scaled difference between group means (t) = ", z$statistic, "\n", "p-value = ", z$p.value, "\n")
	}
}

run_tTest_paired <- function(x, variable)	{
	z <- t.test(x[which(variable==unique(variable)[1])], x[which(variable==unique(variable)[2])], paired=T)
	if (z$p.value < 0.001)	{
		cat("t-Test results", "\n", names(z$estimate)[1], z$estimate[1], "\n", names(z$estimate)[2], z$estimate[2], "\n", "degrees of freedom =", z$parameter, "\n", "scaled difference between group means (t) =", z$statistic, "\n", "p-value < 0.001", "\n")		
	}
	else		{
		cat("t-Test results", "\n", names(z$estimate)[1], z$estimate[1], "\n", names(z$estimate)[2], z$estimate[2], "\n", "degrees of freedom =", z$parameter, "\n", "scaled difference between group means (t) = ", z$statistic, "\n", "p-value = ", z$p.value, "\n")
	}
}


####################################
###### LAB 05 SLIME MOLD PREF ######
####################################
compare_rates <- function(control, treatment, Ylim = NULL, Side = "topleft", NA_handle = F)	{
	if (NA_handle)	{
	# treats NAs very conservatively as only being slightly later than observed pair
#		control[is.na(control)] <- treatment[is.na(control)] + 1
#		treatment[is.na(treatment)] <- control[is.na(treatment)] + 1
	# treats NAs less conservatively as being closer to the full time
	control[is.na(control)] <- 13
	treatment[is.na(treatment)] <- 13
	}
	overall <- rpois(1e5, mean(c(control,treatment))) - rpois(1e5, mean(c(control,treatment)))
	probs <- table(overall)/length(overall)
	par(las = 1, mar = c(4,5,1,1), mgp = c(1.5, 0.5, 0), tck = -0.01, mfrow = c(1, 2))
	boxplot(control, treatment, names = c("control", "treatment"), col = 'white', boxwex = 0.2, ylim = c(0, max(c(control, treatment))))
	text(c(0.65, 0.85, 1.75), c(0, 0, 0), c("N = ", length(na.omit(control)), length(na.omit(treatment))))
	if (!is.null(Ylim))	{
		hist(control - treatment, freq = F, main = "", xlab = "difference in speed (ctrl - trt)", ylab = "prob.", ylim = Ylim, border = 'white', col = 'gray70')
	}
	else {
		hist(control - treatment, freq = F, main = "", xlab = "difference in speed (ctrl - trt)", ylab = "prob.", border = 'white', col = 'gray70')		
	}
	lines(as.numeric(names(probs)),  probs, col = 'red', lwd = 1.25)
	test <- wilcox.test(control, treatment)
	Stat <- paste(names(test$statistic), test$statistic, sep = " = ")
	if (test$p.value < 0.001)	{
		test$p.value <- 0.001
	}
	Pval <- paste("p-value = ", round(test$p.value, digits = 3))
	legend(Side, legend = c("Wilcoxon test", Stat, Pval), bty = "n")
}