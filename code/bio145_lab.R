#### BIO 145: Beans lab

### generate fake data
#len <- rnorm(20, mean = 10, sd = 1)
#mass <- 3 * len + runif(20)
#dat <- data.frame(len = len, mass = mass)

### bean regression
#par(las = 1, mar = c(4,4,1,1), tck = -0.01, mgp = c(2.5, 0.5, 0))
#plot(dat$len, dat$mass, pch = 16, col = 'black', cex = 1.1, xlab = "length (mm)", ylab = "mass (g)")
#regression <- lm(dat$mass ~ dat$len)
#summary(regression)
#Values <- round(regression$coefficients, digits = 2)
#legend("topleft", bty="n", legend = paste("mass = ", regression$coefficients[1], " + ", regression$coefficients[2], " * length"))

set_up_plot <- function(bottommargin = 4, leftmargin = 4, topmargin = 1, rightmargin = 1, ticklength = 0.01, labeldistance = 2.5, numberdistance = 0.5)	{
	par(las = 1, mar = c(bottommargin, leftmargin, topmargin, rightmargin), tck = -1 * ticklength, mgp = c(labeldistance, numberdistance, 0))
}
beanplot <- function(x, y, xlab = "length", ylab = "mass")	{
	set_up_plot()
	plot(x, y, pch = 16, col = 'black', cex = 1.1, xlab = "length (mm)", ylab = "mass (g)")
	regression <- lm(y ~ x)
	sumreg <- summary(regression)

	Values <- round(sumreg$coefficients, digits = 3)
	legend("topleft", bty="n", legend = paste("mass = ", Values[1,1], " + ", Values[2,1], " * length"))
	pval <- Values[2,4]
	if (pval < 0.001)	{
		pval <- 0.001
	}
	cat("R2 = ", round(sumreg$r.squared, digits = 3), " | p-value < ", pval)	
}


#source("https://jonsmitchell.com/code/bio145_lab.R")
#dat <- read.csv('beans.csv')
#set_up_plot()
#beanplot(dat$len, dat$mass)