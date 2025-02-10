#### BIO 145: Beans lab
# fake data for first figure in beans lab
#x <- rnorm(1e2)
#y1 <- rnorm(1e2)
#y2 <- x + runif(1e2, min = -1.5, max = 1.5)
#y3 <- -1 * x + runif(1e2)

#par(las = 1, mgp = c(2, 0.5, 0), tck = -0.01, mfrow=c(1, 3))
#plot(x, y1, xlab = "variable 1", ylab = "variable 2", pch = 21, bg = 'red', main = "Fig 1. No correlation")
#plot(x, y2, xlab = "variable 1", ylab = "variable 2", pch = 21, bg = 'red', main = "Fig 2. Weak positive correlation")
#plot(x, y3, xlab = "variable 1", ylab = "variable 2", pch = 21, bg = 'red', main = "Fig 3. Strong negative correlation")


### generate fake data to test
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
add_regression <- function(Model, location = "topleft", y_variable = "y", x_variable = "x", show_equation = TRUE, show_line = TRUE, linetype = 1, linecolor = 'red')	{
	code <- setNames(c(1,2,3), c("solid", "dashed", "dotted"))
	if (class(Model) != "lm")	{
		cat("You need to use the function lm() to fit a model and store that model as an object. Then give this function that object. Example:", fill = T)
		cat(	"fit <- lm( y ~ x )", fill = T)
		cat("add_equation( fit )", fill = T)
		cat("try again!")
	}
	else if (class(Model) == "lm")	{
		sumreg <- summary(Model)
		Values <- round(sumreg$coefficients, digits = 3)
		if (show_equation)	{
			legend(location, bty="n", legend = paste(y_variable, " = ", Values[1,1], " + ", Values[2,1], " * ", x_variable))
		}
		if (show_line)	{
			abline(Model, col = linecolor, lty = code[linetype])
		}
		pval <- Values[2,4]
		if (pval < 0.001)	{
			pval <- 0.001
		}
		#cat(expression("R"^2~"="~round(sumreg$r.squared, digits = 3)~" & p-value < "~pval))	
		cat("R2 = ", round(sumreg$r.squared, digits = 3), " & p-value <= ", pval)
	}
}

beanplot <- function(x, y, xlab = "length", ylab = "mass", show_equation = FALSE, pch = pch)	{
#	set_up_plot()
	plot(x, y, pch = pch, col = 'black', cex = 1.1, xlab = xlab, ylab = ylab)
	regression <- lm(y ~ x)
	sumreg <- summary(regression)

	Values <- round(sumreg$coefficients, digits = 3)
	if (show_equation)	{
		legend("topleft", bty="n", legend = paste("mass = ", Values[1,1], " + ", Values[2,1], " * length"))
	}
	pval <- Values[2,4]
	if (pval < 0.001)	{
		pval <- 0.001
	}
	cat("R2 = ", round(sumreg$r.squared, digits = 3), " | p-value < ", pval)	
}

## standard curve
plot_curve <- function(x, y, xlab="", ylab="")	{
	plot(x, y, pch=16, col='gray70', xlab=xlab, ylab=ylab)
	Mod <- lm(y ~ 0 + x)
	abline(Mod, col='red', lty = 2)
	Rsq <- round(summary(Mod)$r.sq, digits = 2)
	Color <- "black"
	if (Rsq < 0.99)	{
		Color <- "red"
	}
	legend("topleft", legend=paste("R-sq = ", Rsq, sep=""), text.col = Color, bty = "n")
}

## back-calculate function
convert <- function(x, model)	{
	(x - model$coefficients[1]) / model$coefficients[2]
#	model$coefficients[1] + ( model$coefficients[2] * x )
}

## regression curves
## multiple lines on one plot
#set_up_plot()
#plot(1, 1, type="n", xlab = "time (sec)", ylab="Tetra-guaiacol concentration (mg/mL)", xlim=c(0, 210), ylim=c(0, max(dat[,4])))
#points(y = dat[,2], x = dat[,1], pch = 21, bg = "red")
#points(y = dat[,3], x = dat[,1], pch = 22, bg = "orange")
#points(y = dat[,4], x = dat[,1], pch = 23, bg = "darkred")

#add_regression(lm(dat[,2]~dat[,1]), show_equation = F, linecolor = "blue")
#add_regression(lm(dat[,3]~dat[,1]), show_equation = F, linecolor = "skyblue")
#add_regression(lm(dat[,4]~dat[,1]), show_equation = F, linecolor = "blue")

