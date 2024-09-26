#setwd("~/Dropbox/Professional/Courses/BIO155_O&E/lab/lab05")
#dat <- read.csv("roach_smr.csv")
#head(dat)
# Calculate SMR / mass
#dat$smr_by_mass <- dat$smr / dat$mass
# Get means
# body mass
#mean(dat$mass_mg)
# standard metabolic rate
#mean(dat$smr)
# smr / mass
#mean(dat$smr_bymass)
# Get means by species
# body mass
#tapply(dat$mass_mg, dat$species, mean)
# standard metabolic rate
#tapply(dat$smr, dat$species, mean)
# smr / mass
#tapply(dat$smr, dat$species, mean)
# Get standard deviations
# body mass
#sd(dat$mass_mg)
# standard metabolic rate
#sd(dat$smr)
# smr / mass
#sd(dat$smr_bymass)
# Get standard deviations by species
# body mass
#tapply(dat$mass_mg, dat$species, sd)
# standard metabolic rate
#tapply(dat$smr, dat$species, sd)
# smr / mass
#tapply(dat$smr, dat$species, sd)

make_boxplot <- function(x, variable, ylabel="", xlabel="")	{
	par(mar=c(3,4,1,1), las=1, mgp=c(2, 0.25, 0), tck=-0.01)
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

